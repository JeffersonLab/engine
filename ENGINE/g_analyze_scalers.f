      subroutine g_analyze_scalers(event,ABORT,err)
*
* $Log$
* Revision 1.15.18.1  2008/09/25 00:55:27  jones
* Updated for running on Fedora 8 with gfortran
*
* Revision 1.16  2008/09/25 00:06:33  jones
* Updated to run with gfortran compiler
*
* Revision 1.15.20.1  2007/09/10 20:33:37  pcarter
* Implemented changes to allow compilation on RHEL 3,4,5 and MacOSX
*
* Revision 1.15  2003/09/05 20:54:41  jones
* Merge in online03 changes (mkj)
*
* Revision 1.14.2.2  2003/08/14 00:40:09  cdaq
* Modify so "beam on" scalers for both bcm1 and bcm2 (mkj)
*
* Revision 1.14.2.1  2003/04/11 13:25:11  cdaq
* Remove old hardwire check on run number
*
* Revision 1.14  1999/11/04 20:35:14  saw
* Linux/G77 compatibility fixes
*
* Revision 1.13  1999/02/10 18:17:21  csa
* Added beam-on calculations (D. McKee)
*
* Revision 1.12  1996/11/05 20:45:02  saw
* (SAW) Use parameter for G_LUN_CHARGE_SCALER instead of hard coded #
*
* Revision 1.11  1996/09/04 14:31:53  saw
* (JRA) Update BCM calculations
*
* Revision 1.10  1996/04/29 19:41:57  saw
* (JRA) Deal with out of order scaler events
*
* Revision 1.9  1996/01/22 15:10:03  saw
* (JRA) Extract event number from scaler events
*
* Revision 1.8  1996/01/16 18:39:17  cdaq
* (CB,SAW) Add current monitor calculations.  Make compatible with SAW's new
*          scaler header format.
*
* Revision 1.7  1995/10/09 17:55:32  cdaq
* (JRA) Add arrays for previous scalers and differences from previous scalers
*
* Revision 1.6  1995/09/01 13:41:25  cdaq
* (JRA) Calculate time of run
*
* Revision 1.5  1995/07/27  19:04:54  cdaq
* (SAW) Use specific bit manipulation routines for f2c compatibility
*
* Revision 1.4  1995/04/06  20:04:33  cdaq
* (JRA) Handle overflows and save them in real variables
*
* Revision 1.3  1994/07/07  15:24:24  cdaq
* (SAW) Fix bugs
*
c Revision 1.2  1994/07/07  15:23:16  cdaq
c (SAW) Correct pointers for actual bank structure
c
c Revision 1.1  1994/06/22  21:02:17  cdaq
c Initial revision
c
*
      implicit none
      save
      external jishft, jiand, jieor
      integer*4 event(*)
*
      character*17 here
      parameter (here='g_analyze_scalers')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 'gen_scalers.cmn'
      INCLUDE 'gen_run_info.cmn'
      INCLUDE 'gen_filenames.cmn'
*
      integer ind
      integer*4 cratenum                ! 1=hms,2=sos
      real*8 realscal
      logical update_bcms
*
      integer*4 jiand, jishft, jieor           ! Declare to help f2c
*
*     Scaler events have a header in from of each scaler.  High 16 bits
*     will contain the address (the switch settings).  Address for hall C
*     will be of the form DANN, where NN is the scaler number.  The low 16
*     bits will contain the number of scaler values to follow (this should
*     be no larger than 16, but we will allow more.)
*
*
*     NOTE that the variables gscaler(i) is REAL!!!!!
*     this is so that we can record the correct value when the 
*     hardware scalers (32 bit <> I*4) overflow.
*

      integer evtype, evnum, evlen, pointer
      integer scalid, countinmod, address, counter
*
*     Temporary variables for beam current and charge calculations
*
      real*8 ave_current_bcm1, ave_current_bcm2, ave_current_bcm3
      real*8 ave_current_unser
      real*8 delta_time
*
* Find if hms or sos scaler event (assumes first HMS scaler is DA01).
      if (jieor(jiand(jishft(event(3),-16),'FFFF'X),'DA01'X).eq.0) then !first scaler
        cratenum=1     !hms
      else
        cratenum=2     !sos
      endif
*
      evtype = jishft(event(2),-16)
      evnum = jiand(event(2),'FF'x)  ! last 2 bytes give event number (mod 256)
*
* evnum is mod(256), so must reset lastevnum for rollover
      if (evnum.eq.0 .and. gscal_lastevnum(cratenum).gt.200) then
        gscal_lastevnum(cratenum)=0
      else if (evnum.le.gscal_lastevnum(cratenum)) then
        write(6,*) 'STATUS: skipping outoforder scaler event:',
     &             ' crate,oldevnum,newevnum=',cratenum,
     &             gscal_lastevnum(cratenum),evnum
        return
      endif
*
      gscal_lastevnum(cratenum)=evnum
*
*     Should check against list of known scaler events
*
      evlen = event(1) + 1
      update_bcms = .false.
      if(evlen.gt.3) then           ! We have a scaler bank
        pointer = 3
*
        do while(pointer.lt.evlen)
*
          scalid = jiand(jishft(event(pointer),-16),'FF'x)
          countinmod = jiand(event(pointer),'FFFF'x)
          if(jieor(jiand(event(pointer),'FF000000'x),'DA000000'x).eq.0) then
c     Old style header with scaler ID @ 00FF0000
            scalid = jiand(jishft(event(pointer),-16),'FF'x)
            address = scalid*16
*
*     Might want to check that count is not to big.
*
            if(countinmod.ne.16) then
              err = 'Scaler module header word has count<>16'
              ABORT = .true.
              call g_add_path(here,err)
              return                    ! Safest action
            endif
          else
c
c     New style header with scaler ID @ FFF?0000
c     (If ? is non zero, it means we are starting in the middle of a scaler)
c     Allows for non multiple of 16 address starts
c
            address = jishft(event(pointer),-16)
*
*     Might want to check that count is not to big.
*
            if(countinmod.gt.16) then
              err = 'Scaler module header word has count >16'
              ABORT = .true.
              call g_add_path(here,err)
              return                    ! Safest action
            endif
          endif
*
          address = scalid*16
          do counter = 1,countinmod
            ind=address+counter
            realscal=dfloat(event(pointer+counter))
            if (ind.eq.gbcm1_index) update_bcms=.true. !assume bcms in same crate

* Save scaler value from previous scaler event:
            gscaler_old(ind) = gscaler(ind)

            if (realscal.lt.-0.5) then
              realscal=realscal+4294967296.
            endif
            if ( (realscal+dfloat(gscaler_nroll(ind))*4294967296.) .ge.
     &           gscaler(ind) ) then    ! 2**32 = 4.295e+9
              gscaler(ind) = realscal + gscaler_nroll(ind)*4294967296.
            else                        !32 bit scaler rolled over.
              gscaler_nroll(ind)=gscaler_nroll(ind)+1
              gscaler(ind) = realscal + gscaler_nroll(ind)*4294967296.
            endif
* Calculate difference between current scaler value and previous value:
            gscaler_change(ind) = gscaler(ind) - gscaler_old(ind)
          enddo
          pointer = pointer + countinmod + 1 ! Add 17 to pointer
        enddo
      else
        err = 'Event not big enough to contain scalers'
        ABORT = .true.
        call g_add_path(here,err)
        return
      endif

* calculate time of run (must not be zero to avoid div. by zero).
      g_run_time = max(0.001D00,gscaler(gclock_index)/gclock_rate)

* Calculate beam current and charge between scaler events

      if (update_bcms) then             ! can't assume in hms crate, moved for some runs

        delta_time = max(gscaler_change(gclock_index)/gclock_rate,.0001D00)

        ave_current_bcm1 = gbcm1_gain*((gscaler_change(gbcm1_index)
     &       /delta_time) - gbcm1_offset)
        ave_current_bcm3 = gbcm3_gain*((gscaler_change(gbcm3_index)
     &       /delta_time) - gbcm3_offset)
        ave_current_unser = gunser_gain*((gscaler_change(gunser_index)
     &       /delta_time) - gunser_offset)

          ave_current_bcm2 = gbcm2_gain*((gscaler_change(gbcm2_index)
     &         /delta_time) - gbcm2_offset)
*
* Remove old, hardwired check on run number.
*
*        if (gen_run_number.le.6268) then
*          ave_current_bcm2 = gbcm2_gain*sqrt(max(0.0D00,
*     &         (gscaler_change(gbcm2_index)/delta_time)-gbcm2_offset))
*        else
*          ave_current_bcm2 = gbcm2_gain*((gscaler_change(gbcm2_index)
*     &         /delta_time) - gbcm2_offset)
*        endif

        if (delta_time.gt.0.0001) then
          gbcm1_charge = gbcm1_charge + ave_current_bcm1*delta_time
          gbcm2_charge = gbcm2_charge + ave_current_bcm2*delta_time
          gbcm3_charge = gbcm3_charge + ave_current_bcm3*delta_time
          gunser_charge = gunser_charge + ave_current_unser*delta_time

*     Check for the "beam on" condition, and update "beam on" variables
*     if needed. I'm only using bcm2 (the current "best" bcm) right
*     now. This could be changed or added to.

          if (ave_current_bcm2 .ge. g_beam_on_thresh_cur(2)) then
             g_beam_on_run_time(2) = g_beam_on_run_time(2) + delta_time
             g_beam_on_bcm_charge(2) = g_beam_on_bcm_charge(2) + ave_current_bcm2*delta_time
          endif
          if (ave_current_bcm1 .ge. g_beam_on_thresh_cur(1)) then
             g_beam_on_run_time(1) = g_beam_on_run_time(1) + delta_time
             g_beam_on_bcm_charge(1) = g_beam_on_bcm_charge(1) + ave_current_bcm1*delta_time
          endif

          gscaler_event_num = gscaler_event_num + 1

*         Write out pertinent charge scaler rates for each scaler event.

          if (g_charge_scaler_filename.ne.' ') then
            write(G_LUN_CHARGE_SCALER,1001) gscaler_event_num, !scaler event num
     &           gscaler_change(gunser_index)/delta_time, !scaler rate(Hz)
     &           gscaler_change(gbcm1_index)/delta_time, !scaler rate(Hz)
     &           gscaler_change(gbcm2_index)/delta_time, !scaler rate(Hz)
     &           gscaler_change(gbcm3_index)/delta_time, !scaler rate(Hz)
     &           delta_time             !time since last scaler event (sec)
          endif
        endif
      endif

 1001 format(i6,4f13.2,f12.6)

      return
      end
