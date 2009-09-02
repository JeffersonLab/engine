      SUBROUTINE H_TRANS_DC(ABORT,err)
*--------------------------------------------------------
*-
*-   Purpose and Methods : Translate HMS raw drift and start time 
*-                                to decoded information 
*-
*-      Required Input BANKS     HMS_RAW_DC
*-                               HMS_DECODED_SCIN
*-
*-      Output BANKS             HMS_DECODED_DC
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
* $Log$
* Revision 1.15.26.2  2009/09/02 13:40:39  jones
* eliminate commented emacs definitions
*
* Revision 1.15.26.1  2009/05/18 14:08:00  jones
* 1) add code to remove all DC hits for one plane if that plane
*    has more than h_max_hits_per_plane..
* 2) add code to purge DC hits based on matching the location of
*    paddles in S1X and S1Y
*
* Revision 1.15  2002/10/02 13:42:43  saw
* Check that user hists are defined before filling
*
* Revision 1.14  1996/09/04 14:23:38  saw
* (??) Cosmetic
*
* Revision 1.13  1996/01/16 21:37:13  cdaq
* (JRA) Change sign on hstart_time
*
* Revision 1.12  1995/10/11 13:51:04  cdaq
* (JRA) Cleanup, add bypass switch to h_dc_eff call
*
* Revision 1.11  1995/08/30 15:27:39  cdaq
* (JRA) Add call to h_dc_eff, warn about invalid plane numbers
*
* Revision 1.10  1995/05/22  19:39:32  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.9  1995/05/17  14:02:20  cdaq
* (JRA) Add hist for all dc tdc's in one histogram.
*
* Revision 1.8  1995/04/06  19:34:34  cdaq
* (JRA) HMAX_NUM_DC_PLANES -> HDC_NUM_PLANES
*
* Revision 1.7  1994/09/14  14:10:49  cdaq
* (JRA) Initialize hdc_center array first time.
*
* Revision 1.6  1994/08/16  13:24:58  cdaq
* (DJA) Move call to h_fill_dc_dec_hist to h_pattern_recognition
*
* Revision 1.5  1994/06/15  20:35:59  cdaq
* (DFG) Add upper and lower limit for valid TDC
*
* Revision 1.4  1994/04/13  17:59:46  cdaq
* (DFG) add histogram call and remove raw dump
*
* Revision 1.3  1994/03/24  19:48:48  cdaq
* (DFG) add print routines and flags
*       check plane number and wire number for validity
*
* Revision 1.2  1994/02/22  05:27:06  cdaq
* (SAW) Make err ' ' instead of ''
*
* Revision 1.1  1994/02/19  06:21:23  cdaq
* Initial revision
*
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*10 here
      parameter (here= 'H_TRANS_DC')
*
      logical ABORT
      character*(*) err
*
      include 'hms_data_structures.cmn'
      include 'gen_constants.par'
      include 'gen_units.par'
      include 'hms_tracking.cmn'
      include 'hms_geometry.cmn'
      include 'hms_track_histid.cmn'
      include 'hms_bypass_switches.cmn'
c added for pruning of junk wires
      include 'hms_scin_tof.cmn'

*
*--------------------------------------------------------
      real*4 h_drift_dist_calc
      external h_drift_dist_calc
      integer*4 ihit,goodhit,old_wire,old_pln,wire,pln,chamber
      real*4 histval
*
c new arrays for checking the two arrays below. 
      integer dcscin(2,16,12,128),plane,counter,ihitsc
      common/pybtest/ dcscin

c hard-wired values of the peak positions
c in the DC plane distributions corresponding to 
c X1 or Y1 hodoscope hit counters. These come from 
c fitting the distribtuions in dsscin array from
c run 72994, April 28, 2009 P. Bosted
      real iw0(12)/111.5, -11.0, 1.3, 108.1, 64., 2.0,
     >             115.0, -12.5,-3.0, 113.6, 67., -1.5/
      real iwsl(12)/ -6.5,  6.8, 6.4, -6.3, -6.7, 6.5,
     >               -7.0,  7.0, 7.0, -7.0, -7.0, 7.0/
      real iwcntr

c Tolerance for how many wires on each side of the
c peak position will be kept by the "wire cleaning"
c code below. Adjusted to keep essentailly all good 
c hits for now.
      real iwtol(12)/8., 8., 11., 11., 8., 8.,
     >               7., 7., 11., 11., 7., 7./

c array to decide if a wire should be purged from
c the list beacuse does not match a good hodoscope hit
      logical purgewire(1000)

c array to decide if wire purging should take place
c for a given plane
      logical purgeplane(12),first/.true./

c array to count how many RAW hits passing TDC cuts
c there are per plane
      integer wperplane(12)

      ABORT= .FALSE.
      err= ' '
      old_wire = -1
      old_pln = -1
      goodhit = 0

c make sure wire purging parameters are reasonable
      if(first) then
        write(*,'(//''Using '',f6.1,
     >     '' nsec for wire purging'')') h_iwslop
        if(h_iwslop.lt.-3.0) then
          h_iwslop = 0.0
          write(*,'(//''*******************************'')')
          write(*,'(''This is too small to work: reset'',
     >      '' h_iwslop to 0.0 nsec'')')
          write(*,'(//''*******************************'')')
        endif
        if(h_iwslop.gt.60.0) then
          write(*,'(''This means wire purging essentially'',
     >      '' is turned off!'')')
        endif
        write(*,'(//''Using max of '',i6,
     >    '' hits per plnae'')') h_max_hits_per_plane
        if(h_max_hits_per_plane.lt.1) then
          h_max_hits_per_plane=6
          write(*,'(''*********************************'')')
          write(*,'(''This is too small: Resetting to 6'')')
        endif
        first = .false.
      endif

      if (hdc_center(1).eq.0.) then   !initialize hdc_center if not yet set.
        do pln = 1, hdc_num_planes
          chamber = hdc_chamber_planes(pln)
          hdc_center(pln) = hdc_xcenter(chamber)*sin(hdc_alpha_angle(pln))+
     &                      hdc_ycenter(chamber)*cos(hdc_alpha_angle(pln))
        enddo
      endif
      
! Inititalize this array
      do pln=1,12
        wperplane(pln)=0
        purgeplane(pln) = .false.
      enddo

! Check if any X1 or Y1 hodoscope hits. Need at least
! cone to purge wires in corresponding planes
      do ihitsc = 1 , hscin_tot_hits
       plane = hscin_plane_num(ihitsc) 
       counter = hscin_counter_num(ihitsc)
       if(hgood_scin_time(1,ihitsc)) then
        if(plane.eq.1.and.counter.gt.1.and.
     >     counter.lt.16) then
         purgeplane(1)=.true.
         purgeplane(3)=.true.
         purgeplane(4)=.true.
         purgeplane(6)=.true.
         purgeplane(7)=.true.
         purgeplane(9)=.true.
         purgeplane(10)=.true.
         purgeplane(12)=.true.
        endif
        if(plane.eq.2.and.counter.gt.1.and.
     >     counter.lt.10) then
         purgeplane(2)=.true.
         purgeplane(5)=.true.
         purgeplane(8)=.true.
         purgeplane(11)=.true.
        endif
       endif
      enddo

*     Are there any raw hits
      if(hdc_raw_tot_hits.gt.0) then
*     loop over all raw hits
        do ihit=1,hdc_raw_tot_hits
          pln = hdc_raw_plane_num(ihit)
          wire  = hdc_raw_wire_num(ihit)

! Initialize all wires in this plane to be purged,
! if there is appropriate X1 or Y1 to test on below
          purgewire(min(1000,ihit))=
     >      purgeplane(max(1,min(12,pln)))

! Count how many hits passing TDC cuts there are per
! plane (to use later to remove planes with too many hits)
          if(hdc_raw_tdc(ihit).gt.hdc_tdc_min_win(pln).and.
     >       hdc_raw_tdc(ihit).lt.hdc_tdc_max_win(pln).and.
     >       pln.gt.0.and.pln.le.12) then
           wperplane(pln)=wperplane(pln)+1
          endif

c actually set the purging flag
          do ihitsc = 1 , hscin_tot_hits
           plane = hscin_plane_num(ihitsc) 
           counter = hscin_counter_num(ihitsc)
           if(hgood_scin_time(1,ihitsc).and.
     >        pln.gt.0.and.pln.le.12.and.
     >        plane.gt.0 .and. plane.lt.3) then
            iwcntr = iw0(pln) + iwsl(pln)*counter
            if(plane.eq.1.and.(pln.eq.1.or.pln.eq.3.or.
     >         pln.eq.4.or.pln.eq.6.or.pln.eq.7.or.
     >         pln.eq.9.or.pln.eq.10.or.pln.eq.12).and.
     >         counter.gt.1.and.counter.lt.16) then
             if(wire.gt.iwcntr - iwtol(pln) - h_iwslop .and.
     >          wire.lt.iwcntr + iwtol(pln) + h_iwslop) then
              purgewire(min(1000,ihit))= .false.
             endif
            endif
            if(plane.eq.2.and.(pln.eq.2.or.pln.eq.5.or.
     >         pln.eq.8.or.pln.eq.11).and.
     >         counter.gt.1.and.counter.lt.10) then
             if(wire.gt.iwcntr - iwtol(pln) - h_iwslop .and.
     >          wire.lt.iwcntr + iwtol(pln) + h_iwslop) then
              purgewire(min(1000,ihit))= .false.
             endif
            endif
           endif
          enddo

c Increment one-line histograms to see corresponance
c of hodoscope and wire numbers for each plane, wire
          do ihitsc = 1 , hscin_tot_hits
           plane = hscin_plane_num(ihitsc) 
           counter = hscin_counter_num(ihitsc)
           if(hgood_scin_time(1,ihitsc).and.
     >       counter.gt.0 .and. counter.le.16.and.
     >       pln.gt.0.and.pln.le.12.and.
     >       wire.gt.0.and.wire.le.128.and.
     >       hdc_raw_tdc(ihit).gt.hdc_tdc_min_win(pln).and.
     >       hdc_raw_tdc(ihit).lt.hdc_tdc_max_win(pln).and.
     >       plane.gt.0 .and. plane.lt.3) then
             if(hdc_raw_tot_hits.lt.17) then
              dcscin(plane,counter,pln,wire) = 
     >          dcscin(plane,counter,pln,wire) + 1
             endif
           endif
          enddo

*     check valid plane and wire number
          if(pln.gt.0 .and. pln.le. hdc_num_planes) then
            histval=float(hdc_raw_tdc(ihit))
            if(hidrawtdc.gt.0) call hf1(hidrawtdc,histval,1.)
*     test if tdc value less than lower limit for good hits
            if(hdc_raw_tdc(ihit) .lt. hdc_tdc_min_win(pln))  then
              hwire_early_mult(wire,pln) = hwire_early_mult(wire,pln)+1
            else
              if(hdc_raw_tdc(ihit) .gt. hdc_tdc_max_win(pln))  then
                hwire_late_mult(wire,pln) = hwire_late_mult(wire,pln)+1
              else
*     test for valid wire number
                if(wire .gt. 0 .and. wire .le. hdc_nrwire(pln) ) then
*     test for multiple hit on the same wire
c                  if(pln .eq. old_pln .and. wire .eq. old_wire ) then
c added test on number wires in a plane. If too many,
c none of them will be used. 4/28/2009 P. Bosted
c also added the wire purging based on hodo X1 and Y1
c in this place
                  if((pln .eq. old_pln .and. 
     >                wire .eq. old_wire ).or.
c 6 is optimum value
     >                wperplane(min(12,max(1,pln))).gt.
     >                h_max_hits_per_plane .or.
     >                purgewire(min(1000,ihit)))then
                    hwire_extra_mult(wire,pln) = 
     >                hwire_extra_mult(wire,pln)+1
                  else
                    
*     valid hit proceed with decoding
                    goodhit = goodhit + 1
                    hdc_plane_num(goodhit) = hdc_raw_plane_num(ihit)
                    hdc_wire_num(goodhit) = hdc_raw_wire_num(ihit)
                    hdc_tdc(goodhit) = hdc_raw_tdc(ihit)

                    if(hdc_wire_counting(pln).eq.0) then    !normal ordering
                      hdc_wire_center(goodhit) = hdc_pitch(pln)
     &                     * (float(wire)-hdc_central_wire(pln))
     &                     - hdc_center(pln)
                    else
                      hdc_wire_center(goodhit) = hdc_pitch(pln)
     &                     * ((hdc_nrwire(pln)+(1-wire))
     &                     - hdc_central_wire(pln)) - hdc_center(pln)
                    endif

                    hdc_drift_time(goodhit) = - hstart_time
     &                   - float(hdc_tdc(goodhit))*hdc_tdc_time_per_channel
     &                   + hdc_plane_time_zero(pln)
*  find dist in pattern_recognition, after apply propogation correction.
*                    hdc_drift_dis(goodhit) = h_drift_dist_calc
*     $                   (pln,wire,hdc_drift_time(goodhit))
                    hdc_hits_per_plane(pln) = hdc_hits_per_plane(pln) + 1
                    hwire_mult(wire,pln) = hwire_mult(wire,pln)+1
                    
                  endif                 ! end test on duplicate wire
                  old_pln = pln
                  old_wire  = wire
                endif                   ! end test on valid wire number
              endif                     ! end test on hdc_tdc_max_win
            endif                       ! end test on hdc_tdc_min_win
          else                          ! if not a valid plane number
            write(6,*) 'H_TRANS_DC: invalid plane number = ',pln
          endif                         ! end test on valid plane number
        enddo                           ! end loop over raw hits
*     
*     set total number of good hits
*     
        HDC_TOT_HITS = goodhit
*
        if (hbypass_dc_eff.eq.0) call h_dc_eff !only call if there is a hit.
*
      endif                             !  end test on hdc_raw_tot_hits.gt.0
*
*
*     Dump decoded banks if flag is set
      if(hdebugprintdecodeddc.ne.0) then
        call h_print_decoded_dc(ABORT,err)
      endif
*     
      RETURN
      END
