      SUBROUTINE G_reconstruction(event,ABORT,err)
*----------------------------------------------------------------------
*-       Prototype hall C reconstruction routine
*- 
*-   Purpose and Methods : Given previously filled data structures,
*-                         reconstruction is performed and status returned
*- 
*-   Inputs:
*-       event      Pointer to the first word (length) of an event data bank.
*-
*-   Output: ABORT	- success or failure
*-         : err	- reason for failure, if any
*- 
*-   Created  20-Oct-1993   Kevin B. Beard
*-   Modified 20-Nov-1993   KBB for new error routines
* $Log$
* Revision 1.13.24.13  2008/09/29 16:02:11  puckett
* added checking for existence of histograms before calling fill routines
*
* Revision 1.13.24.12  2008/07/29 16:16:28  puckett
* added HMS cointime cut
*
* Revision 1.13.24.11  2008/04/23 18:02:31  cdaq
* *** empty log message ***
*
* Revision 1.13.24.10  2008/04/17 16:37:07  cdaq
*  added b_use_cointime_cut
*
* Revision 1.13.24.9  2008/01/08 22:43:13  cdaq
* *** empty log message ***
*
* Revision 1.13.24.8  2007/10/19 14:54:58  cdaq
* *** empty log message ***
*
* Revision 1.13.24.7  2007/10/19 14:49:41  cdaq
* *** empty log message ***
*
* Revision 1.13.24.6  2007/10/17 16:08:08  cdaq
* Changed if-block for beamline analysis. Now call for any event type 1-8 if flag is set
*
* Revision 1.13.24.5  2007/10/10 16:24:31  puckett
* *** empty log message ***
*
* Revision 1.13.24.4  2007/10/08 19:22:59  puckett
* Added bad channel list handling for BigCal
*
* Revision 1.13.24.3  2007/09/07 16:08:05  puckett
* put event type 3 option back in for call to h_reconstruction, just in case somebody wants to use it. Also added event type 7 and 8 for calls to b_reconstruction, for the case of the cosmic/light box trigger for bigcal
*
* Revision 1.13.24.2  2007/06/04 14:56:06  puckett
* changed hit array structure for trigger related signals
*
* Revision 1.13.24.1  2007/05/15 02:55:01  jones
* Start to Bigcal code
*
* Revision 1.13  1996/01/22 15:23:34  saw
* (SAW) Add calls to analyze beam position
*
* Revision 1.12  1995/10/09 18:28:41  cdaq
* (JRA) Only call spec analysis routines that correspond to trigger type
*
* Revision 1.11  1995/05/22 20:50:45  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.10  1995/04/01  19:50:22  cdaq
* (JRA) Add pedestal event handling
*
* Revision 1.9  1994/11/22  20:13:39  cdaq
* (SPB) Uncomment call to SOS code
*
* Revision 1.8  1994/10/11  20:03:27  cdaq
* (JRA) Comment out call to SOS
*
* Revision 1.7  1994/08/04  03:46:31  cdaq
* (SAW) Add call to Breuer's hack_anal
*
* Revision 1.6  1994/06/17  03:36:57  cdaq
* (KBB) Upgrade error reporting
*
* Revision 1.5  1994/04/15  20:37:41  cdaq
* (SAW) for ONLINE compatibility get event from argument instead of commmon.
*
* Revision 1.4  1994/02/02  19:58:47  cdaq
* Remove some damn nulls at the end of the file
*
* Revision 1.3  1994/02/02  18:53:43  cdaq
* Actually add call to g_decode_event_by_banks
*
* Revision 1.2  1994/02/01  21:28:48  cdaq
* Add call to G_decode_event_by_banks
*
*-
*- All standards are from "Proposal for Hall C Analysis Software
*- Vade Mecum, Draft 1.0" by D.F.Geesamn and S.Wood, 7 May 1993
*-
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      integer*4 event(*)
*
      character*16 here
      parameter (here= 'G_reconstruction')
*     
      logical ABORT
      character*(*) err
*     
      INCLUDE 'gen_data_structures.cmn'
      INCLUDE 'gen_event_info.cmn'
      include 'gen_run_info.cmn'
      INCLUDE 'hack_.cmn'
      include 'bigcal_data_structures.cmn'
      include 'bigcal_bypass_switches.cmn'
      include 'gep_data_structures.cmn'
*     
      logical FAIL
      character*1024 why
*
      logical update_peds               ! TRUE = There is new pedestal data
      integer i
*--------------------------------------------------------
*
      ABORT= .FALSE.
      err= ' '                                  !erase any old errors
*

      !write(*,*) 'segfault occurs somewhere in g_reconstruction.'
      !write(*,*) 'about to call g_decode_event_by_banks'
      call G_decode_event_by_banks(event,ABORT,err)
      IF(ABORT) THEN
         call G_add_path(here,err)
         RETURN
      ENDIF

      !write(*,*) 'g_decode_event_by_banks finished successfully'

*
*
*  INTERRUPT ANALYSIS FOR PEDESTAL EVENTS.
*
*
      IF(gen_event_type .eq. 4) then            !pedestal event
         !write(*,*) 'calling g_analyze_pedestal'
         call g_analyze_pedestal(ABORT,err)

         !write(*,*) 'g_analyze_pedestal successful'
        update_peds = .true.                    !need to recalculate pedestals
        RETURN
      ENDIF
*
*  check to see if pedestals need to be recalculated.  Note that this is only
*  done if the event was NOT a scaler event, because of the 'return' at the
*  end of the pedestal handling call.
*
      IF(update_peds) then
         !write(*,*) 'calling g_calc_pedestal,evtype=',gen_event_type
        call g_calc_pedestal(ABORT,err)
        !write(*,*) 'g_calc_pedestal successful'
        update_peds = .false.
c        ncalls_calc_ped = ncalls_calc_ped + 1
      ENDIF
*
*-Beamline reconstruction
*-for GEp, avoid event types 2 and 3!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! (No SOS!)
c      IF(gen_event_type.ge.1 .and. gen_event_type.le.3) then  !HMS/SOS/COIN trig
c$$$      if((gen_event_type.eq.1 .or. gen_event_type.eq.5 .or. 
c$$$     $     gen_event_type.eq.6).and.gen_analyze_beamline.ne.0) then ! 1 = HMS singles, 5 = BigCal singles, 6 = HMS-BigCal coin.
        
      if(b_suppress_annoying_pulser.ne.0.and.bigcal_annoying_pulser_event) then
c         write(*,*) 'found annoying pulser event, skipping'
         return
      endif

      if((gen_event_trigtype(4).eq.1.or.gen_event_trigtype(1).eq.1).and.
     $     gep_bypass_coin1.ne.0) then
         return
      endif
      
      if((gen_event_trigtype(5).eq.1.or.gen_event_trigtype(2).eq.1).and.
     $     gep_bypass_coin2.ne.0) then
         return
      endif

      if(gen_event_type.ge.1.and.gen_event_type.le.8.and.gen_analyze_beamline
     $     .ne.0) then
         !write(*,*) 'calling g_trans_misc'
        call g_trans_misc(FAIL,why)
        !write(*,*) 'g_trans_misc successful'
        IF(err.NE.' ' .and. why.NE.' ') THEN
          call G_append(err,' & '//why)
        ELSEIF(why.NE.' ') THEN
          err= why
        ENDIF
        ABORT= ABORT .or. FAIL

        !write(*,*) 'calling g_analyze_misc' 
        call g_analyze_misc(FAIL,why) !UNCOMMENT WHEN WE GET IN HALL AND BEAM ON!
        !write(*,*) 'g_analyze_misc successful'
        IF(err.NE.' ' .and. why.NE.' ') THEN
          call G_append(err,' & '//why)
        ELSEIF(why.NE.' ') THEN
          err= why
        ENDIF
        ABORT= ABORT .or. FAIL
      ENDIF
c mkj add cut on coincidence time to remove accidentals
      b_passed_cointime_cut = .true.
      h_passed_cointime_cut = .true.

      if ( b_use_cointime_cut .ne. 0) then
         b_passed_cointime_cut = .false.
         do i=1,8
            if ( gep_btime(i) .gt. gep_btime_elastic(1) 
     $           .and. gep_btime(i) .lt. gep_btime_elastic(2) )
     >           b_passed_cointime_cut = .true.
         enddo
c         if (.not. b_passed_cointime_cut)   return
      endif
      
      
      if ( h_use_cointime_cut .ne. 0) then
         h_passed_cointime_cut = .false.
         
         do i=1,8
            if ( gen_event_trigtype(4).eq.1 .and. gep_h1time(i) .gt. 
     $           gep_htrig_cut(1) .and. gep_h1time(i) .lt. 
     $           gep_htrig_cut(2) ) then
               h_passed_cointime_cut = .true.
            endif
            
            if( gen_event_trigtype(5).eq.1 .and. gep_h2time(i) .gt.
     $           gep_htrig_cut(3) .and. gep_h2time(i) .lt.
     $           gep_htrig_cut(4) ) then
               h_passed_cointime_cut = .true.
            endif
            
c            if(.not. h_passed_cointime_cut) return
         enddo
      endif

      if(.not.(h_passed_cointime_cut.and.b_passed_cointime_cut)) return

c
*
*-HMS reconstruction
c      IF(gen_event_type.eq.1 .or. gen_event_type.eq.3) then  !HMS/COIN trig
      
      if(gen_event_type.eq.1 .or. gen_event_type .eq. 6 .or. 
     $     gen_event_type .eq. 3) then               !HMS/COIN trig

c         write(*,*) 'calling HMS reconstruction, gen_event_type=',gen_event_type

         call H_reconstruction(FAIL,why)
        IF(err.NE.' ' .and. why.NE.' ') THEN
          call G_append(err,' & '//why)
        ELSEIF(why.NE.' ') THEN
          err= why
        ENDIF
        ABORT= ABORT .or. FAIL
      ENDIF

c      write(*,*) 'h_reconstruction successful'

*
*-SOS reconstruction
      IF(gen_event_type.eq.2 .or. gen_event_type.eq.3) then  !SOS/COIN trig
        call S_reconstruction(FAIL,why)
        IF(err.NE.' ' .and. why.NE.' ') THEN
          call G_append(err,' & '//why)
        ELSEIF(why.NE.' ') THEN
          err= why
        ENDIF
        ABORT= ABORT .or. FAIL
      ENDIF
*-BIGCAL reconstruction
      if(gen_event_type.ge.5 .and. gen_event_type.le.8) then !5.BIGCAL/6.HMS-BIGCAL COIN/7.COSMIC/8.LIGHT BOX
         !write(*,*) 'calling b_reconstruction'
         
         call B_reconstruction(FAIL,why)

         !write(*,*) 'b_reconstruction successful'
         IF(err.NE.' ' .and. why.NE.' ') THEN
            call G_append(err,' & '//why)
         ELSEIF(why.NE.' ') THEN
            err= why
         ENDIF
         ABORT= ABORT .or. FAIL
      endif

c      write(*,*) 'b_reconstruction successful'

*-GEP-COIN reconstruction
      if(gen_event_type.eq.6) then !GEp-coin. trig
*         write(*,*) 'calling gep_reconstruction'
         call GEp_reconstruction(FAIL,why)
*         write(*,*) 'gep_reconstruction successful'
         if(err.ne.' '.and.why.ne.' ') then
            call G_append(err,' & '//why)
         else if(why.ne.' ') then
            err = why
         endif
         ABORT = ABORT.or.FAIL
      endif
      
c      write(*,*) 'gep_reconstruction successful'
*
*-COIN reconstruction
      IF(gen_event_type.eq.3) then  !COIN trig
        call C_reconstruction(FAIL,why)
        IF(err.NE.' ' .and. why.NE.' ') THEN
          call G_append(err,' & '//why)
        ELSEIF(why.NE.' ') THEN
          err= why
        ENDIF
        ABORT= ABORT .or. FAIL
      ENDIF
*
      IF(ABORT .or. err.NE.' ') call G_add_path(here,err)
*
      IF(hack_enable.ne.0) then
        call hack_anal(FAIL,why)
        IF(err.NE.' ' .and. why.NE.' ') THEN
          call G_append(err,' & '//why)
        ELSEIF(why.NE.' ') THEN
          err= why
        ENDIF
        ABORT= ABORT .or. FAIL
      ENDIF
*
      RETURN
      END
