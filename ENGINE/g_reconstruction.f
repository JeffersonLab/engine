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
*     
      logical FAIL
      character*1024 why
*
      logical update_peds               ! TRUE = There is new pedestal data
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
         !write(*,*) 'calling g_calc_pedestal'
        call g_calc_pedestal(ABORT,err)
        !write(*,*) 'g_calc_pedestal successful'
        update_peds = .false.
      ENDIF
*
*-Beamline reconstruction
*-for GEp, avoid event types 2 and 3!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! (No SOS!)
c      IF(gen_event_type.ge.1 .and. gen_event_type.le.3) then  !HMS/SOS/COIN trig
      if((gen_event_type.eq.1 .or. gen_event_type.eq.5 .or. 
     $     gen_event_type.eq.6).and.gen_analyze_beamline.ne.0) then ! 1 = HMS singles, 5 = BigCal singles, 6 = HMS-BigCal coin.
        
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
*
*-HMS reconstruction
c      IF(gen_event_type.eq.1 .or. gen_event_type.eq.3) then  !HMS/COIN trig
      
      if(gen_event_type.eq.1 .or. gen_event_type .eq. 6) then !HMS/COIN trig
         call H_reconstruction(FAIL,why)
        IF(err.NE.' ' .and. why.NE.' ') THEN
          call G_append(err,' & '//why)
        ELSEIF(why.NE.' ') THEN
          err= why
        ENDIF
        ABORT= ABORT .or. FAIL
      ENDIF
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
      if(gen_event_type.eq.5 .or. gen_event_type.eq.6) then !BIGCAL/HMS-BIGCAL COIN
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
*-GEP-COIN reconstruction
      if(gen_event_type.eq.6) then !GEp-coin. trig
         !write(*,*) 'calling gep_reconstruction'
         call GEp_reconstruction(FAIL,why)
         !write(*,*) 'gep_reconstruction successful'
         if(err.ne.' '.and.why.ne.' ') then
            call G_append(err,' & '//why)
         else if(why.ne.' ') then
            err = why
         endif
         ABORT = ABORT.or.FAIL
      endif
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
