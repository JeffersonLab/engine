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
* Revision 1.2  1998/12/01 20:59:06  saw
* (SAW) Checkin
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
      call G_decode_event_by_banks(event,ABORT,err)
      IF(ABORT) THEN
         call G_add_path(here,err)
         RETURN
      ENDIF
*
*
*  INTERRUPT ANALYSIS FOR PEDESTAL EVENTS.
*
*
      IF(gen_event_type .eq. 4) then            !pedestal event
        call g_analyze_pedestal(ABORT,err)
        update_peds = .true.                    !need to recalculate pedestals
        RETURN
      ENDIF
*
*  check to see if pedestals need to be recalculated.  Note that this is only
*  done if the event was NOT a scaler event, because of the 'return' at the
*  end of the pedestal handling call.
*
      IF(update_peds) then
        call g_calc_pedestal(ABORT,err)
        update_peds = .false.
      ENDIF
*
*-Beamline reconstruction
      IF((gen_event_type.ge.1).and.(gen_event_type.le.15).and.(gen_event_type.ne.4)) then !HMS/POLDER/COIN trig
        call g_trans_misc(FAIL,why)
        IF(err.NE.' ' .and. why.NE.' ') THEN
          call G_append(err,' & '//why)
        ELSEIF(why.NE.' ') THEN
          err= why
        ENDIF
        ABORT= ABORT .or. FAIL
        call g_analyze_misc(FAIL,why)
        IF(err.NE.' ' .and. why.NE.' ') THEN
          call G_append(err,' & '//why)
        ELSEIF(why.NE.' ') THEN
          err= why
        ENDIF
        ABORT= ABORT .or. FAIL
      ENDIF
*
*-HMS reconstruction
      IF((gen_event_type.ne.2).and.(gen_event_type.ne.4)) then  !HMS/COIN trig
        call H_reconstruction(FAIL,why)
        IF(err.NE.' ' .and. why.NE.' ') THEN
          call G_append(err,' & '//why)
        ELSEIF(why.NE.' ') THEN
          err= why
        ENDIF
        ABORT= ABORT .or. FAIL
      ENDIF
*
*-POLDER reconstruction (T20 experiment)
      IF((gen_event_type.ne.1).and.(gen_event_type.ne.4)) then  !Polder/COIN trig
        call t_reconstruction(FAIL,why)
        IF(err.NE.' ' .and. why.NE.' ') THEN
          call G_append(err,' & '//why)
        ELSEIF(why.NE.' ') THEN
          err= why
        ENDIF
        ABORT= ABORT .or. FAIL
      ENDIF
*
*-Fill histogram(s) with scaler values, for all event types
      IF(gen_event_type.ne.4) then  !physics events
        call g_scaler(FAIL,why)
        IF(err.NE.' ' .and. why.NE.' ') THEN
          call G_append(err,' & '//why)
        ELSEIF(why.NE.' ') THEN
          err= why
        ENDIF
        ABORT= ABORT .or. FAIL
      ENDIF

*
*-COIN reconstruction
*+++ gen_event_type 1 is an HMS single gen_event_type 2 is POLDER single
*++  and gen_event_type 4 is a pedestal
      IF((gen_event_type.ne.1).and.(gen_event_type.ne.2).and.(gen_event_type.ne.4)) then  !COIN trig
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
