      SUBROUTINE g_preproc_event(preprocessor_keep_event)
*---------------------------------------------------------------------
*   prototype C analysis routine
*
*     purpose and mothods : check the event for defined criteria and
*                           set flag to 1 if it meets these criteria.
*
*
*
*   created apr-29-1996 Dave Meekins
* $Log: g_preproc_event.f,v $
* Revision 1.1  1996/06/10 17:47:43  saw
* Initial revision
*
*---------------------------------------------------------------------


      IMPLICIT NONE
      SAVE

      character*20 here
      parameter (here='g_preproc_event')

      integer*4 preprocessor_keep_event

* make sure to include all the necessary include files
* so that the tests below can be carried out

      INCLUDE 'hms_data_structures.cmn'
      INCLUDE 'sos_data_structures.cmn'
      INCLUDE 'hms_scin_tof.cmn'
      INCLUDE 'sos_scin_tof.cmn'
      INCLUDE 'gen_event_info.cmn'


      preprocessor_keep_event=0

* test for good event

* ALL COINCIDENCE EVENTS, coin only (should disable tracking
* and decoding when analyzing, since no analysis is needed).
       if (gen_event_type.eq.3 .or. gen_event_type.eq.4) then
         preprocessor_keep_event=1
       endif

* ALL HMS (AND COINCIDENCE) EVENTS, (should disable tracking
* and decoding when analyzing, since no analysis is needed).
!       if (gen_event_type.eq.1 .or. gen_event_type.eq.3 .or.
!     &     gen_event_type.eq.4) then
!         preprocessor_keep_event=1
!       endif

* ALL SOS (AND COINCIDENCE) EVENTS, (should disable tracking
* and decoding when analyzing, since no analysis is needed).
!       if (gen_event_type.eq.2 .or. gen_event_type.eq.3 .or.
!     &     gen_event_type.eq.4) then
!         preprocessor_keep_event=1
!       endif

* MEEK'S TESTS. 
* Keep pedestals, SOS and COIN with good beta_notrk, reject HMS
!      if (gen_event_type.eq.1) then  !HMS singles trigger
!        if ((hbeta_notrk.gt.0).and.(hbeta_notrk.lt.0.01))then
!          preprocessor_keep_event=1
!        endif
!
!      else if (gen_event_type.eq.2) then !SOS singles trigger
!        if((sbeta_notrk.gt.0).and.(sbeta_notrk.lt.1.5))then
!          preprocessor_keep_event=1
!        endif
!
!      else if (gen_event_type.eq.3) then !COIN trigger
!        if( ((sbeta_notrk.gt.0).and.(sbeta_notrk.lt.1.5)) .or.
!     &      ((hbeta_notrk.gt.0).and.(hbeta_notrk.lt.0.01)) ) then
!          preprocessor_keep_event=1
!        endif
!
!      else if (gen_event_type.eq.4) then !PED trigger
!         preprocessor_keep_event=1
!
!      else
!        write(6,*) 'g_preproc_event was called for event type',
!     &             gen_event_type
!        write(6,*) '     should only be called for event types 1-4 (I think-JRA)'
!
!      endif

      RETURN
      END
