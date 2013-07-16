      subroutine revdis_getev(FAIL,why)
*--------------------------------------------------------
*- Return an interesting event or give up after a wait
*
*	4-Oct-1994 K.B.Beard, Hampton U.
* $Log: revdis_getev.f,v $
* Revision 1.2  1996/01/17 16:32:49  cdaq
* (SAW) Change an include file name
*
* Revision 1.1  1995/03/14 21:25:45  cdaq
* Initial revision
*
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      CHARACTER*12 here
      PARAMETER (here= 'revdis_getev')
*
      logical FAIL
      character*(*) why
*
      integer ierr
*
      INCLUDE 'gen_one_ev_info.cmn'
      INCLUDE 'gen_routines.dec'
*
*--------------------------------------------------------
*
      FAIL = .false.
*
      call thservset(gen_display_server_RPCprgmID,2)
      ierr = thcgetlist(gen_display_event_info,gen_display_RPCclientID
     $     ,gen_display_interesting,gen_display_wait_seconds
     $     ,gen_display_wait_events)
      call thservunset(gen_display_server_RPCprgmID,2)

      if(ierr.ne.0) then
        write(why,'(":thcgetlist failed with error",i5)') ierr
        call G_add_path(here,why)
        FAIL = .true.
      else
        why = ' '
      endif
*
      RETURN
      END




