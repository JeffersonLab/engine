      subroutine usrevent(event, len, flag)
*
*     ONLINE ENGINE - Hall C online Analyzer
*
* $Log$
* Revision 1.1  1994/06/16 03:49:10  cdaq
* Initial revision
*
*
      implicit none
      integer*4 event(*), len, flag
*
      character*8 here
      parameter (here='usrevent')
*
      logical OK, ABORT, problems
      character*800 err

*
*     We need to make sure that clear_event doesn't know about event array.
*
      ABORT = .false.
      err = ' '
      call g_clear_event(ABORT,err)
      if(ABORT .or. err.ne.' ') then
         call g_add_path(here,err)
         call dalogmsg(err)
      endif
*
      ABORT = .false.
      err = ' '
      call g_examine_physics_event(event,ABORT,err)
*
      if(.NOT.ABORT) then
         call g_reconstruction(event,ABORT,err)
         if(ABORT .or. err.ne.' ') then
            call g_add_path(here,err)
            call dalogmsg(err)
         endif
         
*
         ABORT = .false.
         err = ' '
         call g_keep_results(ABORT,err)
         if(ABORT .or. err.ne.' ') then
            call g_add_path(here,err)
            call dalogmsg(err)
         endif
*     
      endif
      return
      end

