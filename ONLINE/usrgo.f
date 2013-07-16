      subroutine usrgo()
*
*     ONLINE ENGINE - Hall C online Analyzer
*
* $Log: usrgo.f,v $
* Revision 1.1  1994/06/14 20:42:05  cdaq
* Initial revision
*
      implicit none
      save

      character*5 here
      parameter (here='usrgo')
*
      logical OK, ABORT
      character*800 err

      call g_clear_event(ABORT,err)
      if(ABORT .or. err.ne.' ') then
         call g_add_path(here,err)
         call dalogmsg(err)
      endif
*
      return
      end
