      subroutine usrend
*
* $Log: usrend.f,v $
* Revision 1.1  1994/06/14 20:42:02  cdaq
* Initial revision
*
*
      implicit none
      save

      character*6 here
      parameter (here='usrend')
*
      logical ABORT
      character*800 err
*
      call g_proper_shutdown(ABORT,err)
*
      if(ABORT .or. err.ne.' ') then
         call g_add_path(here,err)
         call dalogmsg(err)
      endif
*
*     Do we need to rename the run?
*
      return 
      end

	
