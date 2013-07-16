      subroutine usrdump
*
* $Log: usrdump.f,v $
* Revision 1.1  1994/06/14 20:42:33  cdaq
* Initial revision
*
*
      implicit none
      save

      character*7 here
      parameter (here='usrdump')
*
      logical OK,ABORT
      character*800 err
*
      call g_dump_histograms(ABORT,err)
*
      if(ABORT .or. err.ne.' ') then
         call g_add_path(here,err)
         call dalogmsg(err)
      endif
*
      return 
      end
