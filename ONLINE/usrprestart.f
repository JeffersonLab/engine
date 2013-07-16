      subroutine usrprestart(run_number, run_type)
*
* $Log: usrprestart.f,v $
* Revision 1.1  1994/06/14 20:42:13  cdaq
* Initial revision
*
*
      implicit none
      save
*
      character*11 here
      parameter (here='usrprestart')
*
      integer*4 run_number, run_type
*
*     This common block also used in usrprestart.f.  Should probably
*     move these two lines to an include file.
      character*80 g_config_environmental_var
      common /ENVVAR/ g_config_environmental_var
*
      logical ABORT
      character*800 err
*
*     Every new run we may change the filenames
*
      ABORT = .false.
      err = ' '
*
      call g_init_filenames(ABORT,err,g_config_environmental_var)
      if(ABORT .or. err.ne.' ') then
         call g_add_path(here,err)
         call dalogmsg(err)
      endif
*
      ABORT = .false.
      err = ' '
      call g_initialize(ABORT,err)
      if(ABORT .or. err.ne.' ') then
         call g_add_path(here,err)
         call dalogmsg(err)
      endif
*
      ABORT = .false.
      err = ' '
      call g_reset_event(ABORT,err)
      if(ABORT .or. err.ne.' ') then
         call g_add_path(here,err)
         call dalogmsg(err)
      endif
*
      return
      end
