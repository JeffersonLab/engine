      subroutine usrdownload(fname)
*
*     ONLINE ENGINE - Hall C online Analyzer
*
* $Log: usrdownload.f,v $
* Revision 1.2  1994/06/16 18:37:06  cdaq
* (SAW) Move in code from usrmain
*
* Revision 1.1  1994/06/14  20:43:41  cdaq
* Initial revision
*
      implicit none
      save
      character*(*) fname
*
      character*11 here
      parameter (here='usrdownload')
*
      logical oncethru
      data oncethru /.false./
*
*     This common block also used in usrprestart.f.  Should probably
*     move these two lines to an include file.
*
      character*80 g_config_environmental_var
      common /ENVVAR/ g_config_environmental_var
*
      logical ABORT
      character*800 err
*
      if(.not.oncethru) then

         ABORT = .FALSE.
         err = ' '
         g_config_environmental_var = 'ONLINE_CONFIG_FILE'

         call g_register_variables(ABORT,err)
         if(ABORT .or. err.ne.' ') then
            call g_add_path(here,err)
            call dalogmsg(err)
         endif
         
         ABORT = .FALSE.
         err = ' '
         call g_init_filenames(ABORT,err,g_config_environmental_var)
         if(ABORT .or. err.ne.' ') then
            call g_add_path(here,err)
            call dalogmsg(err)
         endif
         
         ABORT = .FALSE.
         err = ' '
         call g_decode_init(ABORT,err)
         if(ABORT .or. err.ne.' ') then
            call g_add_path(here,err)
            call dalogmsg(err)
         endif

         oncethru = .true.
      endif
*
      return
      end
