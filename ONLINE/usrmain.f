*
*     ONLINE ENGINE - Hall C online Analyzer
*
* $Log$
* Revision 1.1  1994/06/16 03:49:26  cdaq
* Initial revision
*

      program usrmain

      implicit none
      save
*     
*     We will be an analysis program so need the analysis services
*
      integer rc_service_eb, rc_service_ana ! Pointers to common blocks
      common/rc_service_eb/rc_service_eb
      common/rc_service_ana/rc_service_ana
*
      character*7 here
      parameter (here='usrmain')
*
*     This common block also used in usrprestart.f.  Should probably
*     move these two lines to an include file.
      character*80 g_config_environmental_var
      common /ENVVAR/ g_config_environmental_var

      logical OK, ABORT
      character*800 err
      character*80 nodename
      
      g_config_environmental_var = 'ONLINE_CONFIG_FILE'
      err = ' '

      call getenv('NODE',nodename)
      call dalogopen(nodename,'ANA',0)

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
      call g_decode_init(ABORT,err)     ! May want to move to download
      if(ABORT .or. err.ne.' ') then
         call g_add_path(here,err)
         call dalogmsg(err)
      endif

c
c    Open communication with Run Control
c
      call rcService(rc_service_eb)
      call rcService(rc_service_ana)
      call rcExecute()

c     Never return from rcService

      end
c

