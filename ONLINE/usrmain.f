*
*     ONLINE ENGINE - Hall C online Analyzer
*
* $Log: usrmain.f,v $
* Revision 1.2  1994/06/16 18:36:15  cdaq
* (SAW) Move register, g_init_filenames call and map file reading to usrdownload
*
* Revision 1.1  1994/06/16  03:49:26  cdaq
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
*
*    Open communication with Run Control
*
      call rcService(rc_service_eb)
      call rcService(rc_service_ana)
      call rcExecute()

*     Never return from rcService

      end

