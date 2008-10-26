      subroutine g_analyze_pedestal(ABORT,err)
*
* $Log$
* Revision 1.2.24.1.2.2  2008/10/26 19:12:33  cdaq
* SEM
*
* Revision 1.2.24.1.2.1  2008/05/15 18:59:21  bhovik
* 1'st version
*
* Revision 1.2.24.1  2007/05/15 02:55:01  jones
* Start to Bigcal code
*
* Revision 1.2  1996/01/22 15:09:24  saw
* (JRA) Add call to g_analyze_beam_pedestal
*
* Revision 1.1  1995/04/01 19:36:55  cdaq
* Initial revision
*
*
      implicit none
*
      character*18 here
      parameter (here='g_analyze_pedestal')
*
      logical ABORT
      character*(*) err
*
      call g_analyze_beam_pedestal(ABORT,err)
      if(ABORT) then
         call G_add_path(here,err)
         return
      endif
*
      call h_analyze_pedestal(ABORT,err)
      if(ABORT) then
         call G_add_path(here,err)
         return
      endif
*
      call b_analyze_pedestal(ABORT,err) ! bigcal
      if(ABORT) then
         call G_add_path(here,err)
         return
      endif
*
      call sane_analyze_pedestal(ABORT,err) ! bigcal
      if(ABORT) then
         call G_add_path(here,err)
         return
      endif
      call sem_analyze_pedestal(ABORT,err) ! bigcal
      if(ABORT) then
         call G_add_path(here,err)
         return
      endif
*
      call s_analyze_pedestal(ABORT,err)
      if(ABORT) then
         call G_add_path(here,err)
         return
      endif
*
      return
      end
