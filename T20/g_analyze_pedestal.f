      subroutine g_analyze_pedestal(ABORT,err)
*
* $Log$
* Revision 1.1  1998/12/01 21:01:12  saw
* Initial revision
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
      call t_analyze_pedestal(ABORT,err)
      if(ABORT) then
         call G_add_path(here,err)
         return
      endif
*
      return
      end
