      subroutine g_calc_pedestal(ABORT,err)
*
* $Log$
* Revision 1.1  1995/04/01 19:37:06  cdaq
* Initial revision
*
*
      implicit none
*
      character*18 here
      parameter (here='g_calc_pedestal')
*
      logical ABORT
      character*(*) err
*
      call h_calc_pedestal(ABORT,err)
      if(ABORT) then
         call G_add_path(here,err)
         return
      endif
*
      call s_calc_pedestal(ABORT,err)
      if(ABORT) then
         call G_add_path(here,err)
         return
      endif
*
      return
      end
