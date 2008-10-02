      subroutine f1trigger_register_variables(ABORT,err)
      implicit none
      save

      character*20 here
      parameter (here='f1trigger_register_variables')
      
      logical ABORT
      character*(*) err

      logical FAIL
      character*1000 why

      err= ' '
      ABORT= .false.

      call r_f1trigger_data_structures
     
*****************************************************************************
*****************************************************************************

      abort = abort.or.fail

      if(abort.or.err.ne.' ') call G_add_path(here,err)

      return
      end

*****************************************************************************
*
