      subroutine f1t_register_variables(ABORT,err)
      implicit none
      save

      character*20 here
      parameter (here='f1t_register_variables')
      
      logical ABORT
      character*(*) err

      logical FAIL
      character*1000 why

      err= ' '
      ABORT= .false.

      call r_f1trigger_data_structures
     
*****************************************************************************
*****************************************************************************
c      if(err.ne.' '.and.why.ne.' ') then
c         call G_append(err,' & '//why)
c      else if(why.ne.' ') then
c         err=why
c      endif
      abort = abort.or.fail

      if(abort.or.err.ne.' ') call G_add_path(here,err)

      return
      end

*****************************************************************************
*
