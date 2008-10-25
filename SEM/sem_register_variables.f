      subroutine sem_register_variables(ABORT,err)
      implicit none
      save

      character*20 here
      parameter (here='sem_register_variables')
      
      logical ABORT
      character*(*) err

      logical FAIL
      character*1000 why

      err= ' '
      ABORT= .false.

      call r_sem_data_structures

     
*****************************************************************************
*****************************************************************************


      if(err.ne.' '.and.why.ne.' ') then
         call G_append(err,' & '//why)
      else if(why.ne.' ') then
         err=why
      endif
      abort = abort.or.fail

      if(abort.or.err.ne.' ') call G_add_path(here,err)

      return
      end

*****************************************************************************
*****************************************************************************

