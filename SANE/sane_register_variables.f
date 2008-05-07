      subroutine sane_register_variables(ABORT,err)
      implicit none
      save

      character*20 here
      parameter (here='sane_register_variables')
      
      logical ABORT
      character*(*) err

      logical FAIL
      character*1000 why

      err= ' '
      ABORT= .false.

      call r_sane_data_structures

      call r_sane_filenames

      call r_sane_ntuple
     
*****************************************************************************
*****************************************************************************

      call sane_ntup_register(FAIL,why)    ! remove this when ctp files fixed
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

