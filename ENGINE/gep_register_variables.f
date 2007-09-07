      subroutine gep_register_variables(ABORT,err)

      implicit none
      save
      
      character*22 here
      parameter (here='gep_register_variables')

      logical ABORT
      character*(*) err
      logical FAIL
      character*100 why

      err = ' '
      ABORT = .false.

      call r_gep_data_structures
      call r_gep_filenames
      call r_gep_ntuple

      if(abort)then
         call G_prepend(':unable to register',err)
      endif

      if(abort .or. err.ne.' ') call g_add_path(here,err)

      call gep_ntuple_register(FAIL,why)    ! remove this when ctp files fixed
      if(err.ne.' '.and.why.ne.' ') then
         call G_append(err,' & '//why)
      else if(why.ne.' ') then
         err=why
      endif
      abort = abort.or.fail

      return 
      end
