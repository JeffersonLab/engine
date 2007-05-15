      subroutine b_register_variables(ABORT,err)

      implicit none
      save

      character*20 here
      parameter (here='b_register_variables')
      
      logical ABORT
      character*(*) err
*
      logical FAIL
      character*1000 why

      err= ' '
      ABORT= .false.

      call r_bigcal_data_structures

      call r_bigcal_filenames

      call r_b_ntuple

      call b_register_param(FAIL,why) ! reconstruction variables

      IF(err.NE.' ' .and. why.NE.' ') THEN   !keep warnings
        call G_append(err,' & '//why)
      ELSEIF(why.NE.' ') THEN
        err= why
      ENDIF

      ABORT= ABORT .or. FAIL

      IF(ABORT .or. err.NE.' ') call G_add_path(here,err)

      call b_ntuple_register(FAIL,why)    ! remove this when ctp files fixed
      if(err.ne.' '.and.why.ne.' ') then
         call G_append(err,' & '//why)
      else if(why.ne.' ') then
         err=why
      endif
      abort = abort.or.fail

      if(abort.or.err.ne.' ') call G_add_path(here,err)

      return
      end
