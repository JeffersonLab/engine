      subroutine b_initialize(ABORT,err)

      implicit none
      save

      character*12 here
      parameter(here='b_initialize')
      
      logical ABORT
      character*(*) err
      character*20 mss
      integer*4 istat

      logical fail
      character*1000 why

      err = ' '
      abort = .false.
      
c     "calculate physics singles constants"
      call b_init_physics(FAIL,why)
      if(err.ne.' '.and.why.ne.' ') then
         call G_append(err,' & '//why)
      else if(why.ne.' ') then
         err = why
      endif
      abort = abort .or. fail

      call b_generate_geometry ! initialize geometry constants

      call b_init_tof(FAIL,why) ! initialize timing constants
      if(why.ne.' '.and.err.ne.' ') then
         call G_append(err,' & '//why)
      else if(why.ne.' ') then
         err = why
      endif
      abort=abort.or.fail

      call b_init_gain(FAIL,why) ! initialize calibration constants
      if(why.ne.' '.and.err.ne.' ') then
         call G_append(err,' & '//why)
      else if(why.ne.' ') then
         err = why
      endif
      abort=abort.or.fail

      call b_init_shower(FAIL,why) ! initialize shower reconstruction parms
      if(why.ne.' '.and.err.ne.' ') then
         call G_append(err,' & '//why)
      else if(why.ne.' ') then
         err = why
      endif
      abort=abort.or.fail

      if(abort .or. err.ne.' ') call g_add_path(here,err)

      return 
      end
