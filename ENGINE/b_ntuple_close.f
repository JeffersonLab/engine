      subroutine b_ntuple_close(ABORT,err)

      implicit none
      save
      
      character*14 here
      parameter(here='b_ntuple_close')
      
      logical ABORT
      character*(*) err
      
      include 'b_ntuple.cmn'
      include 'gen_routines.dec'

      logical HEXIST ! CERNLIB function

      logical FAIL
      character*80 why,directory,name
      character*1000 msg
      integer io,id,cycle,m

      err=' '
      abort=.false.

      if(.not.b_ntuple_exists) return

      call HCDIR(directory,'R') ! keep current directory

      id=b_ntuple_ID
      io=b_ntuple_IOchannel
      name=b_ntuple_name

      abort=.not.HEXIST(id)

      if(abort) then
         call G_add_path(here,err)
         if(io.gt.0) then
            call G_IO_control(io,'FREE',FAIL,why) ! free up
            if(.not.fail) close(io)
         endif
      endif

      call HCDIR(b_ntuple_directory,' ') ! go to ntuple directory

      call G_add_path(here,msg)
      call G_log_message('INFO: '//msg)

      cycle= 0
      call HROUT(id,cycle,' ')
      call HREND(name)
      call G_IO_control(io,'FREE',ABORT,err)
      close(io)
      write(*,*)'B NTUPLE IO',io
      call HCDIR(directory,' ')     ! return to "current" directory

      b_ntuple_directory=' '
      b_ntuple_exists=.false.
      b_ntuple_IOchannel= 0
      
      if(abort) call G_add_path(here,err)

      return 
      end
      
