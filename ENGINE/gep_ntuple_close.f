      subroutine gep_ntuple_close(abort,err)

      implicit none
      save
      
      character*16 here
      parameter(here='gep_ntuple_close')

      logical abort
      character*(*) err
      
      include 'gep_ntuple.cmn'
      include 'gen_routines.dec'

      logical HEXIST ! cernlib function

      logical fail
      character*80 why,directory,name
      character*1000 msg
      integer io,id,cycle,m

      err=' '
      abort=.false.

      if(.not.gep_ntuple_exists) return

      call HCDIR(directory,'R') ! keep current directory

      id = gep_ntuple_ID
      io = gep_ntuple_IO_channel
      name = gep_ntuple_name

      abort = .not. HEXIST(id)

      if(abort) then
         call G_add_path(here,err)

         if(io.gt.0) then
            call G_IO_control(io,'FREE',FAIL,why) 
            if(.not.fail) close(io)
         endif
      endif

      call HCDIR(gep_ntuple_directory,' ') ! go to ntuple directory

      call G_add_path(here,msg)
      call G_log_message('INFO: '//msg)

      cycle = 0
      call HROUT(id,cycle,' ')
      call HREND(name)
      call G_IO_control(io,'FREE',ABORT,err)
      close(io)

      call HCDIR(directory,' ')    ! return to current directory

      gep_ntuple_directory=' '
      gep_ntuple_exists=.false.
      gep_ntuple_IO_channel = 0

      if(abort) call G_add_path(here,err)

      return
      end
