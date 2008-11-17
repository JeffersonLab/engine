 2    subroutine sane_ntup_close(ABORT,err)

      implicit none
      save
      
      character*14 here
      parameter(here='sane_ntuple_close')
      
      logical ABORT
      character*(*) err
      
      include 'b_ntuple.cmn'
      include 'sane_ntuple.cmn'
      include 'gen_routines.dec'

      logical HEXIST ! CERNLIB function

      logical FAIL
      character*80 why,directory,name
      character*1000 msg
      integer io,id,cycle,m,i

      err=' '
      abort=.false.

      if(.not.sane_ntuple_exists) return

      call HCDIR(directory,'R') ! keep current directory

      id=sane_ntuple_ID
      io=sane_ntuple_IOchannel
      name=sane_ntuple_name

      abort=.not.HEXIST(id)

      if(abort) then
         call G_add_path(here,err)
         if(io.gt.0) then
            call G_IO_control(io,'FREE',FAIL,why) ! free up
            if(.not.fail) close(io)
         endif
      endif

      call HCDIR(sane_ntuple_directory,' ') ! go to ntuple directory

      call G_add_path(here,msg)
      call G_log_message('INFO: '//msg)

      cycle= 0
      call HROUT(id,cycle,' ')
      call HROUT(10100,cycle,' ')
      call HROUT(10101,cycle,' ')
      call HROUT(10102,cycle,' ')
      call HROUT(10111,cycle,' ')
      call HROUT(10112,cycle,' ')
      call HROUT(10121,cycle,' ')
      call HROUT(10122,cycle,' ')
      call HROUT(10125,cycle,' ')
      call HROUT(10126,cycle,' ')
      call HROUT(10200,cycle,' ')
      do i =1,8
         call HROUT(10500+i,cycle,' ')

      enddo
      call HROUT(10210,cycle,' ')
      call HROUT(10211,cycle,' ')
      call HROUT(10212,cycle,' ')
      call HROUT(10213,cycle,' ')

      call HROUT(10300,cycle,' ')
      call HROUT(10301,cycle,' ')
      call HROUT(10302,cycle,' ')
      call HROUT(10303,cycle,' ')
      call HROUT(10304,cycle,' ')


      call HROUT(10550,cycle,' ')
      call HROUT(10551,cycle,' ')

      call HROUT(10601,cycle,' ')
      call HROUT(10611,cycle,' ')
      call HROUT(10602,cycle,' ')
      call HROUT(10612,cycle,' ')
      call HROUT(10603,cycle,' ')
      call HROUT(10613,cycle,' ')
      call HROUT(10604,cycle,' ')
      call HROUT(10614,cycle,' ')
      call HROUT(10620,cycle,' ')
      call HROUT(10621,cycle,' ')
        
      call HREND(name)
      call G_IO_control(io,'FREE',ABORT,err)
      close(io)
      
      call HCDIR(directory,' ')     ! return to "current" directory

      sane_ntuple_directory=' '
      sane_ntuple_exists=.false.
      sane_ntuple_IOchannel= 0
      
      if(abort) call G_add_path(here,err)

      return 
      end
      
