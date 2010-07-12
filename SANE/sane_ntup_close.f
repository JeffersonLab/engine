      subroutine sane_ntup_close(ABORT,err)

      implicit none
      save
      
      character*14 here
      parameter(here='sane_ntup_close')
      
      logical ABORT
      character*(*) err
      
      include 'b_ntuple.cmn'
      include 'sane_ntuple.cmn'
      include 'gen_routines.dec'

      logical HEXIST ! CERNLIB function

      logical FAIL
      character*80 why,directory,name
      character*1000 msg
      integer io,id,cycle,m,i,itcol,itrow,icc

      err=' '
      abort=.false.

      if(.not.sane_ntuple_exists) return

      call HCDIR(directory,'R') ! keep current directory

      id=sane_ntuple_ID
      io=sane_ntuple_IOchannel
      name=sane_Ntuple_name

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

c      write(*,*) HEXIST(9502),sane_ntuple_directory
      cycle= 0
      call HROUT(id,cycle,' ')
c      call HROUT(10100,cycle,' ')
c      call HROUT(10101,cycle,' ')
c      call HROUT(10102,cycle,' ')
      call HROUT(10103,cycle,' ')
      call HROUT(10104,cycle,' ')
      call HROUT(10105,cycle,' ')
      call HROUT(10106,cycle,' ')
      call HROUT(10107,cycle,' ')
      call HROUT(10108,cycle,' ')
c      call HROUT(10109,cycle,' ')

c      call HROUT(10111,cycle,' ')
c      call HROUT(10112,cycle,' ')
c      call HROUT(10113,cycle,' ')
c      call HROUT(10114,cycle,' ')
      call HROUT(10121,cycle,' ')
      call HROUT(10122,cycle,' ')
c      call HROUT(10125,cycle,' ')
c      call HROUT(10126,cycle,' ')
      call HROUT(10128,cycle,' ')
      do i =1,18
       call HROUT(17100+i,cycle,' ')  
       call HROUT(17200+i,cycle,' ')  
       call HROUT(18100+i,cycle,' ')  
       call HROUT(18200+i,cycle,' ')  
      enddo
      do i =1,8
         call HROUT(10500+i,cycle,' ')
         call HROUT(10510+i,cycle,' ')
         call HROUT(10520+i,cycle,' ')
         call HROUT(10530+i,cycle,' ')
         call HROUT(10540+i,cycle,' ')
         call HROUT(10560+i,cycle,' ')
         call HROUT(10570+i,cycle,' ')
         call HROUT(10580+i,cycle,' ')
         call HROUT(10710+i,cycle,' ')
         call HROUT(10720+i,cycle,' ')
         call HROUT(10730+i,cycle,' ')
         call HROUT(10740+i,cycle,' ')

      enddo
c      if ( sane_ntuple_type .eq. 1) then
         do i =1,28
c            call HROUT(10150+i,cycle,' ')
c            call HROUT(20150+i,cycle,' ')
c            call HROUT(20250+i,cycle,' ')
        enddo
c      endif
      call HROUT(10200,cycle,' ')
      do i =0,6
         call HROUT(10210+i,cycle,' ')

      enddo

*      call HROUT(10300,cycle,' ')
*      call HROUT(10301,cycle,' ')
*      call HROUT(10302,cycle,' ')
*      call HROUT(10303,cycle,' ')
*      call HROUT(10304,cycle,' ')
*      call HROUT(10310,cycle,' ')
*      call HROUT(10311,cycle,' ')
*      call HROUT(10312,cycle,' ')
*      call HROUT(10313,cycle,' ')
*      call HROUT(10314,cycle,' ')
*      call HROUT(10315,cycle,' ')
*      call HROUT(10316,cycle,' ')
*      call HROUT(10317,cycle,' ')
*      call HROUT(10321,cycle,' ')
*      call HROUT(10322,cycle,' ')
*      call HROUT(10323,cycle,' ')
*      call HROUT(10324,cycle,' ')



c      call HROUT(10550,cycle,' ')
c      call HROUT(10551,cycle,' ')

c      call HROUT(10601,cycle,' ')
c      call HROUT(10611,cycle,' ')
c      call HROUT(10602,cycle,' ')
c      call HROUT(10612,cycle,' ')
c      call HROUT(10603,cycle,' ')
c      call HROUT(10613,cycle,' ')
c      call HROUT(10604,cycle,' ')
c      call HROUT(10614,cycle,' ')
c      call HROUT(10620,cycle,' ')
c      call HROUT(10621,cycle,' ')
      call HROUT(10622,cycle,' ')
      call HROUT(10623,cycle,' ')

c      call HPRINT(9502)
 
      write(*,*)name!,10623
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
      
