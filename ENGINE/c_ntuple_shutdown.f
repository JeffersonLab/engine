      subroutine c_Ntuple_shutdown(ABORT,err)
*----------------------------------------------------------------------
*
*     Final shutdown of the COIN Ntuple
*
*     Purpose : Flushes and closes the COIN Ntuple
*
*     Output: ABORT      - success or failure
*           : err        - reason for failure, if any
*
*     Created: 8-Apr-1994  K.B.Beard, HU: added Ntuples
* $Log$
* Revision 1.2  1994/06/17 03:00:30  cdaq
* (KBB) Upgrade
*
* Revision 1.1  1994/04/12  16:14:33  cdaq
* Initial revision
*
*
*----------------------------------------------------------------------
      implicit none
      save
*
      character*17 here
      parameter (here='c_Ntuple_shutdown')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 'c_ntuple.cmn'
      INCLUDE 'gen_routines.dec'
*
      logical FAIL
      character*80 why,directory,name
      character*1000 msg,pat
      integer io,id,cycle,iv(10),m
*
      logical HEXIST      !CERNLIB function
*
*--------------------------------------------------------
      err= ' '
      ABORT = .FALSE.
*
      IF(.NOT.c_Ntuple_exists) RETURN       !nothing to do
*
      call HCDIR(directory,'R')                !keep current directory
*
      id= c_Ntuple_ID
      io= c_Ntuple_IOchannel
*
      ABORT= .NOT.HEXIST(id)
      IF(ABORT) THEN
         pat= ': Ntuple ID#$ does not exist'
         call G_build_note(pat,'$',id,' ',0.,err)
         call G_add_path(here,err)
         If(io.GT.0) Then
           call G_IO_control(io,'FREE',FAIL,why) !free up
           if(.NOT.FAIL) CLOSE(io)
         EndIf
         c_Ntuple_exists= .FALSE.
         c_Ntuple_ID= 0
         c_Ntuple_name= ' '
         c_Ntuple_IOchannel= 0
         c_Ntuple_file= ' '
         c_Ntuple_title= ' '
         c_Ntuple_directory= ' '
         c_Ntuple_size= 0
         do m=1,CMAX_Ntuple_size
           c_Ntuple_tag(m)= ' '
           c_Ntuple_contents(m)= 0.
         enddo
         RETURN
      ENDIF
*
      id= c_Ntuple_ID
      io= c_Ntuple_IOchannel
      name= c_Ntuple_name
      call HCDIR(c_Ntuple_directory,' ')      !goto Ntuple directory
*
      iv(1)= id
      iv(2)= io
      pat= 'closing ID#$ IO#$ "'//c_Ntuple_file//'"'
      call G_build_note(pat,'$',iv,' ',0.,' ',msg)
      call G_add_path(here,msg)
      call G_log_message('INFO: '//msg)
*
      cycle= 0                                !dummy for HROUT
      call HROUT(id,cycle,' ')                !flush CERNLIB buffers
      call HREND(name)                        !CERNLIB close file
      call HDELET(id)                         !CERNLIB delete tuple
      call G_IO_control(io,'FREE',ABORT,err)  !free up IO channel
      CLOSE(io)                               !close channel
*
      call HCDIR(directory,' ')               !return to current directory
*
      c_Ntuple_exists= .FALSE.
      c_Ntuple_ID= 0
      c_Ntuple_name= ' '
      c_Ntuple_IOchannel= 0
      c_Ntuple_file= ' '
      c_Ntuple_title= ' '
      c_Ntuple_directory= ' '
      c_Ntuple_size= 0
      do m=1,CMAX_Ntuple_size
        c_Ntuple_tag(m)= ' '
        c_Ntuple_contents(m)= 0.
      enddo
*
      IF(ABORT) call G_add_path(here,err)
*
      RETURN
      END      
