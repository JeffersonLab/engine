      subroutine h_Ntuple_shutdown(ABORT,err)
*----------------------------------------------------------------------
*
*     Final shutdown of the HMS Ntuple
*
*     Purpose : Flushes and closes the HMS Ntuple
*
*     Output: ABORT      - success or failure
*           : err        - reason for failure, if any
*
*     Created: 8-Apr-1994  K.B.Beard, HU: added Ntuples
* $Log$
* Revision 1.3  1994/06/29 03:27:43  cdaq
* (KBB) Remove HDELET call
*
* Revision 1.2  1994/06/17  02:59:12  cdaq
* (KBB) Upgrade
*
* Revision 1.1  1994/04/12  16:15:43  cdaq
* Initial revision
*
*
*----------------------------------------------------------------------
      implicit none
      save
*
      character*17 here
      parameter (here='h_Ntuple_shutdown')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 'h_ntuple.cmn'
      INCLUDE 'gen_routines.dec'
*
      logical HEXIST    !CERNLIB function
*
      logical FAIL
      character*80 why,directory,name
      character*1000 pat,msg
      integer io,id,cycle,m,iv(10)
*
*--------------------------------------------------------
      err= ' '
      ABORT = .FALSE.
*
      IF(.NOT.h_Ntuple_exists) RETURN       !nothing to do
*
      call HCDIR(directory,'R')                !keep current directory
*
      id= h_Ntuple_ID
      io= h_Ntuple_IOchannel
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
         h_Ntuple_exists= .FALSE.
         h_Ntuple_ID= 0
         h_Ntuple_name= ' '
         h_Ntuple_IOchannel= 0
         h_Ntuple_file= ' '
         h_Ntuple_title= ' '
         h_Ntuple_directory= ' '
         h_Ntuple_size= 0
         do m=1,HMAX_Ntuple_size
           h_Ntuple_tag(m)= ' '
           h_Ntuple_contents(m)= 0.
         enddo
         RETURN
      ENDIF
*
      id= h_Ntuple_ID
      io= h_Ntuple_IOchannel
      name= h_Ntuple_name
      call HCDIR(h_Ntuple_directory,' ')      !goto Ntuple directory
*
      iv(1)= id
      iv(2)= io
      pat= 'closing ID#$ IO#$ "'//h_Ntuple_file//'"'
      call G_build_note(pat,'$',iv,' ',0.,' ',msg)
      call G_add_path(here,msg)
      call G_log_message('INFO: '//msg)
*
      cycle= 0                                !dummy for HROUT
      call HROUT(id,cycle,' ')                !flush CERNLIB buffers
      call HREND(name)                        !CERNLIB close file
*      call HDELET(id)                         !CERNLIB delete tuple
      call G_IO_control(io,'FREE',ABORT,err)  !free up IO channel
      CLOSE(io)                               !close IO channel
*
      call HCDIR(directory,' ')               !return to current directory
*
      h_Ntuple_exists= .FALSE.
      h_Ntuple_ID= 0
      h_Ntuple_name= ' '
      h_Ntuple_IOchannel= 0
      h_Ntuple_file= ' '
      h_Ntuple_title= ' '
      h_Ntuple_directory= ' '
      h_Ntuple_size= 0
      do m=1,HMAX_Ntuple_size
        h_Ntuple_tag(m)= ' '
        h_Ntuple_contents(m)= 0.
      enddo
*
      IF(ABORT) call G_add_path(here,err)
*
      RETURN
      END      
