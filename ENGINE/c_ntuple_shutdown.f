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
* Revision 1.1  1994/04/12 16:14:33  cdaq
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
      character*80 directory,name,msg
      integer io,id,cycle,iv(10)
      real rv(10)
*
*--------------------------------------------------------
      err= ' '
      ABORT = .FALSE.
*
      IF(.NOT.c_Ntuple_exists) RETURN       !nothing to do
*
      call HCDIR(directory,'R')                !keep current directory
      call G_log_message(here//' current directory='//directory)
*
      id= c_Ntuple_ID
      io= c_Ntuple_IOchannel
      name= c_Ntuple_name
      call HCDIR(c_Ntuple_directory,' ')      !goto Ntuple directory
*
      call G_log_message('directory='//c_Ntuple_directory)
      call G_log_message('name='//c_Ntuple_name)
      iv(1)= id
      iv(2)= io
      call G_build_note('ID#$, IO=$','$',iv,' ',rv,' ',msg)
      call G_log_message(msg)
*
      cycle= 0                                !dummy for HROUT
      call HROUT(id,cycle,' ')                !flush CERNLIB buffers
      call HREND(name)                        !CERNLIB close file
      call HDELET(id)                         !CERNLIB delete tuple
      call G_IO_control(io,'FREE',ABORT,err)  !free up IO channel
      CLOSE(io)                               !close channel
*
      call HCDIR(directory,' ')               !return to current directory
      c_Ntuple_exists= .FALSE.
*
      IF(ABORT) THEN
        call G_add_path(here,err)
        RETURN
      ENDIF
*
      RETURN
      END      
