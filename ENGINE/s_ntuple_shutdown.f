      subroutine s_Ntuple_shutdown(ABORT,err)
*----------------------------------------------------------------------
*
*     Final shutdown of the SOS Ntuple
*
*     Purpose : Flushes and closes the SOS Ntuple
*
*     Output: ABORT      - success or failure
*           : err        - reason for failure, if any
*
*     Created: 8-Apr-1994  K.B.Beard, HU: added Ntuples
* $Log$
* Revision 1.1  1994/04/12 16:16:53  cdaq
* Initial revision
*
*
*----------------------------------------------------------------------
      implicit none
      save
*
      character*17 here
      parameter (here='s_Ntuple_shutdown')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 's_ntuple.cmn'
      INCLUDE 'gen_routines.dec'
*
      character*80 directory,name
      integer io,id,cycle
*
*--------------------------------------------------------
      err= ' '
      ABORT = .FALSE.
*
      IF(.NOT.s_Ntuple_exists) RETURN       !nothing to do
*
      call HCDIR(directory,'R')                !keep current directory
*
      id= s_Ntuple_ID
      io= s_Ntuple_IOchannel
      name= s_Ntuple_name
      call HCDIR(s_Ntuple_directory,' ')      !goto Ntuple directory
*
      cycle= 0                                !dummy for HROUT
      call HROUT(id,cycle,' ')                !flush CERNLIB buffers
      call HREND(name)                        !CERNLIB close file
      call HDELET(id)                         !CERNLIB delete tuple
      call G_IO_control(io,'FREE',ABORT,err)  !free up IO channel
      CLOSE(io)                               !close IO channel
*
      call HCDIR(directory,' ')               !return to current directory
      s_Ntuple_exists= .FALSE.
*
      IF(ABORT) THEN
        call G_add_path(here,err)
        RETURN
      ENDIF
*
      RETURN
      END      
