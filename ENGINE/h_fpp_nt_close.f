      subroutine h_fpp_nt_close(ABORT,err)
*----------------------------------------------------------------------
*
*     closes the HMS FPP Ntuple file
*
*     Purpose : Flushes and closes the HMS FPP Ntuple
*
*     Output: ABORT      - success or failure
*           : err        - reason for failure, if any
*
*----------------------------------------------------------------------

      implicit none
      save

      character*14 here
      parameter (here='h_fpp_nt_close')

      logical ABORT
      character*(*) err
*
      INCLUDE 'h_fpp_ntuple.cmn'
      INCLUDE 'gen_routines.dec'
*
      logical HEXIST    !CERNLIB function
*
      logical FAIL
      character*80 why,directory,name
      character*1000 msg
      integer io,id,cycle,m
*
*--------------------------------------------------------

      err= ' '
      ABORT = .FALSE.

      IF(.NOT.h_fpp_nt_exists) RETURN       !nothing to do

      call HCDIR(directory,'R')                !keep current directory

      id= h_fpp_nt_ID
      io= h_fpp_nt_IOchannel
      name= h_fpp_nt_name

      ABORT= .NOT.HEXIST(id)
      IF(ABORT) THEN
        call G_add_path(here,err)
        If(io.GT.0) Then
          call G_IO_control(io,'FREE',FAIL,why) !free up
          if(.NOT.FAIL) CLOSE(io)
        EndIf
        h_fpp_nt_exists= .FALSE.
        h_fpp_nt_IOchannel= 0
        RETURN
      ENDIF

      call HCDIR(h_fpp_nt_directory,' ')      !goto Ntuple directory

      call G_add_path(here,msg)
      call G_log_message('INFO: '//msg)

      cycle= 0                                !dummy for HROUT
      call HROUT(id,cycle,' ')                !flush CERNLIB buffers
      call HREND(name)                        !CERNLIB close file
      call G_IO_control(io,'FREE',ABORT,err)  !free up IO channel
      CLOSE(io)                               !close IO channel

      call HCDIR(directory,' ')               !return to current directory

      h_fpp_nt_directory= ' '
      h_fpp_nt_exists= .FALSE.
      h_fpp_nt_IOchannel= 0

      IF(ABORT) call G_add_path(here,err)

      RETURN
      END      
