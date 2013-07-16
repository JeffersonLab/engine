      subroutine h_sv_Nt_shutdown(ABORT,err)
*----------------------------------------------------------------------
*
*     Final shutdown of the HMS Sieve Slit Ntuple
*
*     Purpose : Flushes and closes the HMS Sieve slit Ntuple
*
*     Output: ABORT      - success or failure
*           : err        - reason for failure, if any
*
*     Created: 1-Nov-1994   added Ntuples
* $Log: h_sv_nt_shutdown.f,v $
* Revision 1.2  2003/02/13 15:08:20  jones
* subroutine call G_build_note had 6 instead needed 7 arguments
*
* Revision 1.1  1995/01/27 20:06:10  cdaq
* Initial revision
*
*----------------------------------------------------------------------
      implicit none
      save
*
      character*17 here
      parameter (here='h_sv_Nt_shutdown')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 'h_sieve_ntuple.cmn'
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
      IF(.NOT.h_sieve_Ntuple_exists) RETURN       !nothing to do
*
      call HCDIR(directory,'R')                !keep current directory

      id= h_sieve_Ntuple_ID
      io= h_sieve_Ntuple_IOchannel
*
      ABORT= .NOT.HEXIST(id)
      IF(ABORT) THEN
        pat= ': Ntuple ID#$ does not exist'
        call G_build_note(pat,'$',id,' ',0.,' ',err)
        call G_add_path(here,err)
        If(io.GT.0) Then
          call G_IO_control(io,'FREE',FAIL,why) !free up
          if(.NOT.FAIL) CLOSE(io)
        EndIf
        h_sieve_Ntuple_exists= .FALSE.
        h_sieve_Ntuple_ID= 0
        h_sieve_Ntuple_name= ' '
        h_sieve_Ntuple_IOchannel= 0
        h_sieve_Ntuple_file= ' '
        h_sieve_Ntuple_title= ' '
        h_sieve_Ntuple_directory= ' '
        h_sieve_Ntuple_size= 0
        do m=1,HMAX_Ntuple_size
          h_sieve_Ntuple_tag(m)= ' '
          h_sieve_Ntuple_contents(m)= 0.
        enddo
        RETURN
      ENDIF
*

      id= h_sieve_Ntuple_ID
      io= h_sieve_Ntuple_IOchannel
      name= h_sieve_Ntuple_name
      call HCDIR(h_sieve_Ntuple_directory,' ')      !goto Ntuple directory
*
      iv(1)= id
      iv(2)= io
      pat= 'closing ID#$ IO#$ "'//h_sieve_Ntuple_file//'"'

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
      h_sieve_Ntuple_exists= .FALSE.
      h_sieve_Ntuple_ID= 0
      h_sieve_Ntuple_name= ' '
      h_sieve_Ntuple_IOchannel= 0
      h_sieve_Ntuple_file= ' '
      h_sieve_Ntuple_title= ' '
      h_sieve_Ntuple_directory= ' '
      h_sieve_Ntuple_size= 0
      do m=1,HMAX_Ntuple_size
        h_sieve_Ntuple_tag(m)= ' '
        h_sieve_Ntuple_contents(m)= 0.
      enddo
*
      IF(ABORT) call G_add_path(here,err)
*
      RETURN
      END      
