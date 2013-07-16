      subroutine s_sv_Nt_shutdown(ABORT,err)
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
* $Log: s_sv_nt_shutdown.f,v $
* Revision 1.2  2003/02/12 16:03:21  jones
* Modified Call G_build_note to have the needed 7 variables instead of 6
*
* Revision 1.1  1995/08/11 16:23:18  cdaq
* Initial revision
*
*----------------------------------------------------------------------
      implicit none
      save
*
      character*17 here
      parameter (here='s_sv_Nt_shutdown')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 's_sieve_ntuple.cmn'
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
      IF(.NOT.s_sieve_Ntuple_exists) RETURN       !nothing to do
*
      call HCDIR(directory,'R')                !keep current directory

      id= s_sieve_Ntuple_ID
      io= s_sieve_Ntuple_IOchannel
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
        s_sieve_Ntuple_exists= .FALSE.
        s_sieve_Ntuple_ID= 0
        s_sieve_Ntuple_name= ' '
        s_sieve_Ntuple_IOchannel= 0
        s_sieve_Ntuple_file= ' '
        s_sieve_Ntuple_title= ' '
        s_sieve_Ntuple_directory= ' '
        s_sieve_Ntuple_size= 0
        do m=1,SMAX_sv_Ntuple_size
          s_sieve_Ntuple_tag(m)= ' '
          s_sieve_Ntuple_contents(m)= 0.
        enddo
        RETURN
      ENDIF
*

      id= s_sieve_Ntuple_ID
      io= s_sieve_Ntuple_IOchannel
      name= s_sieve_Ntuple_name
      call HCDIR(s_sieve_Ntuple_directory,' ')      !goto Ntuple directory
*
      iv(1)= id
      iv(2)= io
      pat= 'closing ID#$ IO#$ "'//s_sieve_Ntuple_file//'"'

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
      s_sieve_Ntuple_exists= .FALSE.
      s_sieve_Ntuple_ID= 0
      s_sieve_Ntuple_name= ' '
      s_sieve_Ntuple_IOchannel= 0
      s_sieve_Ntuple_file= ' '
      s_sieve_Ntuple_title= ' '
      s_sieve_Ntuple_directory= ' '
      s_sieve_Ntuple_size= 0
      do m=1,SMAX_sv_Ntuple_size
        s_sieve_Ntuple_tag(m)= ' '
        s_sieve_Ntuple_contents(m)= 0.
      enddo
*
      IF(ABORT) call G_add_path(here,err)
*
      RETURN
      END      
