      subroutine s_sv_Nt_register(ABORT,err)
*----------------------------------------------------------------------
*
*     CTP variable registration routine for the HMS Sieve Slit Ntuples 
*
*     Purpose : Register output filename for HMS Sieve slit Ntuple; temporary
*     implementation to be superceeded by CTP Ntuples
*
*     Output: ABORT      - success or failure
*           : err        - reason for failure, if any
*
*     Created: 1-Nov-1994 : added Ntuples
* $Log: s_sv_nt_register.f,v $
* Revision 1.1  1995/08/11 16:23:06  cdaq
* Initial revision
*
*----------------------------------------------------------------------
      implicit none
      save
*
      character*17 here
      parameter (here='s_sv_Nt_register')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 's_sieve_ntuple.cmn'
      INCLUDE 'gen_routines.dec'
*
      integer ierr
*--------------------------------------------------------
      err= ' '
      ABORT = .FALSE.
*
      call G_reg_C('SOS_sieve_Ntuple',s_sieve_Ntuple_file,ABORT,err)
*
      IF(ABORT) THEN
        call G_prepend(':unable to register-',err)
        call G_add_path(here,err)
      ENDIF
*
      return
      end
