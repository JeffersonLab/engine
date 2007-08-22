      subroutine h_fpp_nt_register(ABORT,err)
*----------------------------------------------------------------------
*
*     CTP variable registration routine for the HMS FPP Ntuples 
*
*     Purpose : Register output filename for HMS FPP Ntuple; temporary
*     implementation to be superceeded by CTP Ntuples
*
*     Output: ABORT      - success or failure
*           : err        - reason for failure, if any
*
*----------------------------------------------------------------------
      implicit none
      save
*
      character*17 here
      parameter (here='h_fpp_nt_register')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 'h_fpp_ntuple.cmn'
      INCLUDE 'gen_routines.dec'
*
      integer ierr
*--------------------------------------------------------
      err= ' '
      ABORT = .FALSE.
*
      call G_reg_C('HMS_FPP_Ntuple',h_fpp_nt_file,ABORT,err)
*
      IF(ABORT) THEN
        call G_prepend(':unable to register-',err)
        call G_add_path(here,err)
      ENDIF
*
      return
      end
