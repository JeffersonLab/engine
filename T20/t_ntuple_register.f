      subroutine t_Ntuple_register(ABORT,err)
*----------------------------------------------------------------------
*
*     CTP variable registration routine for the SOS Ntuples 
*
*     Purpose : Register output filename for SOS Ntuple; temporary
*     implementation to be superceeded by CTP Ntuples
*
*     Output: ABORT      - success or failure
*           : err        - reason for failure, if any
*
*     Created: 8-Apr-1994  K.B.Beard, HU: added Ntuples
* $Log$
* Revision 1.1  1998/12/01 20:56:56  saw
* Initial revision
*
*----------------------------------------------------------------------
      implicit none
      save
*
      character*17 here
      parameter (here='t_Ntuple_register')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 't_ntuple.cmn'
      INCLUDE 'gen_routines.dec'
*
      integer ierr
*--------------------------------------------------------
      err= ' '
      ABORT = .FALSE.
*
      call G_reg_C('T20_Ntuple',t_Ntuple_file,ABORT,err)
*
      IF(ABORT) THEN
        call G_prepend(':unable to register-',err)
        call G_add_path(here,err)
      ENDIF
*
      return
      end
