      subroutine c_Ntuple_register(ABORT,err)
*----------------------------------------------------------------------
*
*     CTP variable registration routine for the COIN Ntuples 
*
*     Purpose : Register output filename for COIN Ntuple; temporary
*     implementation to be superceeded by CTP Ntuples
*
*     Output: ABORT      - success or failure
*           : err        - reason for failure, if any
*
*     Created: 8-Apr-1994  K.B.Beard, HU: added Ntuples
* $Log$
* Revision 1.2  1994/06/17 02:47:58  cdaq
* (KBB) Upgrade
*
* Revision 1.1  1994/04/12  16:12:59  cdaq
* Initial revision
*
*
*----------------------------------------------------------------------
      implicit none
      save
*
      character*17 here
      parameter (here='c_Ntuple_register')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 'c_ntuple.cmn'
      INCLUDE 'gen_routines.dec'
*
      integer ierr
*--------------------------------------------------------
      err= ' '
      ABORT = .FALSE.
*
      call G_reg_C('COIN_Ntuple',c_Ntuple_file,ABORT,err)
*
      IF(ABORT) THEN
        call G_prepend(':unable to register-',err)
        call G_add_path(here,err)
      ENDIF
*
      return
      end
