      SUBROUTINE H_apply_offsets(ABORT,err)
*--------------------------------------------------------
*-       Prototype C analysis routine
*-
*-
*-   Purpose and Methods : applies offsets to HMS
*-   central momentum and central angle.
*-
*- 
*-   Output: ABORT		- success or failure
*-         : err	- reason for failure, if any
*- 
*-   Created  31-Aug-1999 Chris Armstrong
*- 
*-
*- All standards are from "Proposal for Hall C Analysis Software
*- Vade Mecum, Draft 1.0" by D.F.Geesamn and S.Wood, 7 May 1993
*-
*-
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*15 here
      parameter (here= 'H_apply_offsets')

      logical ABORT
      character*(*) err
*
      INCLUDE 'hms_data_structures.cmn'
      INCLUDE 'hms_physics_sing.cmn'
      INCLUDE 'gen_constants.par'
*
*--------------------------------------------------------
*
 
* csa 8/31/99 -- We really should be filling *new* variables
* here!
      if (hphicentral_offset .ne. 0 )
     > write(6,*)' h_apply_offs: before:   hphi_lab =',hphi_lab
      hphi_lab = hphi_lab + hphicentral_offset/degree
      if (hphicentral_offset .ne. 0 )
     > write(6,*)' h_apply_offs:  after:   hphi_lab =',hphi_lab

      if (hpcentral_offset .ne. 0 )
     > write(6,*)' h_apply_offs: before:  hpcentral =',hpcentral
      hpcentral = hpcentral * ( 1. + hpcentral_offset / 100. )
      if (hpcentral_offset .ne. 0 )
     > write(6,*)' h_apply_offs:  after:  hpcentral =',hpcentral
      if (hmomentum_factor .gt. 0.1) then   !avoid setting p=0
        hpcentral = hpcentral * hmomentum_factor
      endif
      if (hmomentum_factor .gt. 0.1 )
     > write(6,*)' h_apply_offs: after2:  hpcentral =',hpcentral

      if (hthetacentral_offset .ne. 0 )
     > write(6,*)' h_apply_offs: before: htheta_lab =',htheta_lab
      htheta_lab=htheta_lab + hthetacentral_offset/degree
      if (hthetacentral_offset .ne. 0 )
     > write(6,*)' h_apply_offs:  after: htheta_lab =',htheta_lab

* csa 8/31/99 -- moved to h_physics  no says beni
      coshthetas = cos(htheta_lab*degree)
      sinhthetas = sin(htheta_lab*degree)

      ABORT= .FALSE.
      err= ' '
      RETURN
      END
