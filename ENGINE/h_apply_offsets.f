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
 
      if (h_oopcentral_offset .ne. 0 ) then
        write(*,*) ' ******'
       write(6,*)' h_apply_offs: h_oopcentral_offset =',h_oopcentral_offset,' rad'
       write(6,*)'  Used to offset hsxp_tar in h_physics.f  '
      endif
c
      if (hpcentral_offset .ne. 0 ) then
        write(*,*) ' ******'
       write(6,*)' h_apply_offs: apply hpcentral_offset(%)  =',hpcentral_offset
       write(6,*)' h_apply_offs: before:  hpcentral =',hpcentral
       hpcentral = hpcentral * ( 1. + hpcentral_offset / 100. )
       write(6,*)' h_apply_offs:  after:  hpcentral =',hpcentral
      endif
c
      if ( hmomentum_factor .gt. 0.1 ) then   
        write(*,*) ' ******'
        write(6,*)' h_apply_offs: apply hmomentum_factor  =',hmomentum_factor
        write(6,*)' h_apply_offs: before :  hpcentral =',hpcentral
        hpcentral = hpcentral * hmomentum_factor
        write(6,*)' h_apply_offs: after :  hpcentral =',hpcentral
      endif
c
      if (hthetacentral_offset .ne. 0 ) then
        write(*,*) ' ******'
       write(6,*)' h_apply_offs: before: htheta_lab =',htheta_lab
       htheta_lab=htheta_lab + hthetacentral_offset/degree
       write(6,*)' h_apply_offs:  after: htheta_lab =',htheta_lab
       coshthetas = cos(htheta_lab*degree)
       sinhthetas = sin(htheta_lab*degree)
      endif
c

      ABORT= .FALSE.
      err= ' '
      RETURN
      END
