      SUBROUTINE S_apply_offsets(ABORT,err)
*--------------------------------------------------------
*-       Prototype C analysis routine
*-
*-
*-   Purpose and Methods : applies offsets to SOS
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
      parameter (here= 'S_apply_offsets')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 'sos_data_structures.cmn'
      INCLUDE 'sos_physics_sing.cmn'
      INCLUDE 'gen_constants.par'
*
*     local variables 
      real*8 sosp0corr
*--------------------------------------------------------
*

 
* csa 8/31/99 -- We really should be filling *new* variables
* here!

      if (sphicentral_offset .ne. 0 )
     > write(6,*)' s_apply_offs: before:   sphi_lab =',sphi_lab
      sphi_lab = sphi_lab + sphicentral_offset/degree
      if (sphicentral_offset .ne. 0 )
     > write(6,*)' s_apply_offs:  after:   sphi_lab =',sphi_lab

      if (spcentral_offset .ne. 0 )
     > write(6,*)' s_apply_offs: before:  spcentral =',spcentral
      spcentral = spcentral * ( 1. + spcentral_offset / 100. )
      if (spcentral_offset .ne. 0 )
     > write(6,*)' s_apply_offs:  after:  spcentral =',spcentral
      if (smomentum_factor .gt. 0.1) then    !avoid setting p=0
        sosp0corr=0.45
        if (spcentral .gt. 0.51) sosp0corr=0.496-0.08845*spcentral
     >       -5.743e-4*exp(2.341*(spcentral**2.156))
*        sosp0corr = .225

*        if (spcentral.gt.1.483) sosp0corr=sosp0corr-16.7*(spcentral-1.483)**2
        spcentral = spcentral*smomentum_factor*(1+sosp0corr/100.)
      endif
      if (smomentum_factor .gt. 0.1)
     > write(6,*)' s_apply_offs: after2:  spcentral =',spcentral
*
      if (sthetacentral_offset .ne. 0 )
     > write(6,*)' s_apply_offs: before: stheta_lab =',stheta_lab
      stheta_lab=stheta_lab + sthetacentral_offset/degree
      if (sthetacentral_offset .ne. 0 )
     >  write(6,*)' s_apply_offs:  after: stheta_lab =',stheta_lab
* csa 8/31/99 -- moved to s_physics
*      cossthetas = cos(stheta_lab*degree)
*      sinsthetas = sin(stheta_lab*degree)

      ABORT= .FALSE.
      err= ' '
      RETURN
      END
