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
*--------------------------------------------------------
*

 
* csa 8/31/99 -- We really should be filling *new* variables
* here!

      if (s_oopcentral_offset .ne. 0 ) then
        write(*,*) ' ******'
       write(6,*)' s_apply_offs: s_oopcentral_offset =',s_oopcentral_offset,' rad'
       write(6,*)'  Used to offset ssxp_tar in s_physics.f '
      endif
c
      if (spcentral_offset .ne. 0 ) then
        write(*,*) ' ******'
       write(6,*)' s_apply_offs: apply spcentral_offset(%)  =',spcentral_offset
       write(6,*)' s_apply_offs: before:  spcentral =',spcentral
       spcentral = spcentral * ( 1. + spcentral_offset / 100. )
       write(6,*)' s_apply_offs:  after:  spcentral =',spcentral
      endif
c
      if (smomentum_factor .gt. 0.1) then    !avoid setting p=0
        write(*,*) ' ******'
       write(6,*)' s_apply_offs: apply smomentum_factor  =',smomentum_factor
       write(6,*)' s_apply_offs: before:  spcentral =',spcentral
       spcentral = spcentral * smomentum_factor
       write(6,*)' s_apply_offs: after :  spcentral =',spcentral
      endif
c
      if (sthetacentral_offset .ne. 0 ) then
        write(*,*) ' ******'
       write(6,*)' s_apply_offs: before: stheta_lab =',stheta_lab
       stheta_lab=stheta_lab + sthetacentral_offset/degree
       write(6,*)' s_apply_offs:  after: stheta_lab =',stheta_lab
       cossthetas = cos(stheta_lab*degree)
       sinsthetas = sin(stheta_lab*degree)
      endif



      ABORT= .FALSE.
      err= ' '
      RETURN
      END
