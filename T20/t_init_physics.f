      SUBROUTINE t_init_physics(ABORT,err)
*--------------------------------------------------------
*-
*-   Purpose and Methods : Initialize constants for s_physics
*-                              
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
*-   Created 6-6-94          D. F. Geesaman
* $Log$
* Revision 1.1  1998/12/01 20:56:44  saw
* Initial revision
*
* Revision 1.5  1996/09/05 19:54:16  saw
* (JRA) avoid setting p=0??
*
* Revision 1.4  1996/01/24 16:07:34  saw
* (JRA) Change upper case to lower case, cebeam to gebeam
*
* Revision 1.3  1995/05/22 19:45:41  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.2  1995/05/11  17:07:14  cdaq
* (SAW) Fix SOS to be in plane, beam left
*
* Revision 1.1  1994/06/14  04:09:12  cdaq
* Initial revision
*
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*14 here
      parameter (here= 't_init_physics')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 'gen_data_structures.cmn'
      INCLUDE 't20_data_structures.cmn'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
c      INCLUDE 't20_physics_sing.cmn'
*
*     local variables 
*--------------------------------------------------------
*
      ABORT= .FALSE.
      err= ' '
*
*     Fix SOS to be in plane, beam left
*
c      sphi_lab = tt/2
c*
c      if (smomentum_factor .gt. 0.1) then    !avoid setting p=0
c        spcentral = spcentral * smomentum_factor
c      endif
c*
c      cossthetas = cos(stheta_lab)
c      sinsthetas = sin(stheta_lab)
c*     Constants for elastic kinematics calcultion
c      sphysicsa = 2.*gebeam*gtarg_mass(gtarg_num) -
c     $     mass_electron**2 - spartmass**2
c      sphysicsb = 2. * (gtarg_mass(gtarg_num) - gebeam)
c      sphysicab2 = sphysicsa**2 * sphysicsb**2
c      sphysicsm3b = spartmass**2 * sphysicsb**2
      return
      end
