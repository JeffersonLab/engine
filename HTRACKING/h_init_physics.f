      SUBROUTINE h_init_physics(ABORT,err)
*--------------------------------------------------------
*-
*-   Purpose and Methods : Initialize constants for h_physics
*-                              
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
*-   Created 6-6-94          D. F. Geesaman
* $Log$
* Revision 1.2  1995/05/11 16:21:07  cdaq
* (SAW) Force HMS to be in plane beam right
*
* Revision 1.1  1994/06/14  03:54:14  cdaq
* Initial revision
*
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*50 here
      parameter (here= 'h_init_physics')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 'gen_data_structures.cmn'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      INCLUDE 'hms_physics_sing.cmn'
*
*     local variables 
*--------------------------------------------------------
*
      ABORT= .FALSE.
      err= ' '
*
*     Fix HMS to be in plane, beam right
*
      HPHI_LAB = 3*TT/2
*
      COSHTHETAS = COS(HTHETA_LAB)
      SINHTHETAS = SIN(HTHETA_LAB)
*     Constants for elastic kinematics calcultion
      HPHYSICSA = 2.*CEBEAM*TMASS_TARGET - mass_electron**2 - HPARTMASS**2
      HPHYSICSB = 2. * ( TMASS_TARGET - CEBEAM)
      HPHYSICAB2 = HPHYSICSA**2 * HPHYSICSB**2
      HPHYSICSM3B = HPARTMASS**2 * HPHYSICSB**2
      RETURN
      END
