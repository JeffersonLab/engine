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
* Revision 1.4  1996/01/24 15:57:36  saw
* (JRA) Change variables to lower case
*
* Revision 1.3  1995/05/22 19:39:13  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.2  1995/05/11  16:21:07  cdaq
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
      INCLUDE 'hms_data_structures.cmn'
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
      hphi_lab = 3*tt/2
*
      coshthetas = cos(htheta_lab)
      sinhthetas = sin(htheta_lab)
*     Constants for elastic kinematics calcultion
      hphysicsa = 2.*gebeam*gtarg_mass(gtarg_num) -
     $     mass_electron**2 - hpartmass**2
      hphysicsb = 2. * (gtarg_mass(gtarg_num) - gebeam)
      hphysicab2 = hphysicsa**2 * hphysicsb**2
      hphysicsm3b = hpartmass**2 * hphysicsb**2
      return
      end
