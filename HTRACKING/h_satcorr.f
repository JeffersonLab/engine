      SUBROUTINE H_SATCORR(ABORT,err)
*--------------------------------------------------------
*-
*-   Purpose and Methods : Correct delta or other reconstructed physics variables
*-                         for magnet saturation effects
*-                              
*-                                to decoded information 
*-
*-      Required Input BANKS     HMS_FOCAL_PLANE
*-                               HMS_TARGET
*-
*-      Output BANKS             HMS_PHYSICS_R4
*-                               HMS_PHYSICS_I4
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
*-   Created 24-JUN-1998   J. Volmer
*-                           Dummy Shell routine
* $Log$
* Revision 1.1  1999/02/10 18:34:42  csa
* Initial revision
*
*
* Revision 1.1  1994/02/19  06:16:08  cdaq
* Initial revision
*
*-
*-
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*9 here
      parameter (here= 'H_SATCORR')
*
      logical ABORT
      character*(*) err
      integer ierr
*
      include 'gen_data_structures.cmn'
      INCLUDE 'hms_data_structures.cmn'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
*
*     local variables 
*
      REAL*4 p0corr
      
*--------------------------------------------------------
*
      ierr=0
      ABORT=.FALSE.

      p0corr=0.

      if(genable_hms_satcorr.ne.0) then
         if (hpcentral.lt.3.215) p0corr=-1.1298*(hpcentral-3.215)**2
         hsdelta = hsdelta + p0corr*hsxp_fp
      endif

*      hsdelta = hsdelta + 0.

      ABORT= ierr.ne.0 .or. ABORT

      return
      end



