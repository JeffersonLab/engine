      SUBROUTINE S_YTARCALIB(ABORT,err)
*--------------------------------------------------------
*-
*-   Purpose and Methods : Correct ytar or other reconstructed physics variables
*-                         for magnet saturation effects
*-                              
*-                                to decoded information 
*-
*-      Required Input BANKS     SOS_FOCAL_PLANE
*-                               SOS_TARGET
*-
*-      Output BANKS             SOS_PHYSICS_R4
*-                               SOS_PHYSICS_I4
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
*-   Created 1-JUN-2004   C. Xu
*-                           Dummy Shell routine
* $Log$
* Revision 1.1.2.1  2004/07/01 14:49:50  jones
* New codes to correct SOS ytar, xptar and yptar for the fpi2
* branch of Analyzer.
*
*
*-
*-
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*9 here
      parameter (here= 'S_YTARCALIB')
*
      logical ABORT
      character*(*) err
      integer ierr
*
      include 'gen_data_structures.cmn'
      INCLUDE 'sos_data_structures.cmn'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
*
*     local variables 
*
      REAL*4 ytark,ytarcorr

*--------------------------------------------------------
*
      ierr=0
      ABORT=.FALSE.

      ytarcorr=0.
      ytark=0. 

      if(abs(spcentral-0.9).lt.0.2)   ytark=1.4636
      if(abs(spcentral-1.65).lt.0.03) ytark=0.0
      if(abs(spcentral-1.74).lt.0.03) ytark=-1.7467     
      
       ytarcorr=ytark*ssyp_tar

       ssy_tar = ssy_tar + ytarcorr 

*      write(6,*)' s_ytarcalib: ssy_tar =',ssy_tar

      ABORT= ierr.ne.0 .or. ABORT

      return
      end

