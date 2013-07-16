      SUBROUTINE S_SATCORR(ABORT,err)
*--------------------------------------------------------
*-
*-   Purpose and Methods : Correct delta or other reconstructed physics variables
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
*-   Created 24-JUN-1998   J. Volmer
*-                           Dummy Shell routine
* $Log: s_satcorr.f,v $
* Revision 1.2  2003/09/05 20:00:03  jones
* Merge in online03 changes (mkj)
*
* Revision 1.1.2.1  2003/04/11 14:01:34  cdaq
* eliminate p0corr since already in s_fieldcorr.f (mkj)
*
* Revision 1.1  1999/02/10 18:35:01  csa
* Initial revision
*
*
* Revision 1.1  1998/06/24  14:58:18  jv
* Initial revision
*
*-
*-
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*9 here
      parameter (here= 'S_SATCORR')
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
      REAL*4 deltacorr,p0corr

*--------------------------------------------------------
*
      ierr=0
      ABORT=.FALSE.

      p0corr=0.
      deltacorr=0.

      if (genable_sos_satcorr.ne.0) then
         if (spcentral.gt.0.96296) then
            deltacorr = 46.729-137.21*spcentral+181.15*spcentral**2
     >               -76.089*spcentral**3
         else
            deltacorr = 14.6369
         endif

c         p0corr = .225
c         if (spcentral.gt.1.483) p0corr=p0corr-16.7*(spcentral-1.483)**2
      endif

*      write(6,*)' s_satcorr: ssdelta, ssxp_fp =',ssdelta, ssxp_fp
*      write(6,*)' s_satcorr: deltacorr, p0corr =',deltacorr,p0corr
c      ssdelta = ssdelta + deltacorr*ssxp_fp**2 + p0corr
       ssdelta = ssdelta + deltacorr*ssxp_fp**2 
*      write(6,*)' s_satcorr: ssdelta =',ssdelta

      ABORT= ierr.ne.0 .or. ABORT

      return
      end
