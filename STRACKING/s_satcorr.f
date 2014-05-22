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
      real*4 c1,c2 ! TH - Add for new correction to ssdelta

*--------------------------------------------------------
*
      ierr=0
      ABORT=.FALSE.

      p0corr=0.
      deltacorr=0.

!      if (genable_sos_satcorr.ne.0) then
!         if (spcentral.gt.0.96296) then
!            deltacorr = 46.729-137.21*spcentral+181.15*spcentral**2
!     >               -76.089*spcentral**3
!         else
!            deltacorr = 14.6369
!         endif

! -------------------------------------------------------------------------
! TH - Try new ssdelta/ssxpfp correction based on 2003 Heep data
! and fit to Em va ssxpfp of form C1*ssxpfp + C2*ssxpfp**2
! Correct by region in ssxpfp.
! Fix values below pSOS=1.0 to parametrization value at pSOS=1.0

      if (genable_sos_satcorr.ne.0) then
         if (spcentral.gt.1.0) then
            if (ssxp_fp .gt. -0.06) then
               c1 = 0.6067 - 34.318*(spcentral-1.1984)**4
               c2 = 11.078 - 73.930*(spcentral-0.7847)**4
            else
               c1 = 0
               c2 = 3.6746 - 78.690*(spcentral-0.9000)**4
            endif
         else
            if (ssxp_fp .gt. -0.06) then
               c1 = 0.6
               c2 = 11.0
            else
               c1 = 0
               c2 = 3.
            endif
         endif

! -------------------------------------------------------------------------

      endif

*      write(6,*)' s_satcorr: ssdelta, ssxp_fp =',ssdelta, ssxp_fp
*      write(6,*)' s_satcorr: deltacorr, p0corr =',deltacorr,p0corr
c      ssdelta = ssdelta + deltacorr*ssxp_fp**2 + p0corr

!       ssdelta = ssdelta + deltacorr*ssxp_fp**2 
       ssdelta = ssdelta + c1*ssxp_fp + c2*ssxp_fp**2 

*      write(6,*)' s_satcorr: ssdelta =',ssdelta

      ABORT= ierr.ne.0 .or. ABORT

      return
      end
