      SUBROUTINE S_PHYSICS(ABORT,err)
*--------------------------------------------------------
*-
*-   Purpose and Methods : Do final SOS physics analysis on SOS only part of
*-                            event.
*-                              
*-                                to decoded information 
*-
*-      Required Input BANKS     SOS_FOCAL_PLANE
*-                               SOS_TARGET
*-                               SOS_TRACK_TESTS
*-
*-      Output BANKS             SOS_PHYSICS_R4
*-                               SOS_PHYSICS_I4
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
*-   Created 19-JAN-1994   D. F. Geesaman
*-                           Dummy Shell routine
* $Log$
* Revision 1.4  1995/01/18 20:57:12  cdaq
* (SAW) Correct some trig and check for negative arg in elastic kin calculation
*
* Revision 1.4  1995/01/18  20:00:04  cdaq
* (SAW) Correct some trig and check for negative arg in elastic kin calculation
*
* Revision 1.3  1994/11/23  13:55:03  cdaq
* (SPB) Recopied from hms file and modified names for SOS
*
* Revision 1.2  1994/06/14  03:41:10  cdaq
* (DFG) Calculate physics quantities
*
* Revision 1.1  1994/02/21  16:15:43  cdaq
* Initial revision
*
*-
*-
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*50 here
      parameter (here= 'S_PHYSICS')
*
      logical ABORT
      character*(*) err
      integer*4 ierr
*
      INCLUDE 'gen_data_structures.cmn'
      INCLUDE 'gen_routines.dec'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      INCLUDE 'sos_physics_sing.cmn'
      INCLUDE 'mc_structures.cmn'
*     
*     local variables 
      integer*4 goodtrack,track
      real*4    COSGAMMA,COSSSTHETA,SINSSTHETA,TANDELPHI,SINSPHI
      real*4    p3, t1,ta,t3,sminv2,chi2min,chi2perdeg
*--------------------------------------------------------
*
      ABORT= .FALSE.
      err= ' '
*     Need to test to chose the best track
      if( SNTRACKS_FP.GT. 0) then
         chi2min= 1e10
         goodtrack = 0
         do track = 1, SNTRACKS_FP

            if( SNFREE_FP(track).ge. ssel_ndegreesmin) then
               chi2perdeg = SCHI2_FP(track)/FLOAT(SNFREE_FP(track))
               if(chi2perdeg .lt. chi2min) then
*     simple particle id tests
                  if( ( SDEDX(track,1) .gt. ssel_dedx1min)  .and.
     &                 ( SDEDX(track,1) .lt. ssel_dedx1max)  .and.
     &                 ( SBETA(track)   .gt. ssel_betamin)   .and.
     &                 ( SBETA(track)   .lt. ssel_betamax)   .and.
     &                 ( STRACK_ET(track) .gt. ssel_etmin)   .and.
     &                 ( STRACK_ET(track) .lt. ssel_etmax)) then
                     goodtrack = track
                  endif                 ! end test on track id
               endif                    ! end test on lower chisq
            endif                       ! end test on minimum number of degrees of freedom
         enddo                          ! end loop on track
         SSNUM_TARTRACK = goodtrack
         SSNUM_FPTRACK  = goodtrack
         if(goodtrack.eq.0) return      ! return if no valid tracks

*     ! with zero set in SSNUM_...
         SSP = SP_TAR(SSNUM_TARTRACK)
         SSENERGY = SQRT(SSP*SSP+SPARTMASS*SPARTMASS)
*     Copy variables for ntuple so we can test on them
         SSDELTA  = SDELTA_TAR(SSNUM_TARTRACK)
         SSX_TAR  = SX_TAR(SSNUM_TARTRACK)
         SSY_TAR  = SY_TAR(SSNUM_TARTRACK)
         SSXP_TAR  = SXP_TAR(SSNUM_TARTRACK)
         SSYP_TAR  = SYP_TAR(SSNUM_TARTRACK)
         SSDEDX1   = SDEDX(SSNUM_FPTRACK,1)
         SSDEDX2   = SDEDX(SSNUM_FPTRACK,2)
         SSDEDX3   = SDEDX(SSNUM_FPTRACK,3)
         SSDEDX4   = SDEDX(SSNUM_FPTRACK,4)
         SSBETA   = SBETA(SSNUM_FPTRACK)
         SSBETA_CHISQ = SBETA_CHISQ(SSNUM_FPTRACK)
         SSTRACK_ET   = STRACK_ET(SSNUM_FPTRACK)
         SSTRACK_PRESHOWER_E   = STRACK_PRESHOWER_E(SSNUM_FPTRACK)
         SSTIME_AT_FP   = STIME_AT_FP(SSNUM_FPTRACK)
         SSX_FP   = SX_FP(SSNUM_FPTRACK)
         SSY_FP   = SY_FP(SSNUM_FPTRACK)
         SSXP_FP   = SXP_FP(SSNUM_FPTRACK)
         SSYP_FP   = SYP_FP(SSNUM_FPTRACK)
         SSCHI2PERDEG  = SCHI2_FP(SSNUM_FPTRACK)
     $        /FLOAT(SNFREE_FP(SSNUM_FPTRACK))
         SSNFREE_FP = SNFREE_FP(SSNUM_FPTRACK)
         cosgamma = 1.0/sqrt(1.0 + ssxp_tar**2 - ssyp_tar**2)
         cossstheta = cosgamma*(sinsthetas * ssyp_tar + cossthetas)
ccc         if( ABS(COSSSTHETA) .LT. 1.) then
            SSTHETA = ACOS(COSSSTHETA)
ccc         else
ccc            SSTHETA = 0.
ccc         endif
         SINSSTHETA = SIN(SSTHETA)
         tandelphi = ssxp_tar /
     &        ( sinsthetas - cossthetas*ssyp_tar )
         SSPHI = SPHI_LAB + TANDELPHI   ! PHI_LAB must be multpiple of
         SINSPHI = SIN(SSPHI)           ! pi/2, or above is crap
*     Calculate elastic scattering kinematics
         t1  = 2.*SPHYSICSA*CPBEAM*COSSSTHETA      
         ta  = 4*CPBEAM**2*COSSSTHETA**2 - SPHYSICSB**2
         if(ta.eq.0 .or. ( SPHYSICAB2 + SPHYSICSM3B * ta).lt.0.0) then
            p3=0.       
         else
            t3  = ta-SPHYSICSB**2
            p3  = (t1 - SQRT( SPHYSICAB2 + SPHYSICSM3B * ta)) / ta
         endif
*     This is the difference in the momentum obtained by tracking
*     and the momentum from elastic kinematics
         SSELAS_COR = SSP - p3
*     INVARIANT MASS OF THE REMAINING PARTICLES
         sminv2 =   ( (CEBEAM+TMASS_TARGET-SSENERGY)**2
     &        - (CPBEAM - SSP * COSSSTHETA)**2
     &        - ( SSP * SINSSTHETA)**2  )       
         if(sminv2.ge.0 ) then
            SSMINV = SQRT(sminv2)
         else
            SSMINV = 0.
         endif                          ! end test on positive arg of SQRT
*     SSZBEAM is the intersection of the beam ray with the spectrometer
*     as measured along the z axis.
         if( SINSSTHETA .eq. 0.) then
            SSZBEAM = 0.
         else
            SSZBEAM = SINSPHI * ( -SSY_TAR + CYRAST * COSSSTHETA) /
     $           SINSSTHETA 
       endif                 ! end test on SINSSTHETA=0
*

*     execute physics singles tests
       ierr=thtstexeb('sos_physics_sing')     
*
*     calculate physics statistics and wire chamber efficencies
       call s_physics_stat(ABORT,err)
       ABORT= ierr.ne.0 .or. ABORT
       IF(ABORT) THEN
          call G_add_path(here,err)
       ENDIF
      endif                             ! end test on zero tracks
      RETURN
      END
