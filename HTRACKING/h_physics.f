      SUBROUTINE H_PHYSICS(ABORT,err)
*--------------------------------------------------------
*-
*-   Purpose and Methods : Do final HMS physics analysis on HMS only part of
*-                            event.
*-                              
*-                                to decoded information 
*-
*-      Required Input BANKS     HMS_FOCAL_PLANE
*-                               HMS_TARGET
*-                               HMS_TRACK_TESTS
*-
*-      Output BANKS             HMS_PHYSICS_R4
*-                               HMS_PHYSICS_I4
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
*-   Created 19-JAN-1994   D. F. Geesaman
*-                           Dummy Shell routine
* $Log$
* Revision 1.3  1994/09/13 19:51:03  cdaq
* (JRA) Add HBETA_CHISQ
*
* Revision 1.2  1994/06/14  03:49:49  cdaq
* (DFG) Calculate physics quantities
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
      character*50 here
      parameter (here= 'H_PHYSICS')
*
      logical ABORT
      character*(*) err
      integer*4 ierr
*
      INCLUDE 'gen_data_structures.cmn'
      INCLUDE 'gen_routines.dec'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      INCLUDE 'hms_physics_sing.cmn'
      INCLUDE 'mc_structures.cmn'
*
*     local variables 
      integer*4 goodtrack,track
      real*4    COSGAMMA,COSHSTHETA,SINHSTHETA,TANDELPHI,SINHPHI
      real*4    p3, t1,ta,t3,hminv2,chi2min,chi2perdeg
*--------------------------------------------------------
*
      ABORT= .FALSE.
      err= ' '
*     Need to test to chose the best track
      if( HNTRACKS_FP.GT. 0) then
         chi2min= 1e10
         goodtrack = 0
         do track = 1, HNTRACKS_FP

            if( HNFREE_FP(track).ge. hsel_ndegreesmin) then
               chi2perdeg = HCHI2_FP(track)/FLOAT(HNFREE_FP(track))
               if(chi2perdeg .lt. chi2min) then
*     simple particle id tests
                  if( ( HDEDX(track,1) .gt. hsel_dedx1min)  .and.
     &                 ( HDEDX(track,1) .lt. hsel_dedx1max)  .and.
     &                 ( HBETA(track)   .gt. hsel_betamin)   .and.
     &                 ( HBETA(track)   .lt. hsel_betamax)   .and.
     &                 ( HTRACK_ET(track) .gt. hsel_etmin)   .and.
     &                 ( HTRACK_ET(track) .lt. hsel_etmax)) then
                     goodtrack = track
                  endif                 ! end test on track id
               endif                    ! end test on lower chisq
            endif                       ! end test on minimum number of degrees of freedom
         enddo                          ! end loop on track
         HSNUM_TARTRACK = goodtrack
         HSNUM_FPTRACK  = goodtrack
         if(goodtrack.eq.0) return      ! return if no valid tracks

*     ! with zero set in HSNUM_...
         HSP = HP_TAR(HSNUM_TARTRACK)
         HSENERGY = SQRT(HSP*HSP+HPARTMASS*HPARTMASS)
*     Copy variables for ntuple so we can test on them
         HSDELTA  = HDELTA_TAR(HSNUM_TARTRACK)
         HSX_TAR  = HX_TAR(HSNUM_TARTRACK)
         HSY_TAR  = HY_TAR(HSNUM_TARTRACK)
         HSXP_TAR  = HXP_TAR(HSNUM_TARTRACK)
         HSYP_TAR  = HYP_TAR(HSNUM_TARTRACK)
         HSDEDX1   = HDEDX(HSNUM_FPTRACK,1)
         HSDEDX2   = HDEDX(HSNUM_FPTRACK,2)
         HSDEDX3   = HDEDX(HSNUM_FPTRACK,3)
         HSDEDX4   = HDEDX(HSNUM_FPTRACK,4)
         HSBETA   = HBETA(HSNUM_FPTRACK)
         HSBETA_CHISQ = HBETA_CHISQ(HSNUM_FPTRACK)
         HSTRACK_ET   = HTRACK_ET(HSNUM_FPTRACK)
         HSTRACK_PRESHOWER_E   = HTRACK_PRESHOWER_E(HSNUM_FPTRACK)
         HSTIME_AT_FP   = HTIME_AT_FP(HSNUM_FPTRACK)
         HSX_FP   = HX_FP(HSNUM_FPTRACK)
         HSY_FP   = HY_FP(HSNUM_FPTRACK)
         HSXP_FP   = HXP_FP(HSNUM_FPTRACK)
         HSYP_FP   = HYP_FP(HSNUM_FPTRACK)
         HSCHI2PERDEG  = HCHI2_FP(HSNUM_FPTRACK)
     $        /FLOAT(HNFREE_FP(HSNUM_FPTRACK))
         HSNFREE_FP = HNFREE_FP(HSNUM_FPTRACK)
         COSGAMMA = SQRT( 1. - HSXP_TAR**2 - HSYP_TAR**2)
         COSHSTHETA = SINHTHETAS * HSYP_TAR + COSHTHETAS * COSGAMMA
         if( ABS(COSHSTHETA) .LT. 1.) then
            HSTHETA = ACOS(COSHSTHETA)
         else
            HSTHETA = 0.
         endif
         SINHSTHETA = SIN(HSTHETA)
         TANDELPHI = HSXP_TAR /
     &        ( SINHTHETAS*COSGAMMA - COSHTHETAS*HSYP_TAR )
         HSPHI = HPHI_LAB + TANDELPHI
         SINHPHI = SIN(HSPHI)
*     Calculate elastic scattering kinematics
         t1  = 2.*HPHYSICSA*CPBEAM*COSHSTHETA      
         ta  = 4*CPBEAM**2*COSHSTHETA**2 - HPHYSICSB**2
         if(ta.eq.0) then
            p3=0.       
         else
            t3  = ta-HPHYSICSB**2
            p3  = (t1 - SQRT( HPHYSICAB2 + HPHYSICSM3B * ta)) / ta
         endif
*     This is the difference in the momentum obtained by tracking
*     and the momentum from elastic kinematics
         HSELAS_COR = HSP - p3
*     INVARIANT MASS OF THE REMAINING PARTICLES
         hminv2 =   ( (CEBEAM+TMASS_TARGET-HSENERGY)**2
     &        - (CPBEAM - HSP * COSHSTHETA)**2
     &        - ( HSP * SINHSTHETA)**2  )       
         if(hminv2.ge.0 ) then
            HSMINV = SQRT(hminv2)
         else
            HSMINV = 0.
         endif                          ! end test on positive arg of SQRT
*     HSZBEAM is the intersection of the beam ray with the spectrometer
*     as measured along the z axis.
         if( SINHSTHETA .eq. 0.) then
            HSZBEAM = 0.
         else
            HSZBEAM = SINHPHI * ( -HSY_TAR + CYRAST * COSHSTHETA) /
     $           SINHSTHETA 
         endif                          ! end test on SINHSTHETA=0
*     
         
*     execute physics singles tests
         ierr=thtstexeb('hms_physics_sing')     
*
*     calculate physics statistics and wire chamber efficencies
         call h_physics_stat(ABORT,err)
         ABORT= ierr.ne.0 .or. ABORT
         IF(ABORT) THEN
            call G_add_path(here,err)
         ENDIF
      endif                             ! end test on zero tracks
      RETURN
      END
