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
* Revision 1.10  1995/08/31 18:45:26  cdaq
* (JRA) Add projection to cerenkov mirror pos, fill sdc_sing_res array
*
* Revision 1.9  1995/07/20  18:59:15  cdaq
* (SAW) Declare sind and tand for f2c compatibility
*
* Revision 1.8  1995/05/22  19:45:43  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.7  1995/05/11  17:15:15  cdaq
* (SAW) Add additional kinematics variables
*
* Revision 1.6  1995/04/06  19:37:30  cdaq
* (SAW) Fix typo
*
* Revision 1.5  1995/02/23  13:39:13  cdaq
* (SAW) Moved best track selection code into S_SELECT_BEST_TRACK (new)
*
* Revision 1.4  1995/01/18  20:57:12  cdaq
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
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*9 here
      parameter (here= 'S_PHYSICS')
*
      logical ABORT
      character*(*) err
      integer*4 ierr
*
      include 'gen_data_structures.cmn'
      INCLUDE 'sos_data_structures.cmn'
      INCLUDE 'gen_routines.dec'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      INCLUDE 'sos_physics_sing.cmn'
      INCLUDE 'mc_structures.cmn'
      INCLUDE 'sos_calorimeter.cmn'
      INCLUDE 'sos_scin_parms.cmn'
      INCLUDE 'sos_tracking.cmn'
      INCLUDE 'sos_cer_parms.cmn'
      INCLUDE 'sos_geometry.cmn'
*     
*     local variables 
      integer*4 i,ip
      real*4 cosgamma,tandelphi,sinsphi,cossstheta,sinsstheta
      real*4 t1,ta,p3,t3,sminv2
      real*4 cosssthetaq
      real*4 sind,tand
*     
*--------------------------------------------------------
*
      if(SSNUM_FPTRACK.gt.0) then       ! Good track has been selected
        SSP = SP_TAR(SSNUM_TARTRACK)
        SSENERGY = SQRT(SSP*SSP+SPARTMASS*SPARTMASS)
*     Copy variables for ntuple so we can test on them
        SSDELTA  = SDELTA_TAR(SSNUM_TARTRACK)
        SSX_TAR  = SX_TAR(SSNUM_TARTRACK)
        SSY_TAR  = SY_TAR(SSNUM_TARTRACK)
        SSXP_TAR  = SXP_TAR(SSNUM_TARTRACK) ! This is an angle (radians)
        SSYP_TAR  = SYP_TAR(SSNUM_TARTRACK) ! This is an angle (radians)
        SSBETA   = SBETA(SSNUM_FPTRACK)
        SSBETA_CHISQ = SBETA_CHISQ(SSNUM_FPTRACK)
        SSTRACK_ET   = STRACK_ET(SSNUM_FPTRACK)
        SSTRACK_PRESHOWER_E   = STRACK_PRESHOWER_E(SSNUM_FPTRACK)
        SSTIME_AT_FP   = STIME_AT_FP(SSNUM_FPTRACK)
        SSX_FP   = SX_FP(SSNUM_FPTRACK)
        SSY_FP   = SY_FP(SSNUM_FPTRACK)
        SSXP_FP   = SXP_FP(SSNUM_FPTRACK) ! This is a slope (dx/dz)
        SSYP_FP   = SYP_FP(SSNUM_FPTRACK) ! This is a slope (dy/dz)

        ssx_dc1 = ssx_fp + ssxp_fp * sdc_1_zpos
        ssy_dc1 = ssy_fp + ssyp_fp * sdc_1_zpos
        ssx_dc2 = ssx_fp + ssxp_fp * sdc_2_zpos
        ssy_dc2 = ssy_fp + ssyp_fp * sdc_2_zpos
        ssx_s1 = ssx_fp + ssxp_fp * sscin_1x_zpos
        ssy_s1 = ssy_fp + ssyp_fp * sscin_1x_zpos
        ssx_cer = ssx_fp + ssxp_fp * scer_mirror_zpos
        ssy_cer = ssy_fp + ssyp_fp * scer_mirror_zpos
        ssx_s2 = ssx_fp + ssxp_fp * sscin_2x_zpos
        ssy_s2 = ssy_fp + ssyp_fp * sscin_2x_zpos
        ssx_cal = ssx_fp + ssxp_fp * scal_1pr_zpos
        ssy_cal = ssy_fp + ssyp_fp * scal_1pr_zpos

        do ip=1,4
          ssscin_elem_hit(ip)=0
        enddo
        do i=1,snum_scin_hit(ssnum_fptrack)
          ip=sscin_plane_num(sscin_hit(ssnum_fptrack,i))
          if (ssscin_elem_hit(ip).eq.0) then
            ssscin_elem_hit(ip)=sscin_counter_num(sscin_hit(
     $           ssnum_fptrack,i))
            ssdedx(ip) = sdedx(ssnum_fptrack,i)
          else                          ! more than 1 hit in plane
            ssscin_elem_hit(ip)=18
            ssdedx(ip) = sqrt(ssdedx(ip)*sdedx(ssnum_fptrack,i))  !assume <3 hits. 
          endif
        enddo                             

        ssnum_scin_hit = snum_scin_hit(ssnum_fptrack)
        ssnum_pmt_hit = snum_pmt_hit(ssnum_fptrack)

        do ip = 1 , sdc_num_planes
          sdc_sing_res(ip)=sdc_single_residual(ssnum_fptrack,ip)
        enddo

        SSCHI2PERDEG  = SCHI2_FP(SSNUM_FPTRACK)
     $       /FLOAT(SNFREE_FP(SSNUM_FPTRACK))
        SSNFREE_FP = SNFREE_FP(SSNUM_FPTRACK)
        cosgamma = 1.0/sqrt(1.0 + ssxp_tar**2 + ssyp_tar**2)
        cossstheta = cosgamma*(sinsthetas * ssyp_tar + cossthetas)
        SSTHETA = ACOS(COSSSTHETA)
        SINSSTHETA = SIN(SSTHETA)
        tandelphi = ssxp_tar /
     &       ( sinsthetas - cossthetas*ssyp_tar )
        SSPHI = SPHI_LAB + TANDELPHI    ! PHI_LAB must be multpiple of
        SINSPHI = SIN(SSPHI)            ! pi/2, or above is crap
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
     &       - (CPBEAM - SSP * COSSSTHETA)**2
     &       - ( SSP * SINSSTHETA)**2  )       
        if(sminv2.ge.0 ) then
          SSMINV = SQRT(sminv2)
        else
          SSMINV = 0.
        endif                           ! end test on positive arg of SQRT
*     SSZBEAM is the intersection of the beam ray with the spectrometer
*     as measured along the z axis.
        if( SINSSTHETA .eq. 0.) then
          SSZBEAM = 0.
        else
          SSZBEAM = SINSPHI * ( -SSY_TAR + CYRAST * COSSSTHETA) /
     $         SINSSTHETA 
        endif                           ! end test on SINSSTHETA=0
*
*     More kinematics
*
        if(ssbeta.gt.0) then
          ssmass2 = (1/ssbeta*2 - 1)*ssp**2
        else
          ssmass2 = 1.0E10
        endif

        sst = (CEBEAM - SSENERGY)**2
     $       - (CPBEAM - SSP*COSSSTHETA)**2 - (SSP*SINSSTHETA)**2
        ssu = (TMASS_TARGET - SSENERGY)**2 - SSP**2

        sseloss = CEBEAM - SSENERGY
        ssq3 = sqrt(CPBEAM**2 + SSP**2 - 2*CPBEAM*SSP*COSSSTHETA)
        cosssthetaq = (CPBEAM**2 - CPBEAM*SSP*COSSSTHETA)/CPBEAM/ssq3
        ssthetaq = acos(cosssthetaq)
        ssphiq = ssphi + TT
        ssbigq2 = -sst
        ssx = ssbigq2/(2*mass_nucleon*sseloss)
        ssy = sseloss/CEBEAM
        ssw2 = TMASS_TARGET**2 + 2*TMASS_TARGET*sseloss - ssbigq2
        if(ssw2.ge.0.0) then
          ssw = sqrt(ssw2)
        else
          ssw = 0.0
        endif

*     execute physics singles tests
***   ierr=thtstexeb('sos_physics_sing') ! This is going to get executed twice
*     
*     Turn on to write raw timing information for fitting
        if(sdebugdumptof.ne.0) call s_dump_tof
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
