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
* Revision 1.10  1995/05/11 17:15:07  cdaq
* (SAW) Add additional kinematics variables
*
* Revision 1.9  1995/03/22  16:23:27  cdaq
* (SAW) Target track data is now slopes.
*
* Revision 1.8  1995/02/23  13:37:31  cdaq
* (SAW) Reformat and cleanup
*
* Revision 1.7  1995/02/10  18:44:47  cdaq
* (SAW) _tar values are now angles instead of slopes
*
* Revision 1.6  1995/02/02  13:05:40  cdaq
* (SAW) Moved best track selection code into H_SELECT_BEST_TRACK (new)
*
* Revision 1.5  1995/01/27  20:24:14  cdaq
* (JRA) Add some useful physics quantities
*
* Revision 1.4  1995/01/18  16:29:26  cdaq
* (SAW) Correct some trig and check for negative arg in elastic kin calculation
*
* Revision 1.3  1994/09/13  19:51:03  cdaq
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
      character*9 here
      parameter (here= 'H_PHYSICS')
*
      logical ABORT
      character*(*) err
      integer ierr
*
      INCLUDE 'gen_data_structures.cmn'
      INCLUDE 'gen_routines.dec'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      INCLUDE 'hms_physics_sing.cmn'
      INCLUDE 'hms_calorimeter.cmn'
      INCLUDE 'hms_scin_parms.cmn'
*      INCLUDE 'hms_scin_tof.cmn'
      INCLUDE 'hms_tracking.cmn'
*
*     local variables 
      integer*4 i,ip
      real*4 cosgamma,tandelphi,sinhphi,coshstheta,sinhstheta
      real*4 t1,ta,p3,t3,hminv2
      real*4 coshsthetaq
*
*--------------------------------------------------------
*
      if(HSNUM_FPTRACK.gt.0) then       ! Good track has been selected
        HSP = HP_TAR(HSNUM_TARTRACK)
        HSENERGY = SQRT(HSP*HSP+HPARTMASS*HPARTMASS)
*     Copy variables for ntuple so we can test on them
        HSDELTA  = HDELTA_TAR(HSNUM_TARTRACK)
        HSX_TAR  = HX_TAR(HSNUM_TARTRACK)
        HSY_TAR  = HY_TAR(HSNUM_TARTRACK)
        HSXP_TAR  = HXP_TAR(HSNUM_TARTRACK) ! This is an angle (radians)
        HSYP_TAR  = HYP_TAR(HSNUM_TARTRACK) ! This is an angle (radians)
        HSBETA   = HBETA(HSNUM_FPTRACK)
        HSBETA_CHISQ = HBETA_CHISQ(HSNUM_FPTRACK)
        HSTRACK_ET   = HTRACK_ET(HSNUM_FPTRACK)
        HSTRACK_PRESHOWER_E   = HTRACK_PRESHOWER_E(HSNUM_FPTRACK)
        HSTIME_AT_FP   = HTIME_AT_FP(HSNUM_FPTRACK)
        HSX_FP   = HX_FP(HSNUM_FPTRACK)
        HSY_FP   = HY_FP(HSNUM_FPTRACK)
        HSXP_FP   = HXP_FP(HSNUM_FPTRACK) ! This is a slope (dx/dz)
        HSYP_FP   = HYP_FP(HSNUM_FPTRACK) ! This is a slope (dy/dz)

c     hsx_dc1 = hsx_fp + hsxp_fp * hdc_zpos(1)
c     hsy_dc1 = hsy_fp + hsyp_fp * hdc_zpos(1)
c     hsx_dc2 = hsx_fp + hsxp_fp * hdc_zpos(2)
c     hsy_dc2 = hsy_fp + hsyp_fp * hdc_zpos(2)
        hsx_s1 = hsx_fp + hsxp_fp * hscin_1x_zpos
        hsy_s1 = hsy_fp + hsyp_fp * hscin_1x_zpos
        hsx_s2 = hsx_fp + hsxp_fp * hscin_2x_zpos
        hsy_s2 = hsy_fp + hsyp_fp * hscin_2x_zpos
        hsx_cal = hsx_fp + hsxp_fp * hcal_1pr_zpos
        hsy_cal = hsy_fp + hsyp_fp * hcal_1pr_zpos
c     ?????
        htrue_x_fp = hsx_fp / sind(85.0) / (1/tand(85.0) - hsxp_fp)

        do ip=1,4
          hsscin_elem_hit(ip)=0
        enddo
        do i=1,hnum_scin_hit(hsnum_fptrack)
          ip=hscin_plane_num(hscin_hit(hsnum_fptrack,i))
          if (hsscin_elem_hit(ip).eq.0) then
            hsscin_elem_hit(ip)=hscin_counter_num(hscin_hit(
     $           hsnum_fptrack,i))
            hsdedx(ip)=hdedx(hsnum_fptrack,i)
          else                          ! more than 1 hit in plane
            hsscin_elem_hit(ip)=18
            hsdedx(ip)=sqrt(hsdedx(ip)*hdedx(hsnum_fptrack,i))
          endif
        enddo

        hsnum_scin_hit = hnum_scin_hit(hsnum_fptrack)
        hsnum_pmt_hit = hnum_pmt_hit(hsnum_fptrack)

        HSCHI2PERDEG  = HCHI2_FP(HSNUM_FPTRACK)
     $       /FLOAT(HNFREE_FP(HSNUM_FPTRACK))
        HSNFREE_FP = HNFREE_FP(HSNUM_FPTRACK)
        cosgamma = 1.0/sqrt(1.0 + hsxp_tar**2 + hsyp_tar**2)
        coshstheta = cosgamma*(sinhthetas * hsyp_tar + coshthetas)
        HSTHETA = ACOS(COSHSTHETA)
        SINHSTHETA = SIN(HSTHETA)
        tandelphi = hsxp_tar /
     &       ( sinhthetas - coshthetas*hsyp_tar)
        HSPHI = HPHI_LAB + TANDELPHI    ! HPHI_LAB must be multiple of
        SINHPHI = SIN(HSPHI)            ! pi/2, or above is crap
*     Calculate elastic scattering kinematics
        t1  = 2.*HPHYSICSA*CPBEAM*COSHSTHETA      
        ta  = 4*CPBEAM**2*COSHSTHETA**2 - HPHYSICSB**2
ccc   SAW 1/17/95.  Add the stuff after the or.
        if(ta.eq.0.0 .or. ( HPHYSICAB2 + HPHYSICSM3B * ta).lt.0.0) then
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
     &       - (CPBEAM - HSP * COSHSTHETA)**2
     &       - ( HSP * SINHSTHETA)**2  )       
        if(hminv2.ge.0 ) then
          HSMINV = SQRT(hminv2)
        else
          HSMINV = 0.
        endif                           ! end test on positive arg of SQRT
*     HSZBEAM is the intersection of the beam ray with the spectrometer
*     as measured along the z axis.
        if( SINHSTHETA .eq. 0.) then
          HSZBEAM = 0.
        else
          HSZBEAM = SINHPHI * ( -HSY_TAR + CYRAST * COSHSTHETA) /
     $         SINHSTHETA 
        endif                           ! end test on SINHSTHETA=0
*
*     More kinematics
*
        if(hsbeta.gt.0) then
          hsmass2 = (1/hsbeta*2 - 1)*hsp**2
        else
          hsmass2 = 1.0E10
        endif

        hst = (CEBEAM - HSENERGY)**2
     $       - (CPBEAM - HSP*COSHSTHETA)**2 - (HSP*SINHSTHETA)**2
        hsu = (TMASS_TARGET - HSENERGY)**2 - HSP**2

        hseloss = CEBEAM - HSENERGY
        hsq3 = sqrt(CPBEAM**2 + HSP**2 - 2*CPBEAM*HSP*COSHSTHETA)
        coshsthetaq = (CPBEAM**2 - CPBEAM*HSP*COSHSTHETA)/CPBEAM/hsq3
        hsthetaq = acos(coshsthetaq)
        hsphiq = hsphi + TT
        hsbigq2 = -hst
        hsx = hsbigq2/(2*mass_nucleon*hseloss)
        hsy = hseloss/CEBEAM
        hsw2 = TMASS_TARGET**2 + 2*TMASS_TARGET*hseloss - hsbigq2
        if(hsw2.ge.0.0) then
          hsw = sqrt(hsw2)
        else
          hsw = 0.0
        endif

*     execute physics singles tests.
***   ierr=thtstexeb('hms_physics_sing') ! This is going to get executed twice
*     
*     Write raw timing information for fitting.
        if(hdebugdumptof.ne.0) call h_dump_tof
*
*     Calculate physics statistics and wire chamber efficencies.
        call h_physics_stat(ABORT,err)
        ABORT= ierr.ne.0 .or. ABORT
        IF(ABORT) THEN
          call G_add_path(here,err)
        ENDIF
      endif                             ! end test on zero tracks
      RETURN
      END
