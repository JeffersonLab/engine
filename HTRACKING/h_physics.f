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
* Revision 1.14  1995/10/09 20:22:15  cdaq
* (JRA) Add call to h_dump_cal, change upper to lower case
*
* Revision 1.13  1995/08/31 14:49:03  cdaq
* (JRA) Add projection to cerenkov mirror pos, fill hdc_sing_res array
*
* Revision 1.12  1995/07/19  20:53:26  cdaq
* (SAW) Declare sind and tand for f2c compatibility
*
* Revision 1.11  1995/05/22  19:39:15  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.10  1995/05/11  17:15:07  cdaq
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
      include 'gen_data_structures.cmn'
      INCLUDE 'hms_data_structures.cmn'
      INCLUDE 'gen_routines.dec'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      INCLUDE 'hms_physics_sing.cmn'
      INCLUDE 'hms_calorimeter.cmn'
      INCLUDE 'hms_scin_parms.cmn'
      INCLUDE 'hms_tracking.cmn'
      INCLUDE 'hms_cer_parms.cmn'
      INCLUDE 'hms_geometry.cmn'
*
*     local variables 
      integer*4 i,ip
      real*4 cosgamma,tandelphi,sinhphi,coshstheta,sinhstheta
      real*4 t1,ta,p3,t3,hminv2
      real*4 coshsthetaq
      real*4 sind,tand                  ! For f2c
      real*4 p_nonzero
*
*--------------------------------------------------------
*
      if(hsnum_fptrack.gt.0) then       ! Good track has been selected
        hsp = hp_tar(hsnum_tartrack)
        hsenergy = sqrt(hsp*hsp+hpartmass*hpartmass)
*     Copy variables for ntuple so we can test on them
        hsdelta  = hdelta_tar(hsnum_tartrack)
        hsx_tar  = hx_tar(hsnum_tartrack)
        hsy_tar  = hy_tar(hsnum_tartrack)
        hsxp_tar  = hxp_tar(hsnum_tartrack) ! This is an angle (radians)
        hsyp_tar  = hyp_tar(hsnum_tartrack) ! This is an angle (radians)
        hsbeta   = hbeta(hsnum_fptrack)
        hsbeta_chisq = hbeta_chisq(hsnum_fptrack)
        hstime_at_fp   = htime_at_fp(hsnum_fptrack)

        hstrack_et   = htrack_et(hsnum_fptrack)
        hstrack_preshower_e   = htrack_preshower_e(hsnum_fptrack)
        p_nonzero = max(.0001,hsp)      !momentum (used to normalize calorim.)
        hscal_suma = hcal_e1/p_nonzero  !normalized cal. plane sums
        hscal_sumb = hcal_e2/p_nonzero
        hscal_sumc = hcal_e3/p_nonzero
        hscal_sumd = hcal_e4/p_nonzero
        hsprsum = hscal_suma
        hsshsum = hcal_et/p_nonzero
        hsprtrk = hstrack_preshower_e/p_nonzero
        hsshtrk = hstrack_et/p_nonzero

        hsx_fp   = hx_fp(hsnum_fptrack)
        hsy_fp   = hy_fp(hsnum_fptrack)
        hsxp_fp   = hxp_fp(hsnum_fptrack) ! This is a slope (dx/dz)
        hsyp_fp   = hyp_fp(hsnum_fptrack) ! This is a slope (dy/dz)
        hsx_dc1 = hsx_fp + hsxp_fp * hdc_1_zpos
        hsy_dc1 = hsy_fp + hsyp_fp * hdc_1_zpos
        hsx_dc2 = hsx_fp + hsxp_fp * hdc_2_zpos
        hsy_dc2 = hsy_fp + hsyp_fp * hdc_2_zpos
        hsx_s1 = hsx_fp + hsxp_fp * hscin_1x_zpos
        hsy_s1 = hsy_fp + hsyp_fp * hscin_1x_zpos
        hsx_cer = hsx_fp + hsxp_fp * hcer_mirror_zpos
        hsy_cer = hsy_fp + hsyp_fp * hcer_mirror_zpos
        hsx_s2 = hsx_fp + hsxp_fp * hscin_2x_zpos
        hsy_s2 = hsy_fp + hsyp_fp * hscin_2x_zpos
        hsx_cal = hsx_fp + hsxp_fp * hcal_1pr_zpos
        hsy_cal = hsy_fp + hsyp_fp * hcal_1pr_zpos

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

        do ip = 1, hdc_num_planes
          hdc_sing_res(ip)=hdc_single_residual(hsnum_fptrack,ip)
          hsdc_track_coord(ip)=hdc_track_coord(hsnum_fptrack,ip)
        enddo
        hschi2perdeg  = hchi2_fp(hsnum_fptrack)
     $       /float(hnfree_fp(hsnum_fptrack))
        hsnfree_fp = hnfree_fp(hsnum_fptrack)

        cosgamma = 1.0/sqrt(1.0 + hsxp_tar**2 + hsyp_tar**2)
        coshstheta = cosgamma*(sinhthetas * hsyp_tar + coshthetas)
        hstheta = acos(coshstheta)
        sinhstheta = sin(hstheta)
        tandelphi = hsxp_tar /
     &       ( sinhthetas - coshthetas*hsyp_tar)
        hsphi = hphi_lab + tandelphi    ! hphi_lab MUST BE MULTIPLE OF
        sinhphi = sin(hsphi)            ! PI/2, OR ABOVE IS CRAP
*     Calculate elastic scattering kinematics
        t1  = 2.*hphysicsa*cpbeam*coshstheta      
        ta  = 4*cpbeam**2*coshstheta**2 - hphysicsb**2
ccc   SAW 1/17/95.  Add the stuff after the or.
        if(ta.eq.0.0 .or. ( hphysicab2 + hphysicsm3b * ta).lt.0.0) then
          p3=0.       
        else
          t3  = ta-hphysicsb**2
          p3  = (T1 - sqrt( hphysicab2 + hphysicsm3b * ta)) / ta
        endif
*     This is the difference in the momentum obtained by tracking
*     and the momentum from elastic kinematics
        hselas_cor = hsp - P3
*     invariant mass of the remaining particles
        hminv2 =   ( (cebeam+tmass_target-hsenergy)**2
     &       - (cpbeam - hsp * coshstheta)**2
     &       - ( hsp * sinhstheta)**2  )       
        if(hminv2.ge.0 ) then
          hsminv = sqrt(hminv2)
        else
          hsminv = 0.
        endif                           ! end test on positive arg of SQRT
*     hszbeam is the intersection of the beam ray with the spectrometer
*     as measured along the z axis.
        if( sinhstheta .eq. 0.) then
          hszbeam = 0.
        else
          hszbeam = sinhphi * ( -hsy_tar + cyrast * coshstheta) /
     $         sinhstheta 
        endif                           ! end test on sinhstheta=0
*
*     More kinematics
*
        if(hsbeta.gt.0) then
          hsmass2 = (1/hsbeta*2 - 1)*hsp**2
        else
          hsmass2 = 1.0e10
        endif

        hst = (cebeam - hsenergy)**2
     $       - (cpbeam - hsp*coshstheta)**2 - (hsp*sinhstheta)**2
        hsu = (tmass_target - hsenergy)**2 - hsp**2

        hseloss = cebeam - hsenergy
        hsq3 = sqrt(cpbeam**2 + hsp**2 - 2*cpbeam*hsp*coshstheta)
        coshsthetaq = (cpbeam**2 - cpbeam*hsp*coshstheta)/cpbeam/hsq3
        hsthetaq = acos(coshsthetaq)
        hsphiq = hsphi + tt
        hsbigq2 = -hst
        hsx = hsbigq2/(2*mass_nucleon*hseloss)
        hsy = hseloss/cebeam
        hsw2 = tmass_target**2 + 2*tmass_target*hseloss - hsbigq2
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
        if(hdebugdumpcal.ne.0) call h_dump_cal
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
