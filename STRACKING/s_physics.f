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
* Revision 1.11  1995/10/10 12:54:30  cdaq
* (JRA) Add call to s_dump_cal, change upper to lower case
*
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
      real*4 p_nonzero
*     
*--------------------------------------------------------
*
      if(ssnum_fptrack.gt.0) then       ! Good track has been selected
        ssp = sp_tar(ssnum_tartrack)
        ssenergy = sqrt(ssp*ssp+spartmass*spartmass)
*     Copy variables for ntuple so we can test on them
        ssdelta  = sdelta_tar(ssnum_tartrack)
        ssx_tar  = sx_tar(ssnum_tartrack)
        ssy_tar  = sy_tar(ssnum_tartrack)
        ssxp_tar  = sxp_tar(ssnum_tartrack) ! This is an angle (radians)
        ssyp_tar  = syp_tar(ssnum_tartrack) ! This is an angle (radians)
        ssbeta   = sbeta(ssnum_fptrack)
        ssbeta_chisq = sbeta_chisq(ssnum_fptrack)
        sstime_at_fp   = stime_at_fp(ssnum_fptrack)

        sstrack_et   = strack_et(ssnum_fptrack)
        sstrack_preshower_e   = strack_preshower_e(ssnum_fptrack)
        p_nonzero = max(.0001,ssp)      !momentum (used to normalize calorim.)
        sscal_suma = scal_e1/p_nonzero  !normalized cal. plane sums
        sscal_sumb = scal_e2/p_nonzero
        sscal_sumc = scal_e3/p_nonzero
        sscal_sumd = scal_e4/p_nonzero
        ssprsum = sscal_suma
        ssshsum = scal_et/p_nonzero
        ssprtrk = sstrack_preshower_e/p_nonzero
        ssshtrk = sstrack_et/p_nonzero

        ssx_fp   = sx_fp(ssnum_fptrack)
        ssy_fp   = sy_fp(ssnum_fptrack)
        ssxp_fp   = sxp_fp(ssnum_fptrack) ! This is a slope (dx/dz)
        ssyp_fp   = syp_fp(ssnum_fptrack) ! This is a slope (dy/dz)
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
          ssdc_track_coord(ip)=sdc_track_coord(ssnum_fptrack,ip)
        enddo

        sschi2perdeg  = schi2_fp(ssnum_fptrack)
     $       /float(snfree_fp(ssnum_fptrack))
        ssnfree_fp = snfree_fp(ssnum_fptrack)
        cosgamma = 1.0/sqrt(1.0 + ssxp_tar**2 + ssyp_tar**2)
        cossstheta = cosgamma*(sinsthetas * ssyp_tar + cossthetas)
        sstheta = acos(cossstheta)
        sinsstheta = sin(sstheta)
        tandelphi = ssxp_tar /
     &       ( sinsthetas - cossthetas*ssyp_tar )
        ssphi = sphi_lab + tandelphi    ! phi_lab MUST BE MULTPIPLE OF
        sinsphi = sin(ssphi)            ! PI/2, OR ABOVE IS CRAP
*     Calculate elastic scattering kinematics
        t1  = 2.*sphysicsa*cpbeam*cossstheta      
        ta  = 4*cpbeam**2*cossstheta**2 - sphysicsb**2
        if(ta.eq.0 .OR. ( sphysicab2 + sphysicsm3b * ta).lt.0.0) then
          p3=0.       
        else
          t3  = ta-sphysicsb**2
          p3  = (t1 - SQRT( sphysicab2 + sphysicsm3b * ta)) / ta
        endif
*     This is the difference in the momentum obtained by tracking
*     and the momentum from elastic kinematics
        sselas_cor = ssp - p3
*     invariant mass of the remaining particles
        sminv2 =   ( (cebeam+tmass_target-ssenergy)**2
     &       - (cpbeam - ssp * cossstheta)**2
     &       - ( ssp * sinsstheta)**2  )       
        if(sminv2.ge.0 ) then
          ssminv = SQRT(sminv2)
        else
          ssminv = 0.
        endif                           ! end test on positive arg of SQRT
*     sszbeam is the intersection of the beam ray with the spectrometer
*     as measured along the z axis.
        if( sinsstheta .eq. 0.) then
          sszbeam = 0.
        else
          sszbeam = sinsphi * ( -ssy_tar + cyrast * cossstheta) /
     $         sinsstheta 
        endif                           ! end test on sinsstheta=0
*
*     More kinematics
*
        if(ssbeta.gt.0) then
          ssmass2 = (1/ssbeta*2 - 1)*ssp**2
        else
          ssmass2 = 1.0E10
        endif

        sst = (cebeam - ssenergy)**2
     $       - (cpbeam - ssp*cossstheta)**2 - (ssp*sinsstheta)**2
        ssu = (tmass_target - ssenergy)**2 - ssp**2

        sseloss = cebeam - ssenergy
        ssq3 = sqrt(cpbeam**2 + ssp**2 - 2*cpbeam*ssp*cossstheta)
        cosssthetaq = (cpbeam**2 - cpbeam*ssp*cossstheta)/cpbeam/SSQ3
        ssthetaq = acos(cosssthetaq)
        ssphiq = ssphi + tt
        ssbigq2 = -sst
        ssx = ssbigq2/(2*mass_nucleon*sseloss)
        ssy = sseloss/cebeam
        ssw2 = tmass_target**2 + 2*tmass_target*sseloss - ssbigq2
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
        if(sdebugdumpcal.ne.0) call s_dump_cal
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
