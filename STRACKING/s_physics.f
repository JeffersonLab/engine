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
* Revision 1.16  1996/11/07 19:51:38  saw
* (JRA) Correct error in mass calculation
*
* Revision 1.15  1996/09/05 20:13:14  saw
* (JRA) Improved track length calculation.  Photon E calc. for (gamma,p)
*
* Revision 1.14  1996/04/30 17:13:48  saw
* (JRA) Add pathlength and rf calculations
*
* Revision 1.13  1996/01/24 16:08:14  saw
* (JRA) Change cpbeam/cebeam to gpbeam/gebeam
*
* Revision 1.12  1996/01/17 19:00:50  cdaq
* (JRA) Calculate q, W for electrons
*
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
      INCLUDE 'sos_id_histid.cmn'
      INCLUDE 'sos_track_histid.cmn'
      INCLUDE 'gen_event_info.cmn'
      INCLUDE 'sos_scin_tof.cmn'
*     
*     local variables 
      integer*4 i,ip,ihit
      integer*4 itrkfp
      real*4 xdist,ydist,dist(12),res(12)
      real*4 tmp
      real*4 cosgamma,tandelphi,sinsphi,cossstheta,sinsstheta
      real*4 t1,ta,p3,t3,sminv2
      real*4 cosssthetaq
      real*4 sind,tand
      real*4 p_nonzero,W2
      real*4 denom
*     
*--------------------------------------------------------
*
      ierr=0
      sphi_lab=0.0
      if(ssnum_fptrack.gt.0) then       ! Good track has been selected
        itrkfp=ssnum_fptrack
        ssp = sp_tar(ssnum_tartrack)
        ssenergy = sqrt(ssp*ssp+spartmass*spartmass)
*     Copy variables for ntuple so we can test on them
        ssdelta  = sdelta_tar(ssnum_tartrack)
        ssx_tar  = sx_tar(ssnum_tartrack)
        ssy_tar  = sy_tar(ssnum_tartrack)
        ssxp_tar  = sxp_tar(ssnum_tartrack) ! This is an angle (radians)
        ssyp_tar  = syp_tar(ssnum_tartrack) ! This is an angle (radians)
        ssbeta   = sbeta(itrkfp)
        ssbeta_chisq = sbeta_chisq(itrkfp)
        sstime_at_fp   = stime_at_fp(itrkfp)

        sstrack_e1   = strack_e1(itrkfp)
        sstrack_e2   = strack_e2(itrkfp)
        sstrack_e3   = strack_e3(itrkfp)
        sstrack_e4   = strack_e4(itrkfp)
        sstrack_et   = strack_et(itrkfp)
        sstrack_preshower_e   = strack_preshower_e(itrkfp)
        p_nonzero = max(.0001,ssp)      !momentum (used to normalize calorim.)
        sscal_suma = scal_e1/p_nonzero  !normalized cal. plane sums
        sscal_sumb = scal_e2/p_nonzero
        sscal_sumc = scal_e3/p_nonzero
        sscal_sumd = scal_e4/p_nonzero
        ssprsum = sscal_suma
        ssshsum = scal_et/p_nonzero
        ssprtrk = sstrack_preshower_e/p_nonzero
        ssshtrk = sstrack_et/p_nonzero
        ssshtrk3 = (sstrack_e1+sstrack_e2+sstrack_e3)/p_nonzero

        ssx_sp1=sx_sp1(itrkfp)
        ssy_sp1=sy_sp1(itrkfp)
        ssxp_sp1=sxp_sp1(itrkfp)
        ssx_sp2=sx_sp2(itrkfp)
        ssy_sp2=sy_sp2(itrkfp)
        ssxp_sp2=sxp_sp2(itrkfp)

        do ihit=1,snum_scin_hit(itrkfp)
          call hf1(sidscintimes,sscin_fptime(itrkfp,ihit),1.)
        enddo

        do ihit=1,sntrack_hits(itrkfp,1)
          call hf1(sidcuttdc,
     &       float(sdc_tdc(sntrack_hits(itrkfp,ihit+1))),1.)
        enddo

        ssx_fp   = sx_fp(itrkfp)
        ssy_fp   = sy_fp(itrkfp)
        ssxp_fp   = sxp_fp(itrkfp) ! This is a slope (dx/dz)
        ssyp_fp   = syp_fp(itrkfp) ! This is a slope (dy/dz)
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

        ssbeta_p = ssp/max(ssenergy,.00001)
C old 'fit' value for pathlen correction
C        sspathlength = 2.78*ssxp_fp - 3.5*ssxp_fp**2 + 2.9e-3*ssy_fp
C new 'modeled' value.
        sspathlength = 2.923*ssxp_fp - 6.1065*ssxp_fp**2
     &                +0.006908*ssx_fp*ssxp_fp + 0.001225*ssx_fp
     &                -0.0000324*ssx_fp**2 -21.936*ssyp_fp**2
        sspath_cor = sspathlength/ssbeta_p -
     &      spathlength_central/speed_of_light*(1/max(.01,ssbeta_p) - 1)

        ssrftime = smisc_dec_data(8,1)/9.68
     &           - (sstime_at_fp-sstart_time_center) - sspath_cor

        do ip=1,4
          ssscin_elem_hit(ip)=0
        enddo
        do i=1,snum_scin_hit(itrkfp)
          ip=sscin_plane_num(sscin_hit(itrkfp,i))
          if (ssscin_elem_hit(ip).eq.0) then
            ssscin_elem_hit(ip)=sscin_counter_num(sscin_hit(
     $           itrkfp,i))
            ssdedx(ip) = sdedx(itrkfp,i)
          else                          ! more than 1 hit in plane
            ssscin_elem_hit(ip)=18
            ssdedx(ip) = sqrt(ssdedx(ip)*sdedx(itrkfp,i))  !assume <3 hits. 
          endif
        enddo                             

        ssnum_scin_hit = snum_scin_hit(itrkfp)
        ssnum_pmt_hit = snum_pmt_hit(itrkfp)

        sschi2perdeg  = schi2_fp(itrkfp)
     $       /float(snfree_fp(itrkfp))
        ssnfree_fp = snfree_fp(itrkfp)

        do ip = 1 , sdc_num_planes
          sdc_sing_res(ip)=sdc_single_residual(itrkfp,ip)
          ssdc_track_coord(ip)=sdc_track_coord(itrkfp,ip)
        enddo


        if (sntrack_hits(itrkfp,1).eq.12 .and. sschi2perdeg.le.2) then
          xdist=ssx_dc1
          ydist=ssy_dc1
          do ip=1,12
            if (sdc_readout_x(ip)) then
              dist(ip) = ydist*sdc_readout_corr(ip)
            else                        !readout from top/bottom
              dist(ip) = xdist*sdc_readout_corr(ip)
            endif
            res(ip)=sdc_sing_res(ip)
            tmp = sdc_plane_wirecoord(itrkfp,ip)-sdc_plane_wirecenter(itrkfp,ip)
            if (tmp.eq.0) then          !drift dist = 0
              res(ip)=abs(res(ip))
            else
              res(ip)=res(ip) * (abs(tmp)/tmp) !convert +/- res to near/far res
            endif
          enddo
c          write(38,'(12f7.2,12f8.3,12f8.5)') (ssdc_track_coord(ip),ip=1,12),
c     &    (dist(ip),ip=1,12),(res(ip),ip=1,12)
        endif
        SSP = spcentral*(1 + ssdelta/100.)
c        cosgamma = 1.0/sqrt(1.0 + ssxp_tar**2 + ssyp_tar**2)
c        cossstheta = cosgamma*(sinsthetas * ssyp_tar + cossthetas)
c        sstheta = acos(cossstheta)
        sstheta = stheta_lab*TT/180. + ssyp_tar
        sinsstheta = sin(sstheta)
        cossstheta = cos(sstheta)
        tandelphi = ssxp_tar /
     &       ( sinsthetas - cossthetas*ssyp_tar )
        ssphi = sphi_lab + atan(tandelphi)    ! phi_lab MUST BE MULTPIPLE OF
        sinsphi = sin(ssphi)            ! PI/2, OR ABOVE IS CRAP


        if(spartmass .lt. 2*mass_electron) then ! Less than
          if(gtarg_z(gtarg_num).gt.0.)then
            call total_eloss(2,.true.,gtarg_z(gtarg_num),
     $           gtarg_a(gtarg_num),gtarg_thick(gtarg_num),
     $           gtarg_dens(gtarg_num),
     $           sstheta,gtarg_theta,1.0,sseloss)
            SSENERGY = SSENERGY - sseloss
          else
            sseloss=0.
          endif
          sqx = -SSP*cos(SSxp_tar)*sinsstheta
          sqy = -SSP*sin(Ssxp_tar)
          sqz = gpbeam - SSP*cos(SSxp_tar)*cossstheta
          sqabs= sqrt(sqx**2+sqy**2+sqz**2)
          W2 = gtarg_mass(gtarg_num)**2 +
     $         2.*gtarg_mass(gtarg_num)*(gpbeam-ssp) - sqabs**2
     $         + (gpbeam-ssp)**2
          if(W2.ge.0 ) then
            SINVMASS = SQRT(W2)
          else
            SINVMASS = 0.
          endif
        else
          if(gtarg_z(gtarg_num).gt.0.)then
            call total_eloss(2,.false.,gtarg_z(gtarg_num),
     $           gtarg_a(gtarg_num),
     $           gtarg_thick(gtarg_num),gtarg_dens(gtarg_num),
     $           sstheta,gtarg_theta,ssbeta,sseloss)
            SSENERGY = SSENERGY - sseloss
          else
            sseloss=0.
          endif
        endif

*     Calculate elastic scattering kinematics
        t1  = 2.*sphysicsa*gpbeam*cossstheta      
        ta  = 4*gpbeam**2*cossstheta**2 - sphysicsb**2
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
        sminv2 =   ( (gebeam+gtarg_mass(gtarg_num)-ssenergy)**2
     &       - (gpbeam - ssp * cossstheta)**2
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
          sszbeam = sinsphi * ( -ssy_tar + gbeam_y * cossstheta) /
     $         sinsstheta 
        endif                           ! end test on sinsstheta=0
*
*     More kinematics
*
        if(ssbeta.gt.0) then
          ssmass2 = (1/ssbeta**2 - 1)*ssp**2
        else
          ssmass2 = 1.0E10
        endif

        sst = (gebeam - ssenergy)**2
     $       - (gpbeam - ssp*cossstheta)**2 - (ssp*sinsstheta)**2
        ssu = (gtarg_mass(gtarg_num) - ssenergy)**2 - ssp**2
        if(sseloss.eq.0.)then
          sseloss = gebeam - ssenergy
        endif
        ssq3 = sqrt(gpbeam**2 + ssp**2 - 2*gpbeam*ssp*cossstheta)
        if(gpbeam.ne.0.and.ssq3.ne.0.) then
          cosssthetaq = (gpbeam**2 - gpbeam*ssp*cossstheta)/gpbeam/SSQ3
        endif
        if(cosssthetaq.le.1.and.cosssthetaq.ge.-1.) then
          ssthetaq = acos(cosssthetaq)
        endif
        ssphiq = ssphi + tt
        ssbigq2 = -sst
        ssx = ssbigq2/(2*mass_nucleon*sseloss)
        ssy = sseloss/gebeam
        ssw2 = gtarg_mass(gtarg_num)**2 +
     $       2*gtarg_mass(gtarg_num)*sseloss - ssbigq2
        if(ssw2.ge.0.0) then
          ssw = sqrt(ssw2)
        else
          ssw = 0.0
        endif

*     Calculate photon energy in GeV(E89-012):
        denom = sphoto_mtarget - ssenergy + ssp*cossstheta
        if (abs(denom).le.1.e-10) then
           ssegamma = -1000.0
        else
           ssegamma = ( ssenergy * sphoto_mtarget
     &       - 0.5*(sphoto_mtarget**2 + spartmass**2
     &       - sphoto_mrecoil**2) ) / denom
        end if

*     Photon energy (assuming D2 target, Proton detected).
        denom = 1.87561 - sqrt(ssp**2 + 0.93827**2) + ssp*cossstheta
        ssegamma_p= ( sqrt(ssp**2+0.93827**2) * 1.87561
     &       - 0.5*(1.87561**2 + 0.93827**2 - 0.93957**2) )/denom


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
      return
      end
