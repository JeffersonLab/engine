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
*
*
* $Log$
* Revision 1.19  2002/12/27 22:13:00  jones
*    a. Ioana Niculescu modified total_eloss call
*    b. CSA 4/15/99 -- changed ssbeta to ssbeta_p in total_eloss call
*       to yield reasonable calculation for ssbeta=0 events.
*    c. CSA 4/12/99 -- changed sscorre/p back to ssenergy and ssp so
*       I could keep those names in c_physics.f
*
* Revision 1.18  2002/07/31 20:20:58  saw
* Only try to fill user hists that are defined
*
* Revision 1.17  1999/02/10 17:46:15  csa
* Cleanup and bugfixes (mostly G. Warren)
*
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
      integer ierr
*
      include 'gen_data_structures.cmn'
      INCLUDE 'sos_data_structures.cmn'
      INCLUDE 'gen_routines.dec'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      INCLUDE 'sos_physics_sing.cmn'
      INCLUDE 'sos_calorimeter.cmn'
      INCLUDE 'sos_scin_parms.cmn'
      INCLUDE 'sos_tracking.cmn'
      INCLUDE 'sos_cer_parms.cmn'
      INCLUDE 'sos_geometry.cmn'
      INCLUDE 'sos_id_histid.cmn'
      INCLUDE 'sos_track_histid.cmn'
      include 'gen_event_info.cmn'
      include 'sos_scin_tof.cmn'

*     local variables 

      integer*4 i,ip,ihit
      integer*4 itrkfp
      real*4 cossstheta,sinsstheta
      real*4 p_nonzero
      real*4 xdist,ydist,dist(12),res(12)
      real*4 tmp,W2
      real*4 ssp_z
      real*4 Wvec(4)
      real*4 sstheta_1st
      real*4 scalar,mink
*
*--------------------------------------------------------
*
      ierr=0
      sphi_lab=0.0

      if (ssnum_fptrack.le.0) return    ! No Good track 

      itrkfp=ssnum_fptrack

*     Copy variables for ntuple so we can test on them

      ssdelta      = sdelta_tar(ssnum_tartrack)
      ssx_tar      = sx_tar(ssnum_tartrack)
      ssy_tar      = sy_tar(ssnum_tartrack)
      ssxp_tar     = sxp_tar(ssnum_tartrack) ! This is an angle (radians)
      ssyp_tar     = syp_tar(ssnum_tartrack) ! This is an angle (radians)
      ssbeta       = sbeta(itrkfp)
      ssbeta_chisq = sbeta_chisq(itrkfp)
      sstime_at_fp = stime_at_fp(itrkfp)

      ssx_fp  = sx_fp(itrkfp)
      ssy_fp  = sy_fp(itrkfp)
      ssxp_fp = sxp_fp(itrkfp) ! This is a slope (dx/dz)
      ssyp_fp = syp_fp(itrkfp) ! This is a slope (dy/dz)

*     Correct delta (this must be called AFTER filling 
*     focal plane quantites).

      call s_satcorr(ABORT,err)
      ssp = spcentral*(1.0 + ssdelta/100.) !Momentum in GeV
      ssenergy = sqrt(ssp*ssp+spartmass*spartmass)        

      sstrack_et   = strack_et(itrkfp)
      sstrack_preshower_e = strack_preshower_e(itrkfp)
      p_nonzero    = max(.0001,ssp)      !momentum (used to normalize calorim.)
      sscal_suma   = scal_e1/p_nonzero  !normalized cal. plane sums
      sscal_sumb   = scal_e2/p_nonzero
      sscal_sumc   = scal_e3/p_nonzero
      sscal_sumd   = scal_e4/p_nonzero
      ssprsum      = sscal_suma
      ssshsum      = scal_et/p_nonzero
      ssprtrk      = sstrack_preshower_e/p_nonzero
      ssshtrk      = sstrack_et/p_nonzero

      ssx_sp1      = sx_sp1(itrkfp)
      ssy_sp1      = sy_sp1(itrkfp)
      ssxp_sp1     = sxp_sp1(itrkfp)
      ssx_sp2      = sx_sp2(itrkfp)
      ssy_sp2      = sy_sp2(itrkfp)
      ssxp_sp2     = sxp_sp2(itrkfp)

      do ihit=1,snum_scin_hit(itrkfp)
        if (sidscintimes.gt.0)
     $       call hf1(sidscintimes,sscin_fptime(itrkfp,ihit),1.)
      enddo

      do ihit=1,sntrack_hits(itrkfp,1)
        if (sidcuttdc.gt.0)
     $       call hf1(sidcuttdc,
     &       float(sdc_tdc(sntrack_hits(itrkfp,ihit+1))),1.)
      enddo

      ssx_dc1 = ssx_fp  +  ssxp_fp * sdc_1_zpos
      ssy_dc1 = ssy_fp  +  ssyp_fp * sdc_1_zpos
      ssx_dc2 = ssx_fp  +  ssxp_fp * sdc_2_zpos
      ssy_dc2 = ssy_fp  +  ssyp_fp * sdc_2_zpos
      ssx_s1  = ssx_fp  +  ssxp_fp * sscin_1x_zpos
      ssy_s1  = ssy_fp  +  ssyp_fp * sscin_1x_zpos
      ssx_cer = ssx_fp  +  ssxp_fp * scer_mirror_zpos
      ssy_cer = ssy_fp  +  ssyp_fp * scer_mirror_zpos
      ssx_s2  = ssx_fp  +  ssxp_fp * sscin_2x_zpos
      ssy_s2  = ssy_fp  +  ssyp_fp * sscin_2x_zpos
      ssx_cal = ssx_fp  +  ssxp_fp * scal_1pr_zpos
      ssy_cal = ssy_fp  +  ssyp_fp * scal_1pr_zpos

      ssbeta_p = ssp/max(ssenergy,.00001)

C old 'fit' value for pathlen correction
C        sspathlength = 2.78*ssxp_fp - 3.5*ssxp_fp**2 + 2.9e-3*ssy_fp
C new 'modeled' value.

      sspathlength = 2.923*ssxp_fp - 6.1065*ssxp_fp**2
     &     +0.006908*ssx_fp*ssxp_fp + 0.001225*ssx_fp
     &     -0.0000324*ssx_fp**2 -21.936*ssyp_fp**2

      sspath_cor = sspathlength/ssbeta_p -
     &     spathlength_central/speed_of_light*(1/max(.01,ssbeta_p) - 1)

      ssrftime = smisc_dec_data(8,1)/9.68
     &     - (sstime_at_fp-sstart_time_center) - sspath_cor

      do ip = 1,4
        ssscin_elem_hit(ip) = 0
      enddo

      do i = 1,snum_scin_hit(itrkfp)
        ip = sscin_plane_num(sscin_hit(itrkfp,i))
        if (ssscin_elem_hit(ip).eq.0) then
          ssscin_elem_hit(ip) = sscin_counter_num(sscin_hit(itrkfp,i))
          ssdedx(ip)          = sdedx(itrkfp,i)
        else                          ! more than 1 hit in plane
          ssscin_elem_hit(ip) = 18
          ssdedx(ip)          = sqrt(ssdedx(ip)*sdedx(itrkfp,i))
        endif
      enddo

      ssnum_scin_hit = snum_scin_hit(itrkfp)
      ssnum_pmt_hit  = snum_pmt_hit(itrkfp)

      sschi2perdeg   = schi2_fp(itrkfp) / float(snfree_fp(itrkfp))
      ssnfree_fp     = snfree_fp(itrkfp)

      do ip = 1, sdc_num_planes
        sdc_sing_res(ip)     = sdc_single_residual(itrkfp,ip)
        ssdc_track_coord(ip) = sdc_track_coord(itrkfp,ip)
      enddo

      if (sntrack_hits(itrkfp,1).eq.12 .and. sschi2perdeg.le.4) then
        xdist = ssx_dc1
        ydist = ssy_dc1
        do ip = 1,12
          if (sdc_readout_x(ip)) then
            dist(ip) = ydist*sdc_readout_corr(ip)
          else                        !readout from top/bottom
            dist(ip) = xdist*sdc_readout_corr(ip)
          endif
          res(ip) = sdc_sing_res(ip)
          tmp = sdc_plane_wirecoord(itrkfp,ip)
     $        - sdc_plane_wirecenter(itrkfp,ip)
          if (tmp.eq.0) then          !drift dist = 0
            res(ip) = abs(res(ip))
          else
            res(ip) = res(ip) * (abs(tmp)/tmp) !convert +/- res to near/far res
          endif
        enddo
c       write(37,'(12f7.2,12f8.3,12f8.5)') (ssdc_track_coord(ip),ip=1,12),
c     &           (dist(ip),ip=1,12),(res(ip),ip=1,12)
      endif

*     Do energy loss, which is particle specific

      sstheta_1st = stheta_lab*TT/180. + atan(ssyp_tar) ! rough scat angle

      if (spartmass .lt. 2.*mass_electron) then ! for electron
        if (gtarg_z(gtarg_num).gt.0.) then
          call total_eloss(2,.true.,sstheta_1st,1.0,sseloss)
         else
           sseloss=0.
        endif
      else   ! not an electron
        if (gtarg_z(gtarg_num).gt.0.) then
          call total_eloss(2,.false.,sstheta_1st,ssbeta_p,sseloss)
        else
          sseloss=0.
        endif
      endif           ! particle specific stuff

*     Correct ssenergy and ssp for eloss at the target
* csa 4/12/99 -- changed sscorre/p back to ssenergy and ssp so
* I could keep those names in c_physics.f

      ssenergy = ssenergy + sseloss
      ssp = sqrt(ssenergy**2-spartmass**2)

*     Begin Kinematic stuff

*     coordinate system :
*     z points downstream along beam
*     x points downward 
*     y points toward beam left (away from HMS)
*
*     This coordinate system is a just a simple rotation away from the
*     TRANSPORT coordinate system used in the spectrometers

      ssp_z = ssp/sqrt(1.+ssxp_tar**2+ssyp_tar**2)
            
*     Initial Electron

      ss_kvec(1) = gebeam  ! after energy loss in target
      ss_kvec(2) = 0
      ss_kvec(3) = 0
      ss_kvec(4) = gebeam 

*     Scattered Electron (not meaningful if hadron is in SOS!)
*     calculation without small angle approximation - gaw 98/10/5 csa
*     12/21/98 -- notice assumption of no out-of-plane offset

      ss_kpvec(1) =  ssenergy
      ss_kpvec(2) =  ssp_z*ssxp_tar
      ss_kpvec(3) =  ssp_z*(ssyp_tar*cossthetas+sinsthetas)
      ss_kpvec(4) =  ssp_z*(-ssyp_tar*sinsthetas+cossthetas)

*     Angles for Scattered particle. Theta and phi are conventional
*     polar/azimuthal angles defined w.r.t. coordinate system defined
*     above. In rad, of course. Note that phi is around -pi/2 for HMS,
*     +pi/2 for SOS.

      if (abs(ss_kpvec(4)/ssp).le.1.) then
        sstheta = acos(ss_kpvec(4)/ssp)
      else
        sstheta = -10.
      endif

      ssphi = atan(ss_kpvec(3)/ss_kpvec(2))

      sinsstheta = sin(sstheta)
      cossstheta = cos(sstheta)

      ssphi = sphi_lab + ssphi
      if (ssphi .lt. 0.) ssphi = ssphi + tt

*     sszbeam is the intersection of the beam ray with the
*     spectrometer as measured along the z axis.

      if( sinsstheta .eq. 0.) then
        sszbeam = 0.
      else
        sszbeam = sin(ssphi) * ( -ssy_tar + gbeam_y * cossstheta) /
     $         sinsstheta 
      endif                           ! end test on sinsstheta=0

*     Target particle 4-momentum

      ss_tvec(1) = gtarg_mass(gtarg_num)*m_amu
      ss_tvec(2) = 0.
      ss_tvec(3) = 0.
      ss_tvec(4) = 0.

*     Initialize the electron-specific variables

      do i=1,4
         ss_qvec(i) = -1000.
         Wvec(i)    = -1000.
      enddo 

      ssq3     = -1000.
      ssbigq2  = -1000.
      W2       = -1000.
      sinvmass = -1000.

*     Calculate quantities that are meaningful only if 
*     the particle in the SOS is an electron.

      if (spartmass .lt. 2.*mass_electron) then

         do i=1,4
            ss_qvec(i) = ss_kvec(i) - ss_kpvec(i)
            Wvec(i)    = ss_qvec(i) + ss_tvec(i) ! Q+P 4 vector
         enddo 

*     Magnitudes

         ssq3    = sqrt(scalar(ss_qvec,ss_qvec))
         ssbigq2 = -mink(ss_qvec,ss_qvec) 
         W2      = mink(Wvec,Wvec)
         if(W2.ge.0 ) then
            sinvmass = SQRT(W2)
         else
            sinvmass = 0.
         endif

*     Calculate elastic scattering kinematical correction

*     t1  = 2.*sphysicsa*gpbeam*cossstheta      
*     ta  = 4.*gpbeam**2*cossstheta**2 - sphysicsb**2

*     SAW 1/17/95.  Add the stuff after the or.

*     if(ta.eq.0.0 .or. ( sphysicab2 + sphysicsm3b * ta).lt.0.0) then
*     p3=0.       
*     else
*     t3  = ta-sphysicsb**2
*     p3  = (T1 - sqrt( sphysicab2 + sphysicsm3b * ta)) / ta
*     endif

*     This is the difference in the momentum obtained by tracking
*     and the momentum from elastic kinematics

*     sselas_cor = ssp - P3

      endif

      if (.false.) then
*      if (.true.) then
         write(6,*)' ***********************************'
         write(6,*)' s_phys: stheta_lab, sphi_lab =',stheta_lab,sphi_lab
         write(6,*)' s_phys: ssdelta            =',ssdelta
         write(6,*)' s_phys: ssx_tar, ssy_tar   =',ssx_tar,ssy_tar
         write(6,*)' s_phys: ssxp_tar, ssyp_tar =',ssxp_tar,ssyp_tar
         write(6,*)' s_phys: ssbeta, ssbeta_p   =',ssbeta,ssbeta_p
         write(6,*)' s_phys: ssenergy, ssp      =',ssenergy,ssp
         write(6,*)' s_phys: sseloss            =',sseloss
*         write(6,*)' s_phys: sscorre, sscorrp   =',sscorre,sscorrp
         write(6,*)' s_phys: sstheta_1st        =',sstheta_1st
         write(6,*)' s_phys: ssp_z              =',ssp_z
         write(6,*)' s_phys: ss_kvec            =',ss_kvec
         write(6,*)' s_phys: cos/sinsthetas     =',cossthetas,sinsthetas
         write(6,*)' s_phys: ss_kpvec           =',ss_kpvec
         write(6,*)' s_phys: ss_tvec            =',ss_tvec
         write(6,*)' s_phys: ss_qvec            =',ss_qvec
         write(6,*)' s_phys: Wvec               =',Wvec
         write(6,*)' s_phys: ssq3               =',ssq3
         write(6,*)' s_phys: ssbigq2, W2        =',ssbigq2,W2
         write(6,*)' s_phys: sstheta, ssphi     =',sstheta,ssphi
      endif

*     Write raw timing information for fitting.

      if(sdebugdumptof.ne.0) call s_dump_tof
      if(sdebugdumpcal.ne.0) call s_dump_cal

*     Calculate physics statistics and wire chamber efficencies.

      call s_physics_stat(ABORT,err)
      ABORT= ierr.ne.0 .or. ABORT
      IF(ABORT) THEN
        call G_add_path(here,err)
      ENDIF

      return
      end
