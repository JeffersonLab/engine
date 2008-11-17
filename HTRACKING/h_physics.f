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
*
* $Log$
* Revision 1.23.20.2.2.1  2008/11/17 01:17:55  cdaq
* *** empty log message ***
*
* Revision 1.23.20.2  2007/11/06 19:14:42  cdaq
*  fix zbeam calculation
*
* Revision 1.23.20.1  2007/10/25 00:06:54  cdaq
* *** empty log message ***
*
* Revision 1.23  2003/11/28 14:57:03  jones
* Added variable hsxp_tar_temp = hsxp_tar + h_oopcentral_offset  (MKJ)
*
* Revision 1.22  2003/09/05 18:20:30  jones
* Merge in online03 changes (mkj)
*
* Revision 1.21.2.3  2003/09/04 21:30:12  jones
* Add h_oopcentraloffset (mkj)
*
* Revision 1.21.2.2  2003/07/15 19:04:52  cdaq
* add calculation of hsinplane
*
* Revision 1.21.2.1  2003/04/10 12:39:03  cdaq
* add  e_nonzero and modify p_nonzero.  These are used in calculating E_cal/p and beta.
*
* Revision 1.21  2002/12/27 22:07:04  jones
*    a. Ioana Niculescu modified total_eloss call
*    b. CSA 4/15/99 -- changed hsbeta to hsbeta_p in total_eloss call
*       to yield reasonable calculation for hsbeta=0 events.
*    c. CSA 4/12/99 -- changed hscorre/p back to hsenergy and hsp so
*       I could keep those names in c_physics.f
*
* Revision 1.20  2002/10/02 13:42:43  saw
* Check that user hists are defined before filling
*
* Revision 1.19  1999/02/10 17:45:41  csa
* Cleanup and bugfixes (mostly G. Warren)
*
* Revision 1.18  1996/08/30 19:59:36  saw
* (JRA) Improved track length calculation.  Photon E calc. for (gamma,p)
*
* Revision 1.17  1996/04/30 12:46:06  saw
* (JRA) Add pathlength and rf calculations
*
* Revision 1.16  1996/01/24 15:58:38  saw
* (JRA) Change cpbeam/cebeam to gpbeam/gebeam
*
* Revision 1.15  1996/01/16 21:55:02  cdaq
* (JRA) Calculate q, W for electrons
*
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
      INCLUDE 'hms_id_histid.cmn'
      INCLUDE 'hms_track_histid.cmn'
      include 'gen_event_info.cmn'
      include 'hms_scin_tof.cmn'

*     local variables 

      integer*4 i,ip,ihit
      integer*4 itrkfp
      real*4 coshstheta,sinhstheta
      real*4 p_nonzero,e_nonzero
      real*4 xdist,ydist,dist(12),res(12)
      real*4 tmp,W2
      real*4 hsp_z
      real*4 Wvec(4)
      real*4 hstheta_1st
      real*4 scalar,mink
      real*4 hsxp_tar_temp
*
*--------------------------------------------------------
*
      ierr=0

      if (hsnum_fptrack.le.0) return    ! No Good track 

      itrkfp=hsnum_fptrack

*     Copy variables for ntuple so we can test on them

      hsdelta      = hdelta_tar(hsnum_tartrack)
      hsx_tar      = hx_tar(hsnum_tartrack)
      hsy_tar      = hy_tar(hsnum_tartrack)
      hsxp_tar     = hxp_tar(hsnum_tartrack) ! This is an angle (radians)
      hsxp_tar_temp     = hsxp_tar + h_oopcentral_offset
      hsyp_tar     = hyp_tar(hsnum_tartrack) ! This is an angle (radians)
      hsbeta       = hbeta(itrkfp)
      hsbeta_chisq = hbeta_chisq(itrkfp)
      hstime_at_fp = htime_at_fp(itrkfp)

      hsx_fp  = hx_fp(itrkfp)
      hsy_fp  = hy_fp(itrkfp)
      hsxp_fp = hxp_fp(itrkfp) ! This is a slope (dx/dz)
      hsyp_fp = hyp_fp(itrkfp) ! This is a slope (dy/dz)

*     Correct delta (this must be called AFTER filling 
*     focal plane quantites).

      call h_satcorr(ABORT,err)
      hsp = hpcentral*(1.0 + hsdelta/100.) !Momentum in GeV
      hsenergy = sqrt(hsp*hsp+hpartmass*hpartmass)        

      hstrack_et   = htrack_et(itrkfp)
      hstrack_preshower_e = htrack_preshower_e(itrkfp)
      p_nonzero    = hsp !reconstructed momentum with 'reasonable' limits.
                         !Used to calc. E_cal/p and beta.
      p_nonzero    = max(0.8*hpcentral,p_nonzero)
      p_nonzero    = min(1.2*hpcentral,p_nonzero)
      e_nonzero    = sqrt(p_nonzero**2+hpartmass**2)

      hscal_suma   = hcal_e1/p_nonzero  !normalized cal. plane sums
      hscal_sumb   = hcal_e2/p_nonzero
      hscal_sumc   = hcal_e3/p_nonzero
      hscal_sumd   = hcal_e4/p_nonzero
      hsprsum      = hscal_suma
      hsshsum      = hcal_et/p_nonzero
      hsprtrk      = hstrack_preshower_e/p_nonzero
      hsshtrk      = hstrack_et/p_nonzero

      hsx_sp1      = hx_sp1(itrkfp)
      hsy_sp1      = hy_sp1(itrkfp)
      hsxp_sp1     = hxp_sp1(itrkfp)
      hsx_sp2      = hx_sp2(itrkfp)
      hsy_sp2      = hy_sp2(itrkfp)
      hsxp_sp2     = hxp_sp2(itrkfp)

      if(hidscintimes.gt.0) then
        do ihit=1,hnum_scin_hit(itrkfp)
          call hf1(hidscintimes,hscin_fptime(itrkfp,ihit),1.)
        enddo
      endif

      if(hidcuttdc.gt.0) then
        do ihit=1,hntrack_hits(itrkfp,1)
          call hf1(hidcuttdc,
     &         float(hdc_tdc(hntrack_hits(itrkfp,ihit+1))),1.)
        enddo
      endif

      hsx_dc1 = hsx_fp  +  hsxp_fp * hdc_1_zpos
      hsy_dc1 = hsy_fp  +  hsyp_fp * hdc_1_zpos
      hsx_dc2 = hsx_fp  +  hsxp_fp * hdc_2_zpos
      hsy_dc2 = hsy_fp  +  hsyp_fp * hdc_2_zpos
      hsx_s1  = hsx_fp  +  hsxp_fp * hscin_1x_zpos
      hsy_s1  = hsy_fp  +  hsyp_fp * hscin_1x_zpos
      hsx_cer = hsx_fp  +  hsxp_fp * hcer_mirror_zpos
      hsy_cer = hsy_fp  +  hsyp_fp * hcer_mirror_zpos
      hsx_s2  = hsx_fp  +  hsxp_fp * hscin_2x_zpos
      hsy_s2  = hsy_fp  +  hsyp_fp * hscin_2x_zpos
      hsx_cal = hsx_fp  +  hsxp_fp * hcal_1pr_zpos
      hsy_cal = hsy_fp  +  hsyp_fp * hcal_1pr_zpos

c Used to use hsp, replace with p_nonzero, to give reasonable limits
C (+/-20%) to avoid unreasonable hsbeta_p values
c      hsbeta_p = hsp/max(hsenergy,.00001)

      hsbeta_p = p_nonzero/e_nonzero


C old 'fit' value for pathlen correction
C        hspathlength = -1.47e-2*hsx_fp + 11.6*hsxp_fp - 36*hsxp_fp**2
C new 'modeled' value.

      hspathlength = 12.462*hsxp_fp      + 0.1138*hsxp_fp*hsx_fp
     &              -0.0154*hsx_fp       - 72.292*hsxp_fp**2
     &              -0.0000544*hsx_fp**2 - 116.52*hsyp_fp**2

      hspath_cor = hspathlength/hsbeta_p -
     &      hpathlength_central/speed_of_light*(1/max(.01,hsbeta_p) - 1)

      hsrftime = hmisc_dec_data(49,1)/9.46
     &         - (hstime_at_fp-hstart_time_center) - hspath_cor

      do ip = 1,4
        hsscin_elem_hit(ip) = 0
      enddo

      do i = 1,hnum_scin_hit(itrkfp)
        ip = hscin_plane_num(hscin_hit(itrkfp,i))
        if (hsscin_elem_hit(ip).eq.0) then
          hsscin_elem_hit(ip) = hscin_counter_num(hscin_hit(itrkfp,i))
          hsdedx(ip)          = hdedx(itrkfp,i)
        else                          ! more than 1 hit in plane
          hsscin_elem_hit(ip) = 18
          hsdedx(ip)          = sqrt(hsdedx(ip)*hdedx(itrkfp,i))
        endif
      enddo

      hsnum_scin_hit = hnum_scin_hit(itrkfp)
      hsnum_pmt_hit  = hnum_pmt_hit(itrkfp)

      hschi2perdeg   = hchi2_fp(itrkfp) / float(hnfree_fp(itrkfp))
      hsnfree_fp     = hnfree_fp(itrkfp)

      do ip = 1, hdc_num_planes
        hdc_sing_res(ip)     = hdc_single_residual(itrkfp,ip)
        hsdc_track_coord(ip) = hdc_track_coord(itrkfp,ip)
      enddo

      if (hntrack_hits(itrkfp,1).eq.12 .and. hschi2perdeg.le.4) then
        xdist = hsx_dc1
        ydist = hsy_dc1
        do ip = 1,12
          if (hdc_readout_x(ip)) then
            dist(ip) = ydist*hdc_readout_corr(ip)
          else                        !readout from top/bottom
            dist(ip) = xdist*hdc_readout_corr(ip)
          endif
          res(ip) = hdc_sing_res(ip)
          tmp = hdc_plane_wirecoord(itrkfp,ip)
     $        - hdc_plane_wirecenter(itrkfp,ip)
          if (tmp.eq.0) then          !drift dist = 0
            res(ip) = abs(res(ip))
          else
            res(ip) = res(ip) * (abs(tmp)/tmp) !convert +/- res to near/far res
          endif
        enddo
c       write(37,'(12f7.2,12f8.3,12f8.5)') (hsdc_track_coord(ip),ip=1,12),
c     &           (dist(ip),ip=1,12),(res(ip),ip=1,12)
      endif

*     Do energy loss, which is particle specific

      hstheta_1st = htheta_lab*TT/180. - atan(hsyp_tar) ! rough scat
                                                        ! angle
      hsinplane = htheta_lab*TT/180. - atan(hsyp_tar) ! rough scat angle

      if (hpartmass .lt. 2.*mass_electron) then ! for electron
        if (gtarg_z(gtarg_num).gt.0.) then
          call total_eloss(1,.true.,hstheta_1st,1.0,hseloss)
         else
           hseloss=0.
        endif
      else   ! not an electron
        if (gtarg_z(gtarg_num).gt.0.) then
          call total_eloss(1,.false.,hstheta_1st,hsbeta_p,hseloss)
        else
          hseloss=0.
        endif
      endif           ! particle specific stuff

*     Correct hsenergy and hsp for eloss at the target
* csa 4/12/99 -- changed hscorre/p back to hsenergy and hsp so
* I could keep those names in c_physics.f

      hsenergy = hsenergy + hseloss
      hsp = sqrt(hsenergy**2-hpartmass**2)

*     Begin Kinematic stuff

*     coordinate system :
*     z points downstream along beam
*     x points downward 
*     y points toward beam left (away from HMS)
*
*     This coordinate system is a just a simple rotation away from the
*     TRANSPORT coordinate system used in the spectrometers

      hsp_z = hsp/sqrt(1.+hsxp_tar_temp**2+hsyp_tar**2)
            
*     Initial Electron

      hs_kvec(1) = gebeam  ! after energy loss in target
      hs_kvec(2) = 0
      hs_kvec(3) = 0
      hs_kvec(4) = gebeam 

*     Scattered Particle calculation without small angle approximation
*     - gaw 98/10/5

      hs_kpvec(1) =  hsenergy
      hs_kpvec(2) =  hsp_z*hsxp_tar_temp
      hs_kpvec(3) =  hsp_z*(hsyp_tar*coshthetas-sinhthetas)
      hs_kpvec(4) =  hsp_z*(hsyp_tar*sinhthetas+coshthetas)

*     Angles for Scattered particle. Theta and phi are conventional
*     polar/azimuthal angles defined w.r.t. coordinate system defined
*     above. In rad, of course. Note that phi is around -pi/2 for HMS,
*     +pi/2 for SOS.

      if (abs(hs_kpvec(4)/hsp).le.1.) then
        hstheta = acos(hs_kpvec(4)/hsp)
      else
        hstheta = -10.
      endif
      hsphi = atan2(hs_kpvec(3),hs_kpvec(2))
c      write(*,*)hsphi*57.3, hphi_lab*57.3,htheta_lab
      sinhstheta = sin(hstheta)
      coshstheta = cos(hstheta)
c      write(*,*) ' hsphi = ',hsphi,hphi_lab,hs_kpvec(3),hs_kpvec(2)

      hsphi = hphi_lab + hsphi

c      if (hsphi .gt. 0.) hsphi = hsphi - tt

*     hszbeam is the intersection of the beam ray with the
*     spectrometer as measured along the z axis.

        hszbeam = coshthetas*hsy_tar
     >       /tan(htheta_lab*degree-hsyp_tar)+hsy_tar*sinhthetas

*     Target particle 4-momentum

      hs_tvec(1) = gtarg_mass(gtarg_num)*m_amu
      hs_tvec(2) = 0.
      hs_tvec(3) = 0.
      hs_tvec(4) = 0.

*     Initialize the electron-specific variables

      do i=1,4
         hs_qvec(i) = -1000.
         Wvec(i)    = -1000.
      enddo 

      hsq3     = -1000.
      hsbigq2  = -1000.
      W2       = -1000.
      hinvmass = -1000.

*     Calculate quantities that are meaningful only if 
*     the particle in the HMS is an electron.

      if (hpartmass .lt. 2.*mass_electron) then

         do i=1,4
            hs_qvec(i) = hs_kvec(i) - hs_kpvec(i)
            Wvec(i)    = hs_qvec(i) + hs_tvec(i) ! Q+P 4 vector
         enddo 

*     Magnitudes

         hsq3    = sqrt(scalar(hs_qvec,hs_qvec))
         hsbigq2 = -mink(hs_qvec,hs_qvec) 
         W2      = mink(Wvec,Wvec)
         if(W2.ge.0 ) then
            hinvmass = SQRT(W2)
         else
            hinvmass = 0.
         endif

*     Calculate elastic scattering kinematical correction

*     t1  = 2.*hphysicsa*gpbeam*coshstheta      
*     ta  = 4.*gpbeam**2*coshstheta**2 - hphysicsb**2

*     SAW 1/17/95.  Add the stuff after the or.

*     if(ta.eq.0.0 .or. ( hphysicab2 + hphysicsm3b * ta).lt.0.0) then
*     p3=0.       
*     else
*     t3  = ta-hphysicsb**2
*     p3  = (T1 - sqrt( hphysicab2 + hphysicsm3b * ta)) / ta
*     endif

*     This is the difference in the momentum obtained by tracking
*     and the momentum from elastic kinematics

*     hselas_cor = hsp - P3

      endif

C-----------------------------------------------------------------------
      if (.false.) then
*      if (.true.) then
         write(6,*)' ***********************************'
         write(6,*)' h_phys: htheta_lab, hphi_lab =',htheta_lab,hphi_lab
         write(6,*)' h_phys: hsdelta            =',hsdelta
         write(6,*)' h_phys: hsx_tar, hsy_tar   =',hsx_tar,hsy_tar
         write(6,*)' h_phys: hsxp_tar, hsyp_tar =',hsxp_tar,hsyp_tar
         write(6,*)' h_phys: hsbeta, hsbeta_p   =',hsbeta,hsbeta_p
         write(6,*)' h_phys: hsenergy, hsp      =',hsenergy,hsp
         write(6,*)' h_phys: hseloss            =',hseloss
*         write(6,*)' h_phys: hscorre, hscorrp   =',hscorre,hscorrp
         write(6,*)' h_phys: hstheta_1st        =',hstheta_1st
         write(6,*)' h_phys: hsp_z              =',hsp_z
         write(6,*)' h_phys: hs_kvec            =',hs_kvec
         write(6,*)' h_phys: cos/sinhthetas     =',coshthetas,sinhthetas
         write(6,*)' h_phys: hs_kpvec           =',hs_kpvec
         write(6,*)' h_phys: hs_tvec            =',hs_tvec
         write(6,*)' h_phys: hs_qvec            =',hs_qvec
         write(6,*)' h_phys: Wvec               =',Wvec
         write(6,*)' h_phys: hsq3               =',hsq3
         write(6,*)' h_phys: hsbigq2, W2        =',hsbigq2,W2
         write(6,*)' h_phys: hstheta, hsphi     =',hstheta,hsphi
      endif

*     Write raw timing information for fitting.

      if(hdebugdumptof.ne.0) call h_dump_tof
      if(hdebugdumpcal.ne.0) call h_dump_cal

*     Calculate physics statistics and wire chamber efficencies.

      call h_physics_stat(ABORT,err)
      ABORT= ierr.ne.0 .or. ABORT
      IF(ABORT) THEN
        call G_add_path(here,err)
      ENDIF

      return
      end

***************************************************

      real*4 function scalar(vec1,vec2)

*     scalar product of vec1 and vec2

      real*4 vec1(4)
      real*4 vec2(4)
      integer*4 i
      
      scalar = 0

      do i=2,4
         scalar=vec1(i)*vec2(i)+scalar
      enddo

      return
      end

***************************************************

      real*4 function mink(vec1,vec2)

*     Minkowski product

      implicit none

      real*4 vec1(4),vec2(4)
      real scalar
      
      mink=vec1(1)*vec2(1)-scalar(vec1,vec2)
      return
      end
