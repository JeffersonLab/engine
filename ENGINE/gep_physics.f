      subroutine gep_physics(abort,err)

      implicit none
      save

      character*11 here
      parameter(here='gep_physics')

      logical abort
      character*(*) err

      include 'gen_run_info.cmn'
      include 'gen_event_info.cmn'
      include 'gen_data_structures.cmn'
      include 'hms_data_structures.cmn'
      include 'hms_scin_parms.cmn'
      include 'bigcal_data_structures.cmn'
      include 'gep_data_structures.cmn'
      include 'gen_constants.par'
      include 'hms_scin_tof.cmn'
      include 'hms_physics_sing.cmn'
      include 'bigcal_tof_parms.cmn'
      include 'bigcal_gain_parms.cmn'
      include 'bigcal_shower_parms.cmn'
      include 'bigcal_geometry.cmn'
      include 'bigcal_bypass_switches.cmn'
c
c     local variables:
c

c     FIRST PART OF THIS CODE IS TO USE HMS INFO TO SELECT BEST TRACK IN BIGCAL!!!
c     HMS phi is centered at -PI/2, therefore, we need to rotate bigcal phi so
c     that it is centered at +PI/2
c     here we want to use the HMS singles physics info to choose the best track
c     from BigCal assuming elastic kinematics!!!
      integer pick_best_cal_track

c      logical fixed_bigcal

      real hptemp,hthetatemp,hphitemp,E0temp,zbeamtemp
      real bthetatemp,brtemp,bytemp,sinbtmp,cosbtmp

      real pxlabtmp,pylabtmp,pzlabtmp,pztmp,xptartmp

      real etheta_expect,ephi_expect
      real exhat,eyhat,ezhat,exhat_tar,eyhat_tar,ezhat_tar
      real xint_hexpect,yint_hexpect,zint_hexpect
      real xcal_hexpect
      real ycal_hexpect
      real Ecal_hexpect
      real tcal_hexpect
      real Eprime,ethetarad,ephirad,Q2,nu
      real pthetarad,pphirad
      real vx,vy,vz
      real etint
      real edx,edy,edz,ethetacorr,ephicorr,epathlength,mom_corr
      real gamma_corr,beta_corr,tof
      real Q2_cal,Q2_hms,Q2_htheta,nu_htheta,pp_htheta,pp_btheta
      real nu_btheta
      real Ee_btheta 
      real hoffset_ctime,boffset_ctime,htrigt,btrigt,mindiff
      real E0_2body
      real a,b,c

      real Mp
      parameter(Mp=.938272)
      
      real Me

      integer i,ibest_cal,isecond_cal

      real PI
      parameter(PI=3.14159265359)

c     if the user has not defined something reasonable, then set by hand here:
      
      if(GEP_sigma_Ediff.lt..01) then
         GEP_sigma_Ediff = .1
      endif
      if(GEP_sigma_Xdiff.lt..001.or.GEP_sigma_Xdiff.gt.100.0) then
         GEP_sigma_Xdiff = 3.0
      endif
      if(GEP_sigma_Ydiff.lt..001.or.GEP_sigma_Ydiff.gt.100.0) then
         GEP_sigma_Ydiff  =6.0
      endif
      if(GEP_sigma_Tdiff.lt..1) then
         GEP_sigma_Tdiff = 3.0
      endif

      if(gep_sigma_thdiff.lt..01*gep_sigma_xdiff/bigcal_r_tgt.or.
     $     gep_sigma_thdiff.gt.100.*gep_sigma_xdiff/bigcal_r_tgt) then
         gep_sigma_thdiff = gep_sigma_xdiff / bigcal_r_tgt
      endif

      if(gep_sigma_phdiff.lt..01*gep_sigma_ydiff/bigcal_r_tgt.or.
     $     gep_sigma_phdiff.gt.100.*gep_sigma_ydiff/bigcal_r_tgt) then
         gep_sigma_phdiff = gep_sigma_ydiff / bigcal_r_tgt
      endif

      if(gep_sigma_pmiss.lt.1.e-6.or.gep_sigma_pmiss.gt.10.) then
         gep_sigma_pmiss = 2.e-3
      endif

c     if the user hasn't defined something reasonable, set defaults here:
      
      gep_zbeam_low = min(-10.,gep_zbeam_low)
      gep_zbeam_high = max(15.,gep_zbeam_high)

      if(gen_bigcal_mc.eq.3) then ! fill HMS info from Monte Carlo:
         hsnum_fptrack = 1
         hsp = pp_mc
         hsdelta = (hsp - hpcentral)/hpcentral
         hsenergy = sqrt(hsp**2 + Mp**2)
         hstheta = ptheta_mc*PI/180.
         hsphi = (- pphi_mc - 90.)*PI/180.
         hszbeam = zv_p_mc
         gbeam_x = xv_p_mc
         gbeam_y = yv_p_mc
      endif

      if(hsnum_fptrack.le.0.or.bigcal_all_nclust_good.le.0) then
         return
      endif

c     here we compute the expected cluster time in BigCal (using the focal-plane time for the 
c     good track):

      hoffset_ctime = hstime_at_fp - hstart_time_center + 
     $     hspathlength / hsbeta_p 

      mindiff = 0.

      if(ntrigb.gt.0) then
         do i=1,ntrigb
            if(i.eq.1.or.abs(gep_btime(i)-bigcal_window_center).lt.mindiff) then
               btrigt = gep_btime(i)
               mindiff = abs(gep_btime(i)-bigcal_window_center)
            endif
         enddo
      else
         btrigt = bigcal_window_center
      endif

c     invert (common-stop) hms trigger times using the same user parameter as for BigCal:

c$$$      if(ntrigh2.gt.0.and.ntrigh1.eq.0) then 
c$$$         htrigt = bigcal_end_time - gep_h2time(1)
c$$$      else if(ntrigh1.gt.0.and.ntrigh2.eq.0) then
c$$$         htrigt = bigcal_end_time - gep_h1time(1)
c$$$      else if(ntrigh1.gt.0.and.ntrigh2.gt.0) then ! use trig. time 2
c$$$         htrigt = bigcal_end_time - gep_h2time(1)
c$$$      else ! shouldn't hardcode coin. trigger delay, but most data is taken with 
c$$$c     16 ns delay of HMS trigger relative to BigCal trigger. Eventually set up a user param.
c$$$         htrigt = bigcal_end_time - bigcal_window_center + gep_htrig_delay
c$$$      endif
      
c     here are the possible scenarios for the coincidence trigger: 
c     1. BigCal comes in first by about 16 ns (or gep_htrig_delay). This is most likely for good 
c     elastics, but
c     the timing resolution of BigCal is comparatively poor (several ns as opposed to sub-ns 
c     in the case of the hms scintillators), and the tail of the BigCal trigger time peak has some
c     overlap with the HMS self-timing peak.
c     2. the BigCal trigger comes in later than the HMS trigger, and the ADC gate/TDC stop comes 
c     from the BigCal self-timing peak. 
c     Now, suppose we have a good elastic event for which the hms trigger self-times and picks out
c     a track with focal-plane time near hstart_time_center (normal case). Then, we should look for
c     BigCal clusters coming in 16 ns (gep_htrig_delay) earlier than htrig + hoffset_ctime! 
c     Now, suppose we have a good elastic event for which the bigcal trigger self-times and the HMS
c     trigger comes in earlier. IF BigCal self-times, then we also expect the good cluster time to be
c     in the self-timing peak for BigCal, and an earlier HMS time. But essentially, we should always 
c     look for the BigCal cluster 16 ns before the HMS focal-plane time, because that is where our 
c     elastics are:

c      tcal_hexpect = htrigt + hoffset_ctime - gep_htrig_delay
      tcal_hexpect = hoffset_ctime

c$$$      write(*,*) 'tcal_hexpect=',tcal_hexpect
c$$$      write(*,*) 'hoffset_ctime=',hoffset_ctime
c$$$      write(*,*) 'htrigt=',htrigt

      Me = mass_electron ! convenient shorthand

c     calculate nu for elastic-ep:

      nu = sqrt(Mp**2 + hsp**2) - Mp

c     expected electron energy:
      Eprime = gebeam - nu

      pthetarad = hstheta
cajp051408      pphirad = hsphi - 3.*PI/2. ! ~-PI/2.
      
      pphirad = hsphi

c     calculate proton momentum (assuming elastic) from hstheta:

c$$$      Q2_htheta = 4.*Mp**2*gebeam**2*(cos(hstheta))**2 / 
c$$$     $     (Mp**2 + 2.*Mp*gebeam + gebeam**2*(sin(hstheta))**2)
c$$$      nu_htheta = Q2_htheta / (2.*Mp)
c$$$
c$$$      pp_htheta = sqrt(nu_htheta**2 + 2.*Mp*nu_htheta)

      pp_htheta = 2.*Mp*gebeam*(Mp+gebeam)*cos(hstheta) / 
     $     (Mp**2 + 2.*Mp*gebeam + (gebeam*sin(hstheta))**2)

c     calculate electron angle from gebeam and hsp only, since the resolution of these quantities is better than 
c     you can get using hstheta, the reason being the large Jacobian of the reaction. The error on etheta is
c     magnified roughly by a factor hsp/Eprime compared to the error on hstheta, and this in turn gives a 
c     large error on xcal,ycal

      
      etheta_expect = acos(max(-1.,min(1.,1. - Mp/gebeam * nu / Eprime)))
         
      ! in BigCal coordinates, phi is centered at 0 for BigCal. In target coordinates, BigCal is 
      ! centered at +PI/2, while HMS is centered at -PI/2. However, since BigCal y means -target x
      ! we have to be careful. 

c$$$      if(pphirad.gt.0) then 
c$$$         pphirad = pphirad - PI
c$$$      endif
      
      ephi_expect = pphirad + PI ! ~+PI/2.

      ethetarad = etheta_expect 

      gep_etheta_expect_H = ethetarad
      gep_ephi_expect_H = ephi_expect ! ~+PI/2.
     
c     first calculate exhat,eyhat,ezhat in target coordinates so there is no ambiguity: 
      
      exhat_tar = sin(ethetarad)*cos(ephi_expect)
      eyhat_tar = sin(ethetarad)*sin(ephi_expect)
      ezhat_tar = cos(ethetarad)

c     now rotate to BigCal coordinates:
      
      exhat = eyhat_tar
      eyhat = -exhat_tar
      ezhat = ezhat_tar

      !write(*,*) 'exhat,eyhat,ezhat=',exhat,eyhat,ezhat

c     vertex coordinates expressed in BigCal coordinate system
c     turns out that beam x and y coordinates are the same as BigCal coordinates
c     also correct the z vertex position for raster x:

      vx = -gbeam_x
      vy = gbeam_y
      vz = hszbeam

c      vx = gbeam_x
c      vy = gbeam_y
c      vz = hszbeam
      
c      if(gep_use_frx.ne.0) then
c         vx = vx + gfrx
c      endif
      
c      if(gep_use_fry.ne.0) then
c         vy = vy + gfry
c      endif
      
      if(gep_use_xbeam_zcorr.ne.0) then
         vz = vz - vx / tan(htheta_lab*PI/180. - hsyp_tar)
      endif

c     check for reasonable zbeam:
      if(vz.lt.gep_zbeam_low.or.vz.gt.gep_zbeam_high) then 
         vz = 0.
      endif      

      !write(*,*) 'vertex xyz=',vx,vy,vz

c     etint is the trajectory parameter, calculated at the intersection point with the face of BigCal,
c     in other words, if e- position = vertex + et*ehat, where ehat is the unit trajectory vector and et is 
c     the parameter determining where we are on the line, then etint is the value of the parameter when the 
c     electron hits the calorimeter. 
c     so we are calculating the intersection point of the e- trajectory expected from the HMS with BigCal:

      etint = (bigcal_r_tgt-vx*bigcal_sintheta-vz*bigcal_costheta) / 
     $     (exhat * bigcal_sintheta + ezhat*bigcal_costheta)

      xint_hexpect = vx + etint * exhat
      yint_hexpect = vy + etint * eyhat
      zint_hexpect = vz + etint * ezhat

      !write(*,*) 'xint,yint,zint=',xint_hexpect,yint_hexpect,zint_hexpect

c     now rotate into calo-centered coordinate system:

      xcal_hexpect=xint_hexpect*bigcal_costheta-zint_hexpect*bigcal_sintheta
      ycal_hexpect=yint_hexpect
c      tcal_hexpect= hstime_at_fp - hstart_time_center + hspath_cor

c     for now, just take time difference relative to bigcal_window center
c      tcal_hexpect = bigcal_window_center
      
      if(gen_bigcal_mc.eq.3) then
         tcal_hexpect = 0.0
      endif

 173  continue

      gep_bx_expect_H = xcal_hexpect
      gep_by_expect_H = ycal_hexpect

      !write(*,*) 'bigcal e_hms,x_hms,y_hms=',Eprime,xcal_hexpect,ycal_hexpect

c     how to choose? pick the track for which the quadrature sum of 
c     sum( ((Eclust-Eexpect)/sigma)**2 + ((xclust-xexpect)/sigma)**2 + ((yclust-yexpect)/sigma)**2 ) is minimum
c     the resolution parameters sigma should be CTP parms

      if(b_use_bad_chan_list.ne.0) then
c     check bigcal clusters, and, if necessary, fix clusters near the 
c     expected electron position containing channels in the "bad" list

c         fixed_bigcal = .false.
*     fixed_bigcal will be true if any channels from the bad channel list are 
*     filled with a guess and if any cluster is rebuilt or a new cluster is found
         call gep_check_bigcal(gep_bx_expect_H,gep_by_expect_H,Eprime)
      endif

      call b_calc_physics(abort,err) ! reconstruct physics quantities for BigCal. 
c     this routine should only get called once per event!!!!!!!!!!!!!!!!

      call gep_select_best_cluster

*     now pick up where we left off:

      ibest_cal = bigcal_itrack_best

      if(ibest_cal.eq.0) return

      if(bigcal_all_nclust_good.gt.1) then !lw to add second best cluster to gep ntuple

         if(ibest_cal.eq.1) then  
            isecond_cal = 2
         else
            isecond_cal = 1
         endif                    
         GEP_xclust2 = bigcal_all_clstr_x(isecond_cal) ! second best ADC cluster raw xclust in BigCal coordinates
         GEP_yclust2 = bigcal_all_clstr_y(isecond_cal) ! second best ADC cluster raw yclust in BigCal coordinates
         GEP_eclust2 = bigcal_track_energy(isecond_cal)
         GEP_aclust2 = bigcal_all_clstr_atot(isecond_cal) ! second best ADC sum for best cluster
      else
         GEP_xclust2 = -1000.
         GEP_yclust2 = -1000.
         GEP_eclust2 = 0.
         GEP_aclust2 = 0.
      endif                     !lw 

c     correct angles since we know vertex:
      
      edx = bigcal_track_xface(ibest_cal) - vx
      edy = bigcal_track_yface(ibest_cal) - vy
      edz = bigcal_track_zface(ibest_cal) - vz

      epathlength = sqrt(edx**2 + edy**2 + edz**2)

      bigcal_thetarad = acos(edz/epathlength)
      bigcal_phirad = atan2(edy,edx)
      
      bigcal_track_thetarad(ibest_cal) = bigcal_thetarad
      bigcal_track_phirad(ibest_cal) = bigcal_phirad

      bigcal_energy = bigcal_track_energy(ibest_cal)
      
c     correct tof:
      
      gamma_corr = bigcal_energy / Me

c     could get a "NaN" error here: check:
      if(gamma_corr.lt.1.) gamma_corr = 1.

      beta_corr = sqrt(max(0.,1.-1./gamma_corr**2))

      if(beta_corr.eq.0.) beta_corr = 1.
      bigcal_beta = beta_corr
      bigcal_tof = epathlength / (beta_corr*speed_of_light)
      
      mom_corr = beta_corr * bigcal_energy

      bigcal_px = mom_corr * sin(bigcal_thetarad) * cos(bigcal_phirad)
      bigcal_py = mom_corr * sin(bigcal_thetarad) * sin(bigcal_phirad)
      bigcal_pz = mom_corr * cos(bigcal_thetarad)

      bigcal_eloss = bigcal_track_eloss(ibest_cal)
      bigcal_time = bigcal_track_time(ibest_cal)
      bigcal_tof_cor = bigcal_tof - bigcal_tof_central
      bigcal_ctime = bigcal_track_time(ibest_cal) - bigcal_tof_cor - 
     $     (bigcal_end_time - bigcal_window_center)

      gep_ctime_hms = hoffset_ctime
      gep_ctime_cal = bigcal_ctime

      if(gen_bigcal_mc.eq.3) then 
         gep_ctime_hms = 0.
         gep_ctime_cal = 0.
      endif

      Ee_btheta = gebeam / (1. + gebeam/Mp * (1. - cos(bigcal_thetarad)))

      nu_btheta = gebeam - Ee_btheta
      pp_btheta = sqrt(nu_btheta**2 + 2.*Mp*nu_btheta)
c     compute Q2 three different ways:
c     Q2_Cal uses only BigCal information except for hms vertex info
c     Q2_hms uses only HMS information, period.
      Q2_cal = 2.*gebeam*Ee_btheta*(1.-cos(bigcal_thetarad))
      Q2_hms = 2.*Mp*nu ! HMS only

      E0_2body = .5* (nu + sqrt(max(0.,nu**2 + 2.*Q2_hms / (1. - cos(bigcal_thetarad)))))

      GEP_Ebeam_two_body = E0_2body
      
c     what is the average Q2? Both measurements are very good, except for bigcal_energy
c     best is probably to use Eprime calculated from hsp, but use BigCal angle measurement
c     corrected for HMS vertex info. Q2 from Ebeam, hsp alone (Q2_hms) may be even better than this. 
      GEP_Q2 = 2.*gebeam*Eprime*(1.-cos(bigcal_thetarad))
c     GEP_Q2 = .5*(Q2_cal + Q2_hms)
      GEP_Q2_H = Q2_hms
      GEP_Q2_B = Q2_cal
      GEP_E_electron = Eprime ! electron energy from HMS
      GEP_P_proton = hsp
      GEP_Pel_htheta = pp_htheta
      GEP_Pel_btheta = pp_btheta
      GEP_delta_p = hsdelta
      GEP_xfp_p = HSX_FP
      GEP_yfp_p = HSY_FP
      GEP_xpfp_p = HSXP_FP
      GEP_ypfp_p = HSYP_FP
      GEP_xptar_p = HSXP_TAR
      GEP_yptar_p = HSYP_TAR
      GEP_ytar_p = HSY_TAR
      GEP_xbeam = -gbeam_x ! always use this formula for the ntuple, whether or not we use raster in reconstruction
      GEP_ybeam = gbeam_y ! always use this formula for the ntuple, whether or not we use raster in reconstruction
      GEP_xclust = bigcal_all_clstr_x(ibest_cal) ! raw xclust in BigCal coordinates
      GEP_yclust = bigcal_all_clstr_y(ibest_cal) ! raw yclust in BigCal coordinates
      GEP_eclust = bigcal_energy
      GEP_aclust = bigcal_all_clstr_atot(ibest_cal) ! ADC sum for best cluster
      GEP_epsilon = 1./(1.+2.*(1.+GEP_Q2/(4.*Mp**2))*(tan(bigcal_thetarad/2.))**2)
      GEP_etheta_deg = bigcal_thetarad * 180./PI
      GEP_ptheta_deg = hstheta * 180./PI
      GEP_ephi_deg = bigcal_phirad * 180./PI + 90. !~+90 deg
      GEP_pphi_deg = pphirad * 180./PI !~-90 deg

      GEP_Emiss = gebeam + Mp - hsenergy - bigcal_energy
      GEP_Pmissx = -bigcal_py + hsp*sin(hstheta)*cos(pphirad)
      GEP_Pmissy = bigcal_px + hsp*sin(hstheta)*sin(pphirad)
      GEP_Pmissz = gpbeam - bigcal_pz - hsp*cos(hstheta)
      GEP_Pmiss = sqrt(GEP_Pmissx**2 + GEP_Pmissy**2 + GEP_Pmissz**2)
      GEP_W2 = Mp**2 + Q2_hms - Q2_cal ! 2Mnu - Q2_cal
      GEP_Mmiss = sqrt(abs(GEP_W2 - Mp**2))

      return 
      end


