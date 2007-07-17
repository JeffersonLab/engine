      subroutine gep_physics(abort,err)

      implicit none
      save

      character*11 here
      parameter(here='gep_physics')

      logical abort
      character*(*) err
      
      include 'gen_data_structures.cmn'
      include 'hms_data_structures.cmn'
      include 'bigcal_data_structures.cmn'
      include 'gep_data_structures.cmn'
      include 'gen_constants.par'
      include 'hms_scin_tof.cmn'
      include 'hms_physics_sing.cmn'
      include 'hms_scin_parms.cmn'
      include 'bigcal_tof_parms.cmn'
      include 'bigcal_gain_parms.cmn'
      include 'bigcal_shower_parms.cmn'
      include 'bigcal_geometry.cmn'

c
c     local variables:
c

c     FIRST PART OF THIS CODE IS TO USE HMS INFO TO SELECT BEST TRACK IN BIGCAL!!!
c     HMS phi is centered at -PI/2, therefore, we need to rotate bigcal phi so
c     that it is centered at +PI/2
c     here we want to use the HMS singles physics info to choose the best track
c     from BigCal assuming elastic kinematics!!!
      integer pick_best_cal_track

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
      real Q2_cal,Q2_hms

      real Mp
      parameter(Mp=.938272)
      
      real Me
      integer i,ibest_cal

      real PI
      parameter(PI=3.14159265359)

      Me = mass_electron ! convenient shorthand

      nu = sqrt(Mp**2 + hsp**2) - Mp
 
      Eprime = gebeam - nu
      Ecal_hexpect = Eprime

      pthetarad = hstheta
      pphirad = hsphi

c     calculate electron angle from gebeam and hsp only, since the resolution of these quantities is better than 
c     you can get using hstheta, the reason being the large Jacobian of the reaction. The error on etheta is
c     magnified roughly by a factor hsp/Eprime compared to the error on hstheta, and this in turn gives a 
c     large error on xcal,ycal

      etheta_expect = acos(1. - Mp/gebeam * nu / Eprime)
      
      ! in BigCal coordinates, phi is centered at 0 for BigCal. In target coordinates, BigCal is 
      ! centered at +PI/2, while HMS is centered at -PI/2. However, since BigCal y means -target x
      ! we have to be careful. 

      ephi_expect = pphirad + PI

      ethetarad = etheta_expect 
     
c     first calculate exhat,eyhat,ezhat in target coordinates so there is no ambiguity: 
      
      exhat_tar = sin(ethetarad)*cos(ephi_expect)
      eyhat_tar = sin(ethetarad)*sin(ephi_expect)
      ezhat_tar = cos(ethetarad)

c     now rotate to BigCal coordinates:
      
      exhat = eyhat_tar
      eyhat = -exhat_tar
      ezhat = ezhat_tar

c     vertex coordinates expressed in BigCal coordinate system

      vz = hszbeam ! along beamline
      vx = gbeam_y ! horizontal toward BigCal
      vy = -gbeam_x ! vertical up (target x is vertical down.)

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

c     now rotate into calo-centered coordinate system:

      xcal_hexpect=xint_hexpect*bigcal_costheta-zint_hexpect*bigcal_sintheta
      ycal_hexpect=yint_hexpect
      tcal_hexpect= hstime_at_fp - hstart_time_center + hspath_cor

c     how to choose? pick the track for which the quadrature sum of 
c     sum( ((Eclust-Eexpect)/sigma)**2 + ((xclust-xexpect)/sigma)**2 + ((yclust-yexpect)/sigma)**2 ) is minimum
c     the resolution parameters sigma should be CTP parms

      ibest_cal = pick_best_cal_track(tcal_hexpect,xcal_hexpect,ycal_hexpect,Ecal_hexpect)

c     now compute "missing" quantities using the track we have selected.
c     first correct best calo track for vertex information which we now know from HMS reconstruction:

      bigcal_itrack_best = ibest_cal

c     correct angles since we know vertex:
      
      edx = bigcal_track_xface(ibest_cal) - vx
      edy = bigcal_track_yface(ibest_cal) - vy
      edz = bigcal_track_zface(ibest_cal) - vz

      epathlength = sqrt(edx**2 + edy**2 + edz**2)

      bigcal_thetarad = acos(edz/epathlength)
      bigcal_phirad = atan2(edy,edz)
      
      bigcal_energy = bigcal_track_energy(ibest_cal)
      
c     correct tof:
      
      gamma_corr = bigcal_energy / Me
      beta_corr = sqrt(1.-1./gamma_corr**2)

      bigcal_beta = beta_corr
      bigcal_tof = epathlength / (beta_corr*speed_of_light)
      
      mom_corr = beta_corr * bigcal_energy

      bigcal_px = mom_corr * sin(bigcal_thetarad) * cos(bigcal_phirad)
      bigcal_py = mom_corr * sin(bigcal_thetarad) * sin(bigcal_phirad)
      bigcal_pz = mom_corr * cos(bigcal_thetarad)

      bigcal_eloss = bigcal_track_eloss(ibest_cal)
      bigcal_time = bigcal_track_time(ibest_cal)
      bigcal_ctime = bigcal_track_time(ibest_cal) - tof

      gep_ctime_hms = hstime_at_fp - hstart_time_center + hspath_cor
      gep_ctime_cal = bigcal_ctime


c     compute Q2 three different ways:
c     Q2_Cal uses only BigCal information except for hms vertex info
c     Q2_hms uses only HMS information, period.
      Q2_cal = 2.*gebeam*bigcal_energy*(1.-cos(bigcal_thetarad))
      Q2_hms = 2.*Mp*nu
c     what is the average Q2? Both measurements are very good, except for bigcal_energy
c     best is probably to use Eprime calculated from hsp, but use BigCal angle measurement
c     corrected for HMS vertex info. Q2 from Ebeam, hsp alone (Q2_hms) may be even better than this. 
      GEP_Q2 = 2.*gebeam*Eprime*(1.-cos(bigcal_thetarad))
c     GEP_Q2 = .5*(Q2_cal + Q2_hms)
      GEP_Q2_H = Q2_hms
      GEP_Q2_B = Q2_cal
      GEP_E_electron = Eprime
      GEP_P_proton = hsp
      GEP_delta_p = hsdelta
      GEP_epsilon = 1./(1.+2.*(1.+GEP_Q2/(4.*Mp**2))*(tan(bigcal_thetarad/2.))**2)
      GEP_etheta_deg = bigcal_thetarad * 180./PI
      GEP_ptheta_deg = hstheta * 180./PI
      GEP_ephi_deg = bigcal_phirad * 180./PI + 90.
      GEP_pphi_deg = hsphi * 180./PI

      GEP_Emiss = gebeam + Mp - hsenergy - bigcal_energy
      GEP_Pmissx = -bigcal_py + hsp*sin(hstheta)*cos(hsphi)
      GEP_Pmissy = bigcal_px + hsp*sin(hstheta)*sin(hsphi)
      GEP_Pmissz = gpbeam - bigcal_pz - hsp*cos(hstheta)
      GEP_Pmiss = sqrt(GEP_Pmissx**2 + GEP_Pmissy**2 + GEP_Pmissz**2)
      GEP_W2 = Mp**2 + Q2_hms - Q2_cal ! 2Mnu - Q2_cal
      GEP_Mmiss = sqrt(abs(GEP_W2 - Mp**2))

      return 
      end

      integer function pick_best_cal_track(T_H,X_H,Y_H,E_H)
      
      include 'gep_data_structures.cmn'
      include 'bigcal_data_structures.cmn'

      integer itrack,ibest
      real diffsum,mindiffsum
      real E_cal,X_cal,Y_cal,T_cal

      if(bigcal_phys_ntrack.gt.0) then
         do itrack = 1,bigcal_phys_ntrack
            E_cal = bigcal_track_energy(itrack)
            X_cal = bigcal_all_clstr_x(itrack)
            Y_cal = bigcal_all_clstr_y(itrack)
            T_cal = bigcal_track_time(itrack)
            diffsum = 0.
            diffsum = diffsum + ( (E_cal - E_H)/GEP_sigma_Ediff )**2
            diffsum = diffsum + ( (X_cal - X_H)/GEP_sigma_Xdiff )**2
            diffsum = diffsum + ( (Y_cal - Y_H)/GEP_sigma_Ydiff )**2
            diffsum = diffsum + ( (T_cal - T_H)/GEP_sigma_Tdiff )**2
            if(itrack.eq.1) then
               mindiffsum = diffsum
               ibest = itrack
            else
               if(diffsum.lt.mindiffsum) then
                  mindiffsum = diffsum
                  ibest = itrack
               endif
            endif
         enddo
         pick_best_cal_track = ibest
      else
         pick_best_cal_track = 0
      endif
      
      end
