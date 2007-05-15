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
      real*4 Mp
      parameter(Mp=.938272) ! mass of proton in GeV
      real*4 PI
      parameter(PI=3.141592654)

      real*4 offset_ctime
      real*4 vx,vy,vz
      real*4 tface_cal 
      real*4 exhat,eyhat,ezhat,exhatrot,ezhatrot
      real*4 thetax_inc_H,thetay_inc_H ! x and y incident angle based on HMS vertex and angle info
      real*4 thetax_inc_O,thetay_inc_O ! x and y incident angle from origin
      real*4 nu 
      real*4 costheta_e
      real*4 theta_e_HMS
      real*4 phi_e_HMS
      real*4 xface_HMS,yface_HMS,zface_HMS
      real*4 xface_CAL,yface_CAL,zface_CAL
      real*4 minthetaxdiff
      real*4 minthetaydiff
      real*4 xcell,ycell,xmom,ymom,xcellclst,ycellclst
      real*4 newxpar(6),newypar(6)
      real*4 xface,yface,zface,xrot,zrot,rayperp,px,py,pz
      real*4 Q2_cal,Q2_HMS
      real*4 deltaQ2_cal,deltaQ2_HMS,delta_cosetheta,tau

      integer*4 irowclst,icolclst,irow,icol,icell,ipar,icellclst
      integer*4 irowmindiff,icolmindiff,isectmindiff

      abort=.false.
      err=' '
      
      if(HSNUM_FPTRACK.le.0.or.BIGCAL_PHYS_NTRACK.le.0)then
         return
      endif
      
c     HMS is always hadron arm for "GEp" type events: 
      
c     TARGET coordinates: 
c     z points downstream along beamline
c     x points downward
c     y points beam left (away from HMS, toward BigCal) in contrast to my BigCal coordinates in which
c     x points beam left, and y is upward

c     hstheta is lab scattering angle in radians
c     hsphi is lab azimuthal angle in radians: centered at -PI/2 for HMS in TARGET coordinates
c     hszbeam is lab z coordinate of intersection of beam track with spectrometer ray
c     as far as I can tell, even though gbeam_x, gbeam_y, gbeam_xp and gbeam_yp are calculated from the 
c     BPM/raster info, hsx_tar is just set to zero. For now, use xtar = 0 and ytar = 0 and just use hszbeam
c     to correct BigCal angle info.

c     Apparently, only the y info of the BPM/raster is used to correct the HMS track info and calculate the z 
c     of the vertex. However, we can still use the BPM/raster info to correct the BigCal angle info, along with
c     hszbeam

c     first correct BigCal info based on hms vertex and angle info

c$$$      vx = gbeam_y ! use BPM and raster info 
c$$$      vy = -gbeam_x ! use BPM and raster info

      vx = 0. ! nominally zero
      vy = 0. ! nominally zero
      vz = hszbeam ! vertex z is only vertex coord. that has a significant impact on angular resolution

c     calculate expected electron angle from measured proton momentum assuming elastic kinematics
c     and using calculated beam energy: 

      nu = sqrt(Mp**2 + hsp**2) - Mp
      
      costheta_e = 1. - Mp / gebeam * nu / (gebeam - nu)

      if(costheta_e.gt.1.) costheta_e = 1.

c     BigCal coordinates:
c     x = beam left toward bigcal = y in target coords.
c     y = vertically up = -x in target coords
c     z = downstream along beam = z in target coords.

      theta_e_HMS = acos(costheta_e) 
      phi_e_HMS = hsphi + PI/2. ! in my BigCal coordinates phi_e is centered at phi=0 
      
c     neglecting beam slope for now, set electron trajectory variables:

      exhat = sin(theta_e_HMS)*cos(phi_e_HMS)
      eyhat = sin(theta_e_HMS)*sin(phi_e_HMS)
      ezhat = cos(theta_e_HMS)

c     now find point of intersection of electron's assumed trajectory with BigCal face:
c     x_e = (0,0,vz) + t*(exhat,eyhat,ezhat)
c     tface_cal = value of parameter of trajectory equation at intercept with the plane of BigCal face
c     neglecting transverse beam position info: 
      tface_cal = (bigcal_r_tgt-vz*bigcal_costheta-vx*bigcal_sintheta)/ 
     $     (exhat*bigcal_sintheta + ezhat*bigcal_costheta)
c     or including transverse beam position info:
c$$$      tface_cal = (R - vx*bigcal_sintheta - vz*bigcal_costheta) / 
c$$$     $     (exhat*bigcal_sintheta + ezhat*bigcal_costheta)
c     expected coordinates at BigCal using HMS info:
      xface_HMS = vx + tface_cal*exhat
      yface_HMS = vy + tface_cal*eyhat
      zface_HMS = vz + tface_cal*ezhat
c     calculate incident x and y angles to choose different shower parms if necessary:
c     simple rotation from 
      
      exhatrot = exhat*bigcal_costheta - ezhat*bigcal_sintheta
      ezhatrot = exhat*bigcal_sintheta + ezhat*bigcal_costheta

      thetax_inc_H = atan2(exhatrot,ezhatrot)
      thetay_inc_H = atan2(eyhat,ezhatrot)

c     get best cluster from bigcal:

      if(bigcal_best_clstr_isection.eq.1) then ! protvino
         irowclst = bigcal_prot_clstr_iymax(bigcal_best_clstr_icluster)
         icolclst = bigcal_prot_clstr_ixmax(bigcal_best_clstr_icluster)
         xmom = bigcal_prot_clstr_xmom(bigcal_best_clstr_icluster)
         ymom = bigcal_prot_clstr_ymom(bigcal_best_clstr_icluster)

         icellclst = icolclst + (irowclst-1)*BIGCAL_PROT_NX

      else if(bigcal_best_clstr_isection.eq.2) then ! rcs
         irowclst = bigcal_rcs_clstr_iymax(bigcal_best_clstr_icluster)
         icolclst = bigcal_rcs_clstr_ixmax(bigcal_best_clstr_icluster)
         xmom = bigcal_rcs_clstr_xmom(bigcal_best_clstr_icluster)
         ymom = bigcal_rcs_clstr_ymom(bigcal_best_clstr_icluster)

         icellclst = icolclst+(irowclst-1-BIGCAL_PROT_NY)*BIGCAL_RCS_NX

      else if(bigcal_best_clstr_isection.eq.3) then ! mid
         irowclst = bigcal_mid_clstr_iymax(bigcal_best_clstr_icluster)
         icolclst = bigcal_mid_clstr_ixmax(bigcal_best_clstr_icluster)
         xmom = bigcal_mid_clstr_xmom(bigcal_best_clstr_icluster)
         ymom = bigcal_mid_clstr_ymom(bigcal_best_clstr_icluster)
         
         if(irowclst.le.BIGCAL_PROT_NY) then
            icellclst = icolclst + (irowclst-1)*BIGCAL_PROT_NX
         else
            icellclst=icolclst+(irowclst-1-BIGCAL_PROT_NY)*BIGCAL_RCS_NX
         endif
      else                      ! problem or no good clusters
         return
      endif
      
      minthetaxdiff = 1000.
      minthetaydiff = 1000.

      irowmindiff = 0
      icolmindiff = 0

      do irow=irowclst-5,irowclst+5
         if(irow.ge.2) then
            if(irow.le.BIGCAL_PROT_NY) then
               icol = icolclst
               icell = icolclst + (irow-1)*BIGCAL_PROT_NX
               ycell = bigcal_prot_ycenter(icell)
               
               thetay_inc_O = atan2(ycell,bigcal_r_tgt)

               if(abs(thetay_inc_O - thetay_inc_H).lt.minthetaydiff)then 
                  minthetaydiff = abs(thetay_inc_O - thetay_inc_H)
                  irowmindiff = irow
               endif
            else if(irow-BIGCAL_PROT_NY.lt.BIGCAL_RCS_NY)then
               icol = icolclst
               icell = icolclst + BIGCAL_RCS_NX*(irow-1-BIGCAL_PROT_NY)
               ycell = bigcal_rcs_ycenter(icell)
               
               thetay_inc_O = atan2(ycell,bigcal_r_tgt)

               if(abs(thetay_inc_O - thetay_inc_H).lt.minthetaydiff)then 
                  minthetaydiff = abs(thetay_inc_O - thetay_inc_H)
                  irowmindiff = irow
               endif
            endif
         endif
      enddo 
            
      if(irowmindiff.ne.0.and.irowmindiff.ne.irowclst) then ! pick a different set of fit parameters 
         if(irowmindiff.le.BIGCAL_PROT_NY.and.irowmindiff.ge.2) then
            do ipar=1,6
               newypar(ipar) = bigcal_prot_ypar(irowmindiff,ipar)
            enddo

            ycellclst = bigcal_prot_ycenter(icellclst)
            
c     recalculate y coordinate of best track using new fit parameters:
            yface_cal = newypar(1)*atan(newypar(2)*ymom**4 + 
     $           newypar(3)*ymom**3 + newypar(4)*ymom**2 + 
     $           newypar(5)*ymom + newypar(6)) + ycellclst
            
            bigcal_best_yface = yface_cal
         else if(irowmindiff.lt.BIGCAL_PROT_NY+BIGCAL_RCS_NY) then
            do ipar=1,6
               newypar(ipar) = bigcal_rcs_ypar(irowmindiff,ipar)
            enddo
            
            ycellclst = bigcal_rcs_ycenter(icellclst)

            yface_cal = newypar(1)*atan(newypar(2)*ymom**4 + 
     $           newypar(3)*ymom**3 + newypar(4)*ymom**2 + 
     $           newypar(5)*ymom + newypar(6)) + ycellclst

            bigcal_best_yface = yface_cal
         endif
      endif
c     this logical structure insures that we will recalculate the cluster coordinates if and only if
c     the incident angle for the electron track predicted by the HMS info suggests a better choice of 
c     fit parameters than that assuming all tracks come from the origin. It may even be better simply to use 
c     the HMS vertex info and the initial BigCal coordinates to choose new parameters if necessary, without 
c     referring to the HMS momentum measurement. However, the HMS momentum measurement of ~.1% together with 
c     a beam energy spread of ~3e-5 added in quadrature and assuming elastic kinematics, do in fact give a 
c     determination of the scattered electron angle which is comparable to that obtained by BigCal. 

      do icol=icolclst-5,icolclst+5
         if(irowclst.le.BIGCAL_PROT_NY.and.icol.ge.2.and.icol.le.
     $        BIGCAL_PROT_NX-1) then
            icell = icol + (irowclst-1)*BIGCAL_PROT_NX
            xcell = bigcal_prot_xcenter(icell)

            thetax_inc_O = atan2(xcell,bigcal_r_tgt)

            if(abs(thetax_inc_H - thetax_inc_O).lt.minthetaxdiff)then
               minthetaxdiff = abs(thetax_inc_H - thetax_inc_O)
               icolmindiff = icol
            endif
         else if(irowclst.gt.BIGCAL_PROT_NY.and.icol.ge.2.and.icol.le.
     $           BIGCAL_RCS_NX-1) then
            icell = icol + (irowclst-1-BIGCAL_PROT_NY)*BIGCAL_RCS_NX
            xcell = bigcal_rcs_xcenter(icell)

            thetax_inc_O = atan2(xcell,bigcal_r_tgt)

            if(abs(thetax_inc_H - thetax_inc_O).lt.minthetaxdiff)then
               minthetaxdiff = abs(thetax_inc_H - thetax_inc_O)
               icolmindiff = icol
            endif
         endif
      enddo
            
      if(icolmindiff.ne.0.and.icolmindiff.ne.icolclst) then ! pick a different set of fit parameters
         if(irowclst.le.BIGCAL_PROT_NY.and.icolmindiff.ge.2.and.
     $        icolmindiff.le.BIGCAL_PROT_NX-1) then
            do ipar=1,6
               newxpar(ipar) = bigcal_prot_xpar(icolmindiff,ipar)
            enddo

            xcellclst = bigcal_prot_xcenter(icellclst)

            xface_cal = newxpar(1)*atan(newxpar(2)*xmom**4 + newxpar(3)*
     $           xmom**3 + newxpar(4)*xmom**2 + newxpar(5)*xmom + 
     $           newxpar(6)) + xcellclst

            xrot = xface_cal*bigcal_costheta + 
     $           bigcal_r_tgt*bigcal_sintheta
            zrot = -xface_cal*bigcal_sintheta + 
     $           bigcal_r_tgt*bigcal_costheta

            bigcal_best_xface = xrot
            bigcal_best_zface = zrot

         else if(icolmindiff.ge.2.and.icolmindiff.le.BIGCAL_RCS_NX-1
     $           .and. irowclst.gt.BIGCAL_PROT_NY) then
            do ipar=1,6
               newxpar(ipar) = bigcal_rcs_xpar(icolmindiff,ipar)
            enddo

            xcellclst = bigcal_rcs_xcenter(icellclst)

            xface_cal = newxpar(1)*atan(newxpar(2)*xmom**4 + newxpar(3)*
     $           xmom**3 + newxpar(4)*xmom**2 + newxpar(5)*xmom + 
     $           newxpar(6)) + xcellclst

            xrot = xface_cal*bigcal_costheta + 
     $           bigcal_r_tgt*bigcal_sintheta
            zrot = -xface_cal*bigcal_sintheta + 
     $           bigcal_r_tgt*bigcal_costheta

            bigcal_best_xface = xrot
            bigcal_best_zface = zrot
         endif
      endif

c     now we have made any necessary corrections/improvements to the reconstructed BigCal coordinates:
c     recalculate the BigCal measured angles which depend on these coordinates (and also the HMS vertex coordinates)

      xface = bigcal_best_xface
      yface = bigcal_best_yface
      zface = bigcal_best_zface

      rayperp = sqrt( (xface-vx)**2 + (yface-vy)**2 )
      
      bigcal_best_thetarad = atan2(rayperp,zface-vz)
      bigcal_best_phirad = atan2(yface-vy,xface-vx)

      bigcal_best_thetadeg = bigcal_best_thetarad*180./PI
      bigcal_best_phideg = bigcal_best_phirad*180./PI

      bigcal_best_px = bigcal_best_energy*sin(bigcal_best_thetarad)*
     $     cos(bigcal_best_phirad)
      bigcal_best_py = bigcal_best_energy*sin(bigcal_best_thetarad)*
     $     sin(bigcal_best_phirad)
      bigcal_best_pz = bigcal_best_energy*cos(bigcal_best_thetarad)

c     at this point we should probably change all these quantities to the standard target coordinates:
c     x = vertical down = -(my y)
c     y = beam left = my x
c     z = downstream along beam = my z
c     phi = my phi + pi/2 (phi_HMS = -pi/2)

      bigcal_best_phirad = bigcal_best_phirad + PI/2.
      bigcal_best_phideg = bigcal_best_phideg + 90.

      bigcal_best_xface = -yface
      bigcal_best_yface = xface
      bigcal_best_zface = zface

      px = bigcal_best_px
      py = bigcal_best_py
      pz = bigcal_best_pz

      bigcal_best_px = -py
      bigcal_best_py = px
      bigcal_best_pz = pz

c     now we are ready to compute coincidence physics quantities like missing E,m and p
c     start by working at the most basic level:
c     some of my "coincidence" variables are simply copies of HMS or BigCal reconstructed variables:

      gep_E_electron = bigcal_best_energy
      gep_P_proton = hsp
      gep_delta_p = hsdelta
      
      gep_etheta_deg = bigcal_best_thetadeg
      gep_ephi_deg = bigcal_best_phideg
      
      gep_ptheta_deg = hstheta*180./PI
      gep_pphi_deg = hsphi*180./PI

      Q2_HMS = 2.*Mp*nu ! this Q2 determination is based solely on beam energy and HMS momentum measurement
      
      Q2_cal = 4.*gebeam*bigcal_best_energy*
     $     (sin(bigcal_best_thetarad/2.))**2
c     Q2_cal determination based solely on beam energy and BigCal angle measurement, which may include 
c     "fine" corrections based on the HMS momentum and vertex information, but only to the extent that a 
c     different set of fit parameters may be chosen to reconstruct the position. 

      GEP_Q2 = Q2_HMS

c     since we are talking about elastic ep, W^2 is not a useful variable. 

      gep_emiss = gebeam - (bigcal_best_energy + nu) ! nu is calculated from HMS mom. measurement only
      
c     compute hms momentum:
      px = hsp*sin(hstheta)*cos(hsphi)
      py = hsp*sin(hstheta)*sin(hsphi)
      pz = hsp*cos(hstheta)
c     
      gep_pmissx = bigcal_best_px + px 
      gep_pmissy = bigcal_best_py + py
      gep_pmissz = bigcal_best_pz + pz - gpbeam

      gep_pmiss = sqrt(gep_pmissx**2 + gep_pmissy**2 + gep_pmissz**2)

      gep_W2 = Mp**2 + Q2_HMS - Q2_cal

      gep_Mmiss = sqrt(abs(gep_W2 - Mp**2) )

      tau = GEP_Q2 / (4.* Mp**2)

      gep_epsilon=1./(1.+2.*(1.+tau)*(tan(bigcal_best_thetarad/2.))**2)

c     don't compute coincidence timing quantities at this time. need to set up parameters 
c     to handle this that will be determined by experiment.

      
      return 
      end

      
