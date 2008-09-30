      subroutine gep_select_best_cluster

      implicit none
      save
      
      include 'hms_data_structures.cmn'
      include 'hms_scin_tof.cmn'
      include 'hms_physics_sing.cmn'
      include 'bigcal_data_structures.cmn'
      include 'bigcal_tof_parms.cmn'
      include 'bigcal_bypass_switches.cmn'
      include 'gep_data_structures.cmn'
      include 'gen_data_structures.cmn'
      include 'gen_constants.par'
     
      real PI
      parameter(PI=3.141592653)

      real Mp
      parameter(Mp=.938272)

      real me
      parameter(me=.511e-3)

      real c

      real vx,vy,vz,x,y,z
      
      real exhat,eyhat,ezhat,edx,edy,edz,L,tint
      real etheta,ephi,ptheta,pphi
      real pp,nu_pp,eth_pp,Ee_pp,pp_eth,nu_eth,Ee_eth
      real eph_pp
      real realcoord(3),idealcoord(3)

      real pmissb,gamma,eloss,pmissh
      real Q2_pth,nu_pth,pp_pth

      real p0,E0,bR,btheta,cosBth,sinBth,Ee
      real dxptar0,dycal
      real pz,pxlab,pylab,pzlab,xptar_temp
      
      real bxface,byface,bzface,xrot,zrot
      real hxface,hyface,hzface,hxrot

      real chi2,minchi2

      real cointimeh,cointimeb,tof,tofcor,beta

      real xbest,ybest,tbest,bdpbest,thetabest,phibest

      real Ecalib,adci,adcj

      logical firstgood,usei,usej,firsttime,passed_5sig

      integer iclust,best,ndf,irow,icol,icell,ihit,jrow,jcol,jcell,jhit
      integer rowmax,colmax,m,n,ngood

      c = speed_of_light

      p0 = hpcentral
      E0 = gebeam
      bR = bigcal_r_tgt
      btheta = bigcal_theta_deg
      dxptar0 = h_oopcentral_offset
      dycal = bigcal_height

      if(gep_select_apply_offsets) then
         p0 = hpcentral * (1. + gep_select_dp0/100.)
         E0 = gebeam + gep_select_dE0 / 1000.
         dxptar0 = h_oopcentral_offset + gep_select_dxptar
         btheta = bigcal_theta_deg + gep_select_dbtheta
         bR = bigcal_r_tgt + gep_select_dbdist
         dycal = dycal + gep_select_dby
      endif

      xptar_temp = hsxp_tar + dxptar0

      cosBth = cos(btheta * PI / 180.)
      sinBth = sin(btheta * PI / 180.)

      pp = p0 * (1. + hsdelta / 100.)
      
      pz = pp / sqrt(1. + xptar_temp**2 + hsyp_tar**2)

      pxlab = pz * xptar_temp
      pylab = pz * (hsyp_tar * coshthetas - sinhthetas)
      pzlab = pz * (hsyp_tar * sinhthetas + coshthetas)

      ptheta = acos(max(-1.,min(pzlab/pp,1.)))
      pphi = atan2(pylab,pxlab)

c$$$      Q2_pth = (2.*Mp*E0*cos(ptheta))**2 / 
c$$$     $     (Mp**2 + 2.*Mp*E0 + (E0*sin(ptheta))**2)
c$$$      nu_pth = Q2_pth / (2.*Mp)
      pp_pth = 2.*Mp*E0*(Mp+E0)*cos(ptheta) / 
     $     (Mp**2 + 2.*Mp*E0 + (E0*sin(ptheta))**2)

      pmissh = (pp - pp_pth)/p0

      nu_pp = sqrt(pp**2 + Mp**2) - Mp  ! proton kinetic energy

      Ee_pp = E0 - nu_pp

      eth_pp = acos(max(-1.,min(1.,1. - Mp/E0 * nu_pp / Ee_pp)))
      eph_pp = pphi + PI

      exhat = sin(eth_pp) * sin(eph_pp)
      eyhat = -sin(eth_pp) * cos(eph_pp)
      ezhat = cos(eth_pp)

      vx = -gbeam_x ! average beam position
      vy = gbeam_y ! average beam position
      vz = hszbeam ! HMS zbeam calculated from ytar and yptar

c      if(gep_use_frx.ne.0) then ! use raster x in vertex correction to BigCal angles
c         vx = vx + gfrx
c      endif
      
c      if(gep_use_fry.ne.0) then ! use raster y in vertex correction to BigCal angles
c         vy = vy + gfry
c      endif
      
      if(gep_use_xbeam_zcorr.ne.0) then ! correct vertex z for beam x (can be as large as 1-2 cm for small htheta_lab, large xbeam)
         vz = vz - vx / tan(htheta_lab*PI/180. - hsyp_tar)
      endif

c     check for reasonable zbeam:
      if(vz.lt.gep_zbeam_low.or.vz.gt.gep_zbeam_high) then 
         vz = 0.
      endif

c     calculate intersection point with the ideal (un-rotated) plane of BigCal at 
c     an angle of btheta and a distance of bR

      tint = (bR - vx*sinBth - vz*cosBth) / (exhat * sinBth + ezhat * cosBth)

      hxface = vx + tint * exhat
      hyface = vy + tint * eyhat
      hzface = vz + tint * ezhat

      hxrot = hxface * cosBth - hzface * sinBth

      cointimeh = hstime_at_fp - hstart_time_center + 
     $     hspathlength / hsbeta_p

      firstgood = .true.

      best = 0
      
      ngood = 0

c     at this point, b_calc_physics has already been called, but just in case
c     offsets have been applied, still want to start with the 
c     raw cluster coordinates and re-apply the rotation matrix for yaw, pitch and roll

      do iclust = 1,bigcal_all_nclstr

         passed_5sig = .false.

         realcoord(1) = bigcal_all_clstr_x(iclust)
         realcoord(2) = bigcal_all_clstr_y(iclust)
         realcoord(3) = 0.

         do m=1,3
            idealcoord(m) = 0.
            do n=1,3
               idealcoord(m) = idealcoord(m) + 
     $              bigcal_rot_matrix(m,n) * realcoord(n)
            enddo
         enddo

         x = idealcoord(1)
         y = idealcoord(2) + dycal
         z = idealcoord(3) + bR

         xrot = x * cosBth + z * sinBth
         zrot = -x * sinBth + z * cosBth
         
         bxface = xrot
         byface = y
         bzface = zrot
         
         edx = bxface - vx
         edy = byface - vy
         edz = bzface - vz
         
         L = sqrt(edx**2 + edy**2 + edz**2)
         
         etheta = acos(edz / L)
         ephi = atan2(edy,edx) + PI/2.

         Ee_eth = E0 / (1. + E0 / Mp * (1. - cos(etheta) ) )
         nu_eth = E0 - Ee_eth
         pp_eth = sqrt(nu_eth**2 + 2.*Mp*nu_eth)

         pmissb = (pp - pp_eth)/p0

         Ee = bigcal_all_clstr_etot(iclust)
         
         gamma = bigcal_all_clstr_etot(iclust) / me

         beta = sqrt(1. - 1. / gamma**2)

         if(gtarg_z(gtarg_num).gt.0) then
            call total_eloss(3,.true.,etheta,log(beta*gamma) / log(10.),eloss)
         else
            eloss = 0.
         endif

         Ee = Ee + eloss

         beta = sqrt(max(0.,Ee**2-me**2)) / Ee

         if(beta.eq.0.) beta = 1.

         tof = L / (beta * c)

         tofcor = tof - bigcal_tof_central

         cointimeb = bigcal_all_clstr_t8cut(iclust) - tofcor - 
     $        (bigcal_end_time - bigcal_window_center)
         
         ndf = 0
         chi2 = 0.

         chi2 = chi2 + (x - hxrot)**2/gep_sigma_xdiff**2
         ndf = ndf + 1
         chi2 = chi2 + (y - hyface)**2/gep_sigma_ydiff**2
         ndf = ndf + 1
         chi2 = chi2 + (etheta - eth_pp)**2/gep_sigma_thdiff**2
         ndf = ndf + 1
         chi2 = chi2 + (ephi - eph_pp)**2/gep_sigma_phdiff**2
         ndf = ndf + 1
         chi2 = chi2 + pmissb**2/gep_sigma_pmiss**2
         ndf = ndf + 1
         chi2 = chi2 + (cointimeh-cointimeb)**2/gep_sigma_tdiff**2
         ndf = ndf + 1
         chi2 = chi2 + (Ee - Ee_pp)**2/gep_sigma_ediff**2
         ndf = ndf + 1

         chi2 = chi2 / float(ndf)

c     check if any cluster passed 5-sigma cuts on 
c     elastic kinematics and coincidence time
c     if any cluster passes, throw out all that don't

         if(sqrt( (x-hxrot)**2/gep_sigma_xdiff**2 + 
     $        (y-hyface)**2/gep_sigma_ydiff**2 ).le.5.0 .and.
     $        abs(etheta - eth_pp).le.5.*gep_sigma_thdiff.and.
     $        abs(ephi - eph_pp).le.5.*gep_sigma_phdiff.and.
     $        abs(pmissb).le.5.*gep_sigma_pmiss.and.
     $        abs(cointimeh-cointimeb).le.5.*gep_sigma_tdiff.and.
     $        abs(Ee - Ee_pp).le.5.*gep_sigma_ediff.and.
     $        bigcal_clstr_keep(iclust)) then
            ngood = ngood + 1
            passed_5sig = .true.
         endif

         if((firstgood.or.chi2.lt.minchi2).and.
     $        bigcal_clstr_keep(iclust).and.
     $        (passed_5sig.or.ngood.eq.0)) then
            firstgood=.false.
            best = iclust
            minchi2 = chi2
            
            xbest = bigcal_all_clstr_x(iclust)
            ybest = byface
            tbest = cointimeb
            bdpbest = pmissb
            thetabest = etheta
            phibest = ephi
         endif
         
         bigcal_all_clstr_chi2(iclust) = chi2
         bigcal_all_clstr_chi2contr(iclust,1) = (Ee - Ee_pp)**2/gep_sigma_ediff**2
         bigcal_all_clstr_chi2contr(iclust,2) = (etheta - eth_pp)**2/gep_sigma_thdiff**2
         bigcal_all_clstr_chi2contr(iclust,3) = (ephi - eph_pp)**2/gep_sigma_phdiff**2
         bigcal_all_clstr_chi2contr(iclust,4) = (x - hxrot)**2/gep_sigma_xdiff**2
         bigcal_all_clstr_chi2contr(iclust,5) = (y - hyface)**2/gep_sigma_ydiff**2
         bigcal_all_clstr_chi2contr(iclust,6) = (cointimeh-cointimeb)**2/gep_sigma_tdiff**2
      enddo
         
c      best = 1

      bigcal_itrack_best = best

      if(best.gt.0.and.sqrt( (xbest-hxrot)**2/gep_bcalib_cut_dx**2 + 
     $     (ybest-hyface)**2/gep_bcalib_cut_dy**2).le.1.
     $     .and.abs(tbest-cointimeh).le.
     $     gep_bcalib_cut_ctime.and.abs(pmissh).le.
     $     gep_bcalib_cut_elastic(1).and.abs(bdpbest).le.
     $     gep_bcalib_cut_elastic(2).and.abs(thetabest-eth_pp).le.
     $     gep_bcalib_cut_theta.and.abs(phibest-eph_pp).le.
     $     gep_bcalib_cut_phi.and.Ee_pp.ge.gep_bcalib_cut_ehms(1)
     $     .and.Ee_pp.le.gep_bcalib_cut_ehms(2)) then 
*     which quantity has better resolution? Ee(etheta) or Ee(pp)?
*     momentum resolution is 1e-3
*     electron angle resolution is ~1 cm / 435 cm = 2 mrad
*     depending on the Jacobian of the reaction, 
*     electron angle can give better resolution than proton momentum
         gep_good_calib_event = .true.
         if(bigcal_do_calibration.ne.0) then ! actually fill the matrix
            if(gep_bcalib_use_etheta) then
               Ecalib = E0 / (1. + E0/Mp * (1. - cos(thetabest)) ) * 1000. ! convert to MeV
            else
               Ecalib = Ee_pp * 1000. ! convert to MeV
            endif
            
            bigcal_nmatr_event = bigcal_nmatr_event + 1
            
            rowmax = bigcal_all_clstr_iycell(best,1)
            colmax = bigcal_all_clstr_ixcell(best,1)

            do ihit=1,bigcal_all_clstr_ncell(best)
               irow = bigcal_all_clstr_iycell(best,ihit)
               icol = bigcal_all_clstr_ixcell(best,ihit)
               if(irow.le.bigcal_prot_ny) then
                  icell = icol + bigcal_prot_nx*(irow-1)
               else 
                  icell = icol + bigcal_rcs_nx*(irow-1-bigcal_prot_ny)
     $                 + bigcal_prot_maxhits
               endif
               
               usei=.false.

               if(abs(irow-rowmax).le.bigcal_clstr_nyecl_max) then
                  if(rowmax.le.32.and.irow.gt.32) then
                     if(abs(icol-bigcal_ixclose_prot(colmax)).le.
     $                    bigcal_clstr_nxecl_max) then
                        usei = .true.
                     endif
                  else if(rowmax.gt.32.and.irow.le.32) then
                     if(abs(icol-bigcal_ixclose_rcs(colmax)).le.
     $                    bigcal_clstr_nxecl_max) then
                        usei = .true.
                     endif
                  else
                     if(abs(icol-colmax).le.bigcal_clstr_nxecl_max) then
                        usei = .true.
                     endif
                  endif
               endif

               if(usei) then

                  adci = bigcal_all_clstr_acell(best,ihit)
               
                  bigcal_vector(icell) = bigcal_vector(icell) + adci / Ecalib
               
                  do jhit=1,bigcal_all_clstr_ncell(best)
                     jrow = bigcal_all_clstr_iycell(best,jhit)
                     jcol = bigcal_all_clstr_ixcell(best,jhit)
                     if(jrow.le.bigcal_prot_ny) then
                        jcell = jcol + bigcal_prot_nx*(jrow-1)
                     else 
                        jcell = jcol + bigcal_rcs_nx*(jrow-1-bigcal_prot_ny)
     $                       + bigcal_prot_maxhits
                     endif
                     
                     usej=.false.

                     if(abs(jrow-rowmax).le.bigcal_clstr_nyecl_max) then
                        if(rowmax.le.32.and.jrow.gt.32) then
                           if(abs(jcol-bigcal_ixclose_prot(colmax)).le.
     $                          bigcal_clstr_nxecl_max) then
                              usej = .true.
                           endif
                        else if(rowmax.gt.32.and.jrow.le.32) then
                           if(abs(jcol-bigcal_ixclose_rcs(colmax)).le.
     $                          bigcal_clstr_nxecl_max) then
                              usej = .true.
                           endif
                        else
                           if(abs(jcol-colmax).le.bigcal_clstr_nxecl_max) then
                              usej = .true.
                           endif
                        endif
                     endif

                     if(usej) then

                        adcj = bigcal_all_clstr_acell(best,jhit)
                        
                        bigcal_matrix(icell,jcell) = bigcal_matrix(icell,jcell) + 
     $                       adci*adcj / (Ecalib**2)
                     endif
                  enddo
               endif
            enddo
         endif
      endif
      
      return
      end
      
