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

      real vx,vy,vz
      
      real exhat,eyhat,ezhat,edx,edy,edz,L,tint
      real etheta,ephi,ptheta,pphi
      real pp,nu_pp,eth_pp,Ee_pp,pp_eth,nu_eth,Ee_eth
      real eph_pp

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

      logical firstgood

      integer iclust,best,ndf,irow,icol,icell,ihit,jrow,jcol,jcell,jhit

      c = speed_of_light

      p0 = hpcentral
      E0 = gebeam
      bR = bigcal_r_tgt
      btheta = bigcal_theta_deg
      dxptar0 = h_oopcentral_offset
      dycal = 0.

      if(gep_select_apply_offsets) then
         p0 = hpcentral * (1. + gep_select_dp0/100.)
         E0 = gebeam + gep_select_dE0 / 1000.
         dxptar0 = h_oopcentral_offset + gep_select_dxptar
         btheta = bigcal_theta_deg + gep_select_dbtheta
         bR = bigcal_r_tgt + gep_select_dbdist
         dycal = gep_select_dby
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

      Q2_pth = (2.*Mp*E0*cos(ptheta))**2 / 
     $     (Mp**2 + 2.*Mp*E0 + (E0*sin(ptheta))**2)
      nu_pth = Q2_pth / (2.*Mp)
      pp_pth = sqrt(nu_pth**2 + 2.*Mp*nu_pth)

      pmissh = (pp - pp_pth)/p0

      nu_pp = sqrt(pp**2 + Mp**2) - Mp  ! proton kinetic energy

      Ee_pp = E0 - nu_pp

      eth_pp = acos(max(-1.,min(1.,1. - Mp/E0 * nu_pp / Ee_pp)))
      eph_pp = pphi + PI

      exhat = sin(eth_pp) * sin(eph_pp)
      eyhat = -sin(eth_pp) * cos(eph_pp)
      ezhat = cos(eth_pp)

      vx = gbeam_x
      vy = gbeam_y
      vz = hsy_tar * ( coshthetas / tan(htheta_lab * PI / 180.-hsyp_tar)
     $     + sinhthetas ) - vx / tan(htheta_lab*PI/180. - hsyp_tar)
      
      tint = (bR - vx*sinBth - vz*cosBth) / (exhat * sinBth + ezhat * cosBth)

      hxface = vx + tint * exhat
      hyface = vy + tint * eyhat
      hzface = vz + tint * ezhat

      hxrot = hxface * cosBth - hzface * sinBth

      cointimeh = hstime_at_fp - hstart_time_center + 
     $     hspathlength / hsbeta_p

      firstgood = .true.

      best = 0

      do iclust = 1,bigcal_all_nclstr
         xrot = bigcal_all_clstr_x(iclust) * cosBth + bR * sinBth
         zrot = -bigcal_all_clstr_x(iclust) * sinBth + bR * cosBth
         
         bxface = xrot
         byface = bigcal_all_clstr_y(iclust) + dycal
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

         chi2 = chi2 + (bigcal_all_clstr_x(iclust) - hxrot)**2/gep_sigma_xdiff**2
         ndf = ndf + 1
         chi2 = chi2 + (byface - hyface)**2/gep_sigma_ydiff**2
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

         if((firstgood.or.chi2.lt.minchi2).and.
     $        bigcal_clstr_keep(iclust)) then
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
         bigcal_all_clstr_chi2contr(iclust,4) = (bigcal_all_clstr_x(iclust) - hxrot)**2/gep_sigma_xdiff**2
         bigcal_all_clstr_chi2contr(iclust,5) = (byface - hyface)**2/gep_sigma_ydiff**2
         bigcal_all_clstr_chi2contr(iclust,6) = (cointimeh-cointimeb)**2/gep_sigma_tdiff**2
      enddo
         
c      best = 1

      bigcal_itrack_best = best

      if(abs(xbest-hxrot).le.gep_bcalib_cut_dx.and.abs(ybest-hyface)
     $     .le.gep_bcalib_cut_dy.and.abs(tbest-cointimeh).le.
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
            
            do ihit=1,bigcal_all_clstr_ncell(best)
               irow = bigcal_all_clstr_iycell(best,ihit)
               icol = bigcal_all_clstr_ixcell(best,ihit)
               if(irow.le.bigcal_prot_ny) then
                  icell = icol + bigcal_prot_nx*(irow-1)
               else 
                  icell = icol + bigcal_rcs_nx*(irow-1-bigcal_prot_ny)
     $                 + bigcal_prot_maxhits
               endif
               
               adci = bigcal_all_clstr_acell(best,ihit)
               
               bigcal_vector(icell) = bigcal_vector(icell) + adci / Ecalib
               
               do jhit=1,bigcal_all_clstr_ncell(best)
                  jrow = bigcal_all_clstr_iycell(best,jhit)
                  jcol = bigcal_all_clstr_ixcell(best,jhit)
                  if(jrow.le.bigcal_prot_ny) then
                     jcell = jcol + bigcal_prot_nx*(jrow-1)
                  else 
                     jcell = jcol + bigcal_rcs_nx*(jrow-1-bigcal_prot_ny)
     $                    + bigcal_prot_maxhits
                  endif
                  
                  adcj = bigcal_all_clstr_acell(best,jhit)
                  
                  bigcal_matrix(icell,jcell) = bigcal_matrix(icell,jcell) + 
     $                 adci*adcj / (Ecalib**2)
               enddo
            enddo
         endif
      endif
      
      return
      end
