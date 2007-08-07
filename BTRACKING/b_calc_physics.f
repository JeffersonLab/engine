      subroutine b_calc_physics(ABORT,err)

      implicit none
      save
      
      character*14 here
      parameter(here='b_calc_physics')

      logical ABORT
      character*(*) err
      
      include 'bigcal_data_structures.cmn'
c      include 'gen_units.par'
      include 'gen_constants.par'
      include 'gen_data_structures.cmn'
      include 'bigcal_hist_id.cmn'

      integer i,j,k,ntrack,itrackmax,nprot,nrcs,nmid
      integer irow,icol,icell
      real E,x,y,z,t,R,L,tof,Rperp
      real xrot,zrot
      real thetadeg,thetarad,phideg,phirad
      real Sinth,Costh
      real m_e
      real mom,beta,c,eloss,gamma,log10betagamma
      real maxetot

      ABORT=.false.
      err=' '

      Sinth = BIGCAL_SINTHETA
      Costh = BIGCAL_COSTHETA
      R = BIGCAL_R_TGT
      m_e = mass_electron
      c = speed_of_light

      ntrack = 0
c$$$      nprot = 0
c$$$      nrcs = 0
c$$$      nmid = 0
c     this routine also fills many standard histograms

      if(BIGCAL_ALL_NCLSTR.gt.0) then
         do i=1,BIGCAL_ALL_NCLSTR
            ntrack = ntrack + 1
c            nprot = nprot + 1
            x = BIGCAL_ALL_CLSTR_X(i)
            y = BIGCAL_ALL_CLSTR_Y(i)
            E = BIGCAL_ALL_CLSTR_ETOT(i)
            call hf1(bid_bcal_xclust,x,1.0)
            call hf1(bid_bcal_yclust,y,1.0)
            call hf1(bid_bcal_xmom,bigcal_all_clstr_xmom(i),1.0)
            call hf1(bid_bcal_ymom,bigcal_all_clstr_ymom(i),1.0)
            call hf1(bid_bcal_eclust,E,1.0)
            call hf1(bid_bcal_ncellclst,float(bigcal_all_clstr_ncell(i)),1.0)
            call hf1(bid_bcal_nxclust,float(bigcal_all_clstr_ncellx(i)),1.0)
            call hf1(bid_bcal_nyclust,float(bigcal_all_clstr_ncelly(i)),1.0)
            call hf2(bid_bcal_nxny,float(bigcal_all_clstr_ncellx(i)),
     $           float(bigcal_all_clstr_ncelly(i)),1.0)
            call hf2(bid_bcal_xy,x,y,1.0)
            t = BIGCAL_ALL_CLSTR_T8MEAN(i)
            call hf1(bid_bcal_tmean,t,1.0)
            call hf1(bid_bcal_trms,bigcal_all_clstr_t8rms(i),1.0)
c     increment energy sum 
            irow = bigcal_all_clstr_iymax(i)
            icol = bigcal_all_clstr_ixmax(i)

            if(irow.le.32) then
               icell = icol + 32*(irow-1)
            else 
               icell = icol + 30*(irow-33) + bigcal_prot_maxhits
            endif

            b_all_run_Esum(icell) = b_all_run_Esum(icell)+E
            b_all_run_Enum(icell) = b_all_run_Enum(icell)+1

c     correct every track for energy loss. BigCal is always electron arm
c     need to set up eloss params for BigCal absorber!
            xrot = x * Costh + R * Sinth
            zrot = -x * Sinth + R * Costh
            
            L = sqrt(xrot**2 + zrot**2 + y**2)
            ! all length units are cm

            thetarad = acos(zrot/L)
            thetadeg = 180./tt * thetarad
            call hf1(bid_bcal_theta,thetadeg,1.0)

            phirad = atan2(y,xrot)
            phideg = 180./tt * phirad
            call hf1(bid_bcal_phi,phideg,1.0)

            gamma = E / m_e
            beta = sqrt(1. - 1./gamma**2)

            log10betagamma = log(beta*gamma) / log(10.)

            if(gtarg_z(gtarg_num).gt.0) then
               call total_eloss(3,.true.,thetarad,log10betagamma,eloss)
            else 
               eloss = 0.
            endif
c     for now, set eloss to zero for monte carlo analysis!
c$$$            if(gen_bigcal_mc.ne.0) then
c$$$               eloss = 0.
c$$$            endif

            E = E + eloss

            mom = sqrt(E**2 - m_e**2) 
            beta = mom/E
            tof = L/(beta*c)

c            Rperp = L*sin(thetarad)

            BIGCAL_TRACK_THETARAD(ntrack) = thetarad
            BIGCAL_TRACK_THETADEG(ntrack) = thetadeg
            BIGCAL_TRACK_PHIRAD(ntrack) = phirad
            BIGCAL_TRACK_PHIDEG(ntrack) = phideg
            BIGCAL_TRACK_ENERGY(ntrack) = E
            BIGCAL_TRACK_TIME(ntrack) = t
            BIGCAL_TRACK_XFACE(ntrack) = xrot
            BIGCAL_TRACK_YFACE(ntrack) = y
            BIGCAL_TRACK_ZFACE(ntrack) = zrot
            BIGCAL_TRACK_PX(ntrack) = mom * sin(thetarad) * cos(phirad)
            BIGCAL_TRACK_PY(ntrack) = mom * sin(thetarad) * sin(phirad)
            BIGCAL_TRACK_PZ(ntrack) = mom * cos(thetarad)
            BIGCAL_TRACK_BETA(ntrack) = beta
            BIGCAL_TRACK_TOF(ntrack) = tof
            BIGCAL_TRACK_COIN_TIME(ntrack) = t - tof ! "vertex" time

c     increment some efficiency sums:
            do irow=bigcal_all_clstr_iylo(i),
     $           bigcal_all_clstr_iyhi(i)
               if(irow.le.32) then
                  do icol=bigcal_all_clstr_ixlo(i,2),
     $                 bigcal_all_clstr_ixhi(i,2)
                     icell=icol+32*(irow-1)
                     if(bigcal_prot_good_det(icell).gt.b_cell_cut_prot)
     $                    then
                        b_all_run_clst_good(icell) = 
     $                       b_all_run_clst_good(icell) + 1
                     else
                        b_all_run_clst_bad(icell) = 
     $                       b_all_run_clst_bad(icell) + 1
                     endif
                  enddo
               else
                  do icol=bigcal_all_clstr_ixlo(i,3),
     $                 bigcal_all_clstr_ixhi(i,3)
                     icell=icol + 30*(irow-33) + bigcal_prot_maxhits
                     if(bigcal_rcs_good_det(icell-bigcal_prot_maxhits)
     $                    .gt.b_cell_cut_rcs) then
                        b_all_run_clst_good(icell) = 
     $                       b_all_run_clst_good(icell) + 1
                     else 
                        b_all_run_clst_bad(icell) = 
     $                       b_all_run_clst_bad(icell) + 1
                     endif
                  enddo
               endif
            enddo
         enddo
         bigcal_phys_ntrack = ntrack
      endif
     
      return 
      end
