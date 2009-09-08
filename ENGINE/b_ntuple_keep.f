      subroutine b_ntuple_keep(ABORT,err,hflag)

      implicit none
      save

      character*13 here
      parameter(here='b_ntuple_keep')

      logical abort
      character*(*) err

      include 'b_ntuple.cmn'
      include 'bigcal_data_structures.cmn'
      include 'bigcal_tof_parms.cmn'
      include 'hms_data_structures.cmn'
      include 'bigcal_gain_parms.cmn'
      include 'gen_event_info.cmn'
      include 'gen_data_structures.cmn'
      include 'gep_data_structures.cmn'
c      include 'gen_scalers.cmn'
      real PI
      parameter(PI=3.14159265359)

      logical HEXIST ! CERNLIB function

c      logical middlebest

      integer m,np,nr,nm,irow,icol,ihit,jhit,itdc,itrig,ngood
c      real ep,er,em
c      integer iybest,ixbest,xclst,yclst,Eclst,xmom,ymom,t8avg,t64avg
      integer L8sum,L64sum,iydiff,ixdiff,icell,jcell,iarray
c      real ecell(BIGCAL_CLSTR_NCELL_MAX)

c      integer itype
      integer jclust,iclust,imax,idiff
      integer nhit_tdc(224)
      integer nhit_ttdc(42)
      integer nhit_tadc(38)
      logical hflag
      real Mp
      parameter(Mp=.938272)

      do ihit=1,224
        nhit_tdc(ihit) = 0
        if(ihit.le.42) nhit_ttdc(ihit) = 0
        if(ihit.le.38) nhit_tadc(ihit) = 0
      enddo

      err=' '
      ABORT=.false.

      if (hflag) then
      if(.not.b_ntuple_exists) return

      if(b_ntuple_max_segmentevents.gt.0) then
         if(b_ntuple_segmentevents.gt.b_ntuple_max_segmentevents) then
            call b_ntuple_change(ABORT,err)
            b_ntuple_segmentevents=0
         else 
            b_ntuple_segmentevents = b_ntuple_segmentevents + 1
         endif
      endif
      endif
      if ( .not. hflag) bigcal_ntuple_type = 1
      if(bigcal_ntuple_type.eq.1) then
         bgid = gen_event_ID_number
         bgtype = gen_event_type
         btrigtype = 0

         do itrig=3,7
            btrigtype = btrigtype + itrig*gen_event_trigtype(itrig)
         enddo

c$$$         write(*,*) 'filling BigCal ntuple'
c$$$         write(*,*) 'event,type,trigtype=',bgid,bgtype,btrigtype

         nclust = bigcal_all_nclust_good
         nclust8 = nclust
         nclust64 = nclust
         ntrack = nclust
         ibest = bigcal_itrack_best

         if(ntrigB.gt.0) then
            btrigtime = bigcal_end_time - gep_btime(1)
         else
            btrigtime = 0.
         endif

c$$$         write(*,*) 'nclust,ncl8,ncl64,ntrk,best=',nclust,nclust8,
c$$$     $        nclust64,ntrack,ibest

         ngood = 0

         do iclust = 1,bigcal_all_nclstr
            if(bigcal_clstr_keep(iclust)) then
               ngood = ngood + 1
               
               if(iclust.eq.ibest) ibest = ngood
               
               ncellclust(ngood) = bigcal_all_clstr_ncell(iclust)
               ncellbad(ngood) = bigcal_all_clstr_nbadlist(iclust)
               ncellx(ngood) = bigcal_all_clstr_ncellx(iclust)
               ncelly(ngood) = bigcal_all_clstr_ncelly(iclust)
               ncell8clust(ngood) = bigcal_all_clstr_ncell8(iclust)
               ncell64clust(ngood) = bigcal_all_clstr_ncell64(iclust)

c$$$            write(*,*) 'iclust,ncell,nbad,nx,ny,n8,n64=',iclust,ncellclust(iclust),
c$$$     $           ncellbad(iclust),ncellx(iclust),ncelly(iclust),ncell8clust(iclust),
c$$$     $           ncell64clust(iclust)

               do icell=1,ncellclust(ngood)
                  iycell(icell,ngood) = bigcal_all_clstr_iycell(iclust,icell)
                  ixcell(icell,ngood) = bigcal_all_clstr_ixcell(iclust,icell)
                  xcell(icell,ngood) = bigcal_all_clstr_xcell(iclust,icell)
                  ycell(icell,ngood) = bigcal_all_clstr_ycell(iclust,icell)
                  eblock(icell,ngood) = bigcal_all_clstr_ecell(iclust,icell)
                  ablock(icell,ngood) = bigcal_all_clstr_acell(iclust,icell)
                  cellbad(icell,ngood) = bigcal_clstr_bad_chan(iclust,icell)
c$$$  write(*,*) 'cell,row,col,x,y,e,bad?=',icell,iycell(icell,iclust),
c$$$  $              ixcell(icell,iclust),xcell(icell,iclust),ycell(icell,iclust),
c$$$  $              eblock(icell,iclust),cellbad(icell,iclust)
               enddo
c     zero all cells above ncellclust
               do icell=ncellclust(ngood)+1,bigcal_clstr_ncell_max
                  iycell(icell,ngood) = 0
                  ixcell(icell,ngood) = 0
                  xcell(icell,ngood) = 0.
                  ycell(icell,ngood) = 0.
                  eblock(icell,ngood) = 0.
                  ablock(icell,ngood) = 0.
                  cellbad(icell,ngood) = .false.
               enddo
               
               do icell=1,ncell8clust(ngood)
                  irow8hit(icell,ngood) = bigcal_all_clstr_irow8(iclust,icell)
                  icol8hit(icell,ngood) = bigcal_all_clstr_icol8(iclust,icell)
                  nhit8clust(icell,ngood) = bigcal_all_clstr_nhit8(iclust,icell)
                  
                  s8(icell,ngood) = bigcal_all_clstr_s8(iclust,icell)
                  
c$$$  write(*,*) 'cell8,row8,col8,nh=',icell,irow8hit(icell,iclust),
c$$$  $              icol8hit(icell,iclust),nhit8clust(icell,iclust)
                  
                  do ihit=1,nhit8clust(icell,ngood)
c$$$  write(*,*) 'hit,time=',ihit,tcell8(icell,ihit,iclust)
                     tcell8(icell,ihit,ngood) = bigcal_all_clstr_tcell8(iclust,icell,ihit)
                  enddo
c     zero all hits above nhit8clust(icell,iclust)
                  do ihit=nhit8clust(icell,ngood)+1,8
                     tcell8(icell,ihit,ngood) = 0.
                  enddo
               enddo
c     zero all cells and all hits of all cells above ncell8clust
               do icell=ncell8clust(ngood)+1,10
                  irow8hit(icell,ngood) = 0
                  icol8hit(icell,ngood) = 0
                  nhit8clust(icell,ngood) = 0
                  s8(icell,ngood) = 0.
                  do ihit=1,8
                     tcell8(icell,ihit,ngood) = 0.
                  enddo
               enddo
               
               do icell=1,ncell64clust(ngood)
                  irow64hit(icell,ngood) = bigcal_all_clstr_irow64(iclust,icell)
                  icol64hit(icell,ngood) = bigcal_all_clstr_icol64(iclust,icell)
                  nhit64clust(icell,ngood) = bigcal_all_clstr_nhit64(iclust,icell)
                  a64(icell,ngood) = bigcal_all_clstr_a64(iclust,icell)
                  s64(icell,ngood) = bigcal_all_clstr_sum64(iclust,icell)
c$$$  write(*,*) 'cell64,row64,col64,nh,a64,s64=',icell,irow64hit(icell,iclust),
c$$$  $              icol64hit(icell,iclust),nhit64clust(icell,iclust),a64(icell,iclust),
c$$$  $              s64(icell,iclust)
                  do ihit=1,nhit64clust(icell,ngood)
                                !write(*,*) 'hit,time=',ihit,tcell64(icell,ihit,iclust)
                     tcell64(icell,ihit,ngood) = bigcal_all_clstr_tcell64(iclust,icell,ihit)
                  enddo
c     zero all hits above nhit64clust(icell,iclust)
                  do ihit=nhit64clust(icell,ngood)+1,8
                     tcell64(icell,ihit,ngood) = 0.
                  enddo
               enddo
c     zero all cells and all hits of all cells above ncell64clust
               do icell=ncell64clust(ngood)+1,6
                  irow64hit(icell,ngood) = 0
                  icol64hit(icell,ngood) = 0
                  nhit64clust(icell,ngood) = 0
                  do ihit=1,8
                     tcell64(icell,ihit,ngood) = 0.
                  enddo
               enddo
               
               xmoment(ngood) = bigcal_all_clstr_xmom(iclust)
               ymoment(ngood) = bigcal_all_clstr_ymom(iclust)
               tclust8(ngood) = bigcal_all_clstr_t8mean(iclust)
               tclust64(ngood) = bigcal_all_clstr_t64mean(iclust)
               tcut8(ngood) = bigcal_all_clstr_t8cut(iclust)
c               tofcor8(ngood) = bigcal_all_clstr_t8cut_cor(iclust)
               tcut64(ngood) = bigcal_all_clstr_t64cut(iclust)
c               tofcor64(ngood) = bigcal_all_clstr_t64cut_cor(iclust)
               trms8(ngood) = bigcal_all_clstr_t8rms(iclust)
               trms64(ngood) = bigcal_all_clstr_t64rms(iclust)
               
c            write(*,*) 'tcut8 = ',tcut8(iclust)
c            write(*,*) 'tcut64=',tcut64(iclust)

c$$$            write(*,*) 'xmom,ymom,t8,t64,trms8,trms64=',xmoment(iclust),
c$$$     $           ymoment(iclust),tclust8(iclust),tclust64(iclust),trms8(iclust),
c$$$     $           trms64(iclust)

               xclust(ngood) = bigcal_all_clstr_x(iclust)
               yclust(ngood) = bigcal_all_clstr_y(iclust)
               eclust(ngood) = bigcal_all_clstr_etot(iclust)
               aclust(ngood) = bigcal_all_clstr_atot(iclust)
               
c               keepclst(ngood) = bigcal_clstr_keep(iclust)

cwrite(*,*) 'xclust,yclust,eclust=',xclust(iclust),yclust(iclust),
c     $           eclust(iclust)
               
               thetarad(ngood) = bigcal_track_thetarad(iclust)
               phirad(ngood) = bigcal_track_phirad(iclust)
               energy(ngood) = bigcal_track_energy(iclust)
               xface(ngood) = bigcal_track_xface(iclust)
               yface(ngood) = bigcal_track_yface(iclust)
               zface(ngood) = bigcal_track_zface(iclust)
               px(ngood) = bigcal_track_px(iclust)
               py(ngood) = bigcal_track_py(iclust)
               pz(ngood) = bigcal_track_pz(iclust)
               ctime_clust(ngood) = bigcal_track_coin_time(iclust) - 
     $              (bigcal_end_time - bigcal_window_center)

               if(bgtype.eq.6.and.ibest>0) then
c     write(*,*) 'chi2=',bigcal_all_clstr_chi2(iclust)
                  chi2clust(ngood) = bigcal_all_clstr_chi2(iclust)
                  do idiff=1,6
c                  write(*,*) 'chi2_',idiff,'=',bigcal_all_clstr_chi2contr(iclust,idiff)
                     chi2contr(idiff,ngood) = bigcal_all_clstr_chi2contr(iclust,idiff)
                  enddo
               else
                  chi2clust(ngood) = -9999.
                  do idiff=1,6
                     chi2contr(idiff,ngood) = -9999.
                  enddo
               endif
c$$$  write(*,*) 'theta,phi,E,xf,yf,zf,px,py,pz,t=',thetarad(iclust),phirad(iclust),
c$$$  $           energy(iclust),xface(iclust),yface(iclust),zface(iclust),px(iclust),py(iclust),
c$$$  $           pz(iclust),ctime_clust(iclust)
            endif
         enddo

         if(ngood.ne.nclust) then
            nclust = ngood
            ntrack = ngood 
            nclust8 = ngood
            nclust64 = ngood
         endif

         nmax = bigcal_nmaxima
c     write(*,*) 'nmax=',nmax
         do imax=1,nmax
            edge_max(imax) = bigcal_edge_max(imax)
            not_enough(imax) = bigcal_not_enough(imax)
            too_long_x(imax) = bigcal_too_long_x(imax)
            too_long_y(imax) = bigcal_too_long_y(imax)
            below_thresh(imax) = bigcal_below_cut(imax)
            above_max(imax) = bigcal_above_max(imax)
            second_max(imax) = bigcal_second_max(imax)
c     write(*,*) 'max,edge,small,bigx,bigy,cutlo,cuthi,twomax=',
c     $           edge_max(imax),not_enough(imax),too_long_x(imax),too_long_y(imax),
c     $           below_thresh(imax),above_max(imax),second_max(imax)
         enddo
         ngooda = bigcal_all_ngood
         ngoodt = bigcal_time_ngood
         ngoodta = bigcal_atrig_ngood
         ngoodtt = bigcal_ttrig_ngood
c$$$  write(*,*) '(rowmax,colmax,adcmax)=',bigcal_iymax_adc,
c$$$  $          bigcal_ixmax_adc,bigcal_max_adc
         irowmax = bigcal_iymax_adc
         icolmax = bigcal_ixmax_adc
         max_adc = bigcal_max_adc
         
c$$$  write(*,*) 'na,nt,nta,ntt,rowmax,colmax,maxadc=',ngooda,ngoodt,ngoodta,
c$$$  $        ngoodtt,irowmax,icolmax,max_adc
         
         if(bgtype.eq.6.and.ibest>0) then ! this always assumes elastic kinematics--won't always make sense!
c     E_HMS = gebeam - gep_Q2_H/(2.*Mp)
c            T_HMS = gep_ctime_hms
            TH_HMS = gep_etheta_expect_h
            PH_HMS = gep_ephi_expect_h - PI/2. 
            
c     write(*,*) 'thetaH,phiH,dpel=',th_hms,ph_hms
            E_HMS = gep_E_electron
            X_HMS = gep_bx_expect_H
            Y_HMS = gep_by_expect_H
            dPel_HMS = (gep_p_proton - gep_pel_htheta) / hpcentral ! useful to isolate elastics
c     write(*,*) 'e_hms,x_hms,y_hms,dpel=',e_hms,x_hms,y_hms,dpel_hms
         else
c            T_HMS = -9999.
            TH_HMS = -9999.
            PH_HMS = -9999.
            E_HMS = -9999.
            X_HMS = -9999.
            Y_HMS = -9999.
            dpel_hms = -9999.
         endif
      else if(bigcal_ntuple_type.eq.2) then
         nahit = 0
         if(bigcal_prot_ngood.gt.0) then
            do ihit=1,bigcal_prot_ngood
               icol = bigcal_prot_ixgood(ihit)
               irow = bigcal_prot_iygood(ihit)
               icell = icol + 32*(irow-1)
               if(icell.ge.1.and.icell.le.1024.and.
     $              bigcal_prot_adc_good(ihit).ge.bigcal_prot_adc_threshold(icell)
     $              .and. bigcal_prot_adc_good(ihit).le.8192.) then
                  nahit = nahit + 1
                  xa(nahit) = bigcal_prot_ixgood(ihit)
                  ya(nahit) = bigcal_prot_iygood(ihit)
                  aa(nahit) = bigcal_prot_adc_good(ihit)
               endif
            enddo
         endif
         
         if(bigcal_rcs_ngood.gt.0) then
            do ihit=1,bigcal_rcs_ngood
               icol = bigcal_rcs_ixgood(ihit)
               irow = bigcal_rcs_iygood(ihit)
               icell = icol + 30*(irow - 1)
               if(icell.ge.1.and.icell.le.720.and.
     $              bigcal_rcs_adc_good(ihit).ge.bigcal_rcs_adc_threshold(icell)
     $              .and. bigcal_rcs_adc_good(ihit).le.8192.) then
                  nahit = nahit + 1
                  xa(nahit) = bigcal_rcs_ixgood(ihit) + icol/16
                  ya(nahit) = bigcal_rcs_iygood(ihit) + 32
                  aa(nahit) = bigcal_rcs_adc_good(ihit)
               endif
            enddo
         endif
         
         nthit=0
         
         if(bigcal_tdc_nhit.gt.0) then
            do ihit=1,bigcal_tdc_nhit
               if(bigcal_tdc(ihit).gt.10)then
                  nthit = nthit + 1
                  xt(nthit) = bigcal_tdc_igroup(ihit)
                  yt(nthit) = bigcal_tdc_irow(ihit)
                  itdc = xt(nthit) + 4*(yt(nthit)-1)
                  nhit_tdc(itdc) = nhit_tdc(itdc) + 1
                  hn(nthit) = nhit_tdc(itdc)
                  tt(nthit) = bigcal_tdc_raw(ihit)
c$$$  write(*,*) 'nthit,xt,yt,hn,tt = ',nthit,xt(nthit),
c$$$  $             yt(nthit),hn(nthit),tt(nthit)
               endif
            enddo
         endif
         
         ntahit=0
         ntthit=0
         
         if(bigcal_atrig_ngood.gt.0) then
            do ihit=1,bigcal_atrig_ngood
               icol = bigcal_atrig_good_ihalf(ihit)
               irow = bigcal_atrig_good_igroup(ihit)
               icell = icol + 2*(irow-1)
               if(icell.ge.1.and.icell.le.38.and.
     $              bigcal_atrig_adc_good(ihit).ge.bigcal_trig_adc_threshold(icell)
     $              .and.bigcal_atrig_adc_good(ihit).le.8192.)then
                  ntahit =  ntahit + 1
                  xta(ntahit) = icol
                  yta(ntahit) = irow
                  taa(ntahit) = bigcal_atrig_adc_good(ihit)
               endif
            enddo
         endif

         if(bigcal_ttrig_nhit.gt.0) then
            do ihit=1,bigcal_ttrig_nhit
               icol = bigcal_ttrig_ihalf(ihit)
               irow = bigcal_ttrig_igroup(ihit)
               icell = icol + 2*(irow-1)
               if(bigcal_ttrig_tdc_raw(ihit).gt.10) then
                  ntthit = ntthit + 1
                  nhit_ttdc(icell) = nhit_ttdc(icell) + 1
                  xtt(ntthit) = icol
                  ytt(ntthit) = irow
                  hnt(ntthit) = nhit_ttdc(icell)
                  ttt(ntthit) = bigcal_ttrig_tdc_raw(ihit)
               endif
            enddo
         endif
      else if(bigcal_ntuple_type.eq.3) then
         m=0
         m=m+1
         b_ntuple_contents(m) = float(gen_event_id_number)
         m=m+1
         b_ntuple_contents(m) = float(gen_event_type)
         m=m+1
         if(ntrigB.gt.0) then
            b_ntuple_contents(m) = bigcal_end_time - gep_btime(1)
         else
            b_ntuple_contents(m) = bigcal_end_time - bigcal_window_center
         endif
         m=m+1
         b_ntuple_contents(m) = float(bigcal_all_ngood)
         m=m+1
         b_ntuple_contents(m) = float(bigcal_time_ngood)
         m=m+1
         b_ntuple_contents(m) = float(bigcal_atrig_ngood)
         m=m+1
         b_ntuple_contents(m) = float(bigcal_ttrig_ngood)
         m=m+1
         b_ntuple_contents(m) = float(bigcal_iymax_adc)
         m=m+1
         b_ntuple_contents(m) = float(bigcal_ixmax_adc)
         m=m+1
         b_ntuple_contents(m) = bigcal_max_adc
         m=m+1
         b_ntuple_contents(m) = float(bigcal_all_nclstr)
         m=m+1
         b_ntuple_contents(m) = float(bigcal_nmaxima)
         m=m+1
         if(bigcal_itrack_best.gt.0) then
            ibest = bigcal_itrack_best
         else 
            ibest = 1
         endif
         b_ntuple_contents(m) = float(bigcal_all_clstr_ncell(ibest))
         m=m+1
         b_ntuple_contents(m) = float(bigcal_all_clstr_ncellx(ibest))
         m=m+1
         b_ntuple_contents(m) = float(bigcal_all_clstr_ncelly(ibest))
         m=m+1
         b_ntuple_contents(m) = float(bigcal_all_clstr_ncell8(ibest))
         m=m+1
         b_ntuple_contents(m) = float(bigcal_all_clstr_ncell64(ibest))
         m=m+1
         b_ntuple_contents(m) = float(bigcal_all_clstr_iycell(ibest,1))
         m=m+1
         b_ntuple_contents(m) = float(bigcal_all_clstr_ixcell(ibest,1))
         m=m+1
         b_ntuple_contents(m) = bigcal_all_clstr_xcell(ibest,1)
         m=m+1
         b_ntuple_contents(m) = bigcal_all_clstr_ycell(ibest,1)
         m=m+1
         b_ntuple_contents(m) = bigcal_all_clstr_xmom(ibest)
         m=m+1
         b_ntuple_contents(m) = bigcal_all_clstr_ymom(ibest)
         m=m+1
         b_ntuple_contents(m) = bigcal_all_clstr_x(ibest)
         m=m+1
         b_ntuple_contents(m) = bigcal_all_clstr_y(ibest)
         m=m+1
         b_ntuple_contents(m) = bigcal_all_clstr_atot(ibest)
         m=m+1
         b_ntuple_contents(m) = bigcal_all_clstr_etot(ibest)
         m=m+1
         b_ntuple_contents(m) = bigcal_all_clstr_t8cut(ibest)
         m=m+1
         b_ntuple_contents(m) = bigcal_all_clstr_t8rms(ibest)
         m=m+1
         b_ntuple_contents(m) = bigcal_all_clstr_t64cut(ibest)
         m=m+1
         b_ntuple_contents(m) = bigcal_all_clstr_t64rms(ibest)
         m=m+1
         b_ntuple_contents(m) = bigcal_track_thetarad(ibest)
         m=m+1
         b_ntuple_contents(m) = bigcal_track_phirad(ibest)
         m=m+1
         b_ntuple_contents(m) = bigcal_track_energy(ibest)
         m=m+1
         b_ntuple_contents(m) = bigcal_track_coin_time(ibest) - 
     $        (bigcal_end_time - bigcal_window_center)
         m=m+1
         b_ntuple_contents(m) = bigcal_track_tof_cor(ibest)
         m=m+1
         b_ntuple_contents(m) = float(hsnum_fptrack)
         m=m+1
         if(bigcal_itrack_best.gt.0) then

            b_ntuple_contents(m) = bigcal_all_clstr_chi2(ibest)
            m=m+1
            b_ntuple_contents(m) = bigcal_all_clstr_chi2contr(ibest,4)
            m=m+1
            b_ntuple_contents(m) = bigcal_all_clstr_chi2contr(ibest,5)
            m=m+1
            b_ntuple_contents(m) = bigcal_all_clstr_chi2contr(ibest,2)
            m=m+1
            b_ntuple_contents(m) = bigcal_all_clstr_chi2contr(ibest,3)
            m=m+1
            b_ntuple_contents(m) = bigcal_all_clstr_chi2contr(ibest,1)
            m=m+1
            b_ntuple_contents(m) = bigcal_all_clstr_chi2contr(ibest,6)
            m=m+1
            b_ntuple_contents(m) = gep_E_electron
            m=m+1
            b_ntuple_contents(m) = gep_etheta_expect_h
            m=m+1
            b_ntuple_contents(m) = gep_ephi_expect_h - PI/2.
            m=m+1
            b_ntuple_contents(m) = gep_bx_expect_h
            m=m+1
            b_ntuple_contents(m) = gep_by_expect_h
            m=m+1
            b_ntuple_contents(m) = gep_ctime_hms
            m=m+1
            b_ntuple_contents(m) = (gep_p_proton - gep_pel_htheta) / hpcentral
            m=m+1
            b_ntuple_contents(m) = (gep_p_proton - gep_pel_btheta) / hpcentral
            m=m+1
            b_ntuple_contents(m) = gbeam_x
            m=m+1
            b_ntuple_contents(m) = gbeam_y
            m=m+1
            b_ntuple_contents(m) = hszbeam
            m=m+1
            b_ntuple_contents(m) = hsxp_tar
            m=m+1 
            b_ntuple_contents(m) = hsyp_tar
         else
            b_ntuple_contents(m) = -1.
            m=m+1
            b_ntuple_contents(m) = -1.
            m=m+1
            b_ntuple_contents(m) = -1.
            m=m+1
            b_ntuple_contents(m) = -1.
            m=m+1
            b_ntuple_contents(m) = -1.
            m=m+1
            b_ntuple_contents(m) = -1.
            m=m+1
            b_ntuple_contents(m) = -1.
            m=m+1
            b_ntuple_contents(m) = -999.
            m=m+1
            b_ntuple_contents(m) = -999.
            m=m+1
            b_ntuple_contents(m) = -999.
            m=m+1
            b_ntuple_contents(m) = -999.
            m=m+1
            b_ntuple_contents(m) = -999.
            m=m+1
            b_ntuple_contents(m) = -999.
            m=m+1
            b_ntuple_contents(m) = -999.
            m=m+1
            b_ntuple_contents(m) = -999.
            m=m+1
            b_ntuple_contents(m) = gbeam_x
            m=m+1
            b_ntuple_contents(m) = gbeam_y
            m=m+1
            b_ntuple_contents(m) = 0.
            m=m+1 
            b_ntuple_contents(m) = -999.
            m=m+1
            b_ntuple_contents(m) = -999.
         endif
      endif
      
      
      if ( hflag) then
      abort=.not.HEXIST(b_ntuple_ID)
      if(abort) then
         call G_build_note(':Ntuple ID#$ does not exist',
     $        '$',b_ntuple_ID,' ',0.,' ',err)
         call G_add_path(here,err)
      else if(bigcal_ntuple_type.lt.3) then
         
         call HFNT(b_ntuple_ID)
      else if(bigcal_ntuple_type.eq.3) then
         call HFN(b_ntuple_ID,b_ntuple_contents)
         
      endif
      endif
      return 
      end
