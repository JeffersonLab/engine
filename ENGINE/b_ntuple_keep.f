      subroutine b_ntuple_keep(ABORT,err)

      implicit none
      save

      character*13 here
      parameter(here='b_ntuple_keep')

      logical abort
      character*(*) err

      include 'b_ntuple.cmn'
      include 'bigcal_data_structures.cmn'
      include 'hms_data_structures.cmn'
      include 'bigcal_gain_parms.cmn'
      include 'gen_event_info.cmn'
      include 'gen_data_structures.cmn'
      include 'gep_data_structures.cmn'
c      include 'gen_scalers.cmn'
      

      logical HEXIST ! CERNLIB function

c      logical middlebest

      integer m,np,nr,nm,irow,icol,ihit,jhit,itdc,itrig
c      real ep,er,em
c      integer iybest,ixbest,xclst,yclst,Eclst,xmom,ymom,t8avg,t64avg
      integer L8sum,L64sum,iydiff,ixdiff,icell,jcell,iarray
c      real ecell(BIGCAL_CLSTR_NCELL_MAX)

c      integer itype
      integer jclust,iclust,imax
      integer nhit_tdc(224)
      integer nhit_ttdc(42)
      integer nhit_tadc(38)

      real Mp
      parameter(Mp=.938272)

      do ihit=1,224
        nhit_tdc(ihit) = 0
        if(ihit.le.42) nhit_ttdc(ihit) = 0
        if(ihit.le.38) nhit_tadc(ihit) = 0
      enddo

      err=' '
      ABORT=.false.

      if(.not.b_ntuple_exists) return

      if(b_ntuple_max_segmentevents.gt.0) then
         if(b_ntuple_segmentevents.gt.b_ntuple_max_segmentevents) then
            call b_ntuple_change(ABORT,err)
            b_ntuple_segmentevents=0
         else 
            b_ntuple_segmentevents = b_ntuple_segmentevents + 1
         endif
      endif

      if(bigcal_ntuple_type.eq.1) then
         bgid = gen_event_ID_number
         bgtype = gen_event_type
         btrigtype = 0
         do itrig=3,7
            btrigtype = btrigtype + itrig*gen_event_trigtype(itrig)
         enddo

         !write(*,*) 'filling BigCal ntuple'
         !write(*,*) 'event,type,trigtype=',bgid,bgtype,btrigtype

         nclust = bigcal_all_nclstr
         nclust8 = nclust
         nclust64 = nclust
         ntrack = nclust
         ibest = bigcal_itrack_best

         !write(*,*) 'nclust,ncl8,ncl64,ntrk,best=',nclust,nclust8,
c     $        nclust64,ntrack,ibest

         do iclust = 1,nclust
            ncellclust(iclust) = bigcal_all_clstr_ncell(iclust)
            ncellbad(iclust) = bigcal_all_clstr_nbadlist(iclust)
            ncellx(iclust) = bigcal_all_clstr_ncellx(iclust)
            ncelly(iclust) = bigcal_all_clstr_ncelly(iclust)
            ncell8clust(iclust) = bigcal_all_clstr_ncell8(iclust)
            ncell64clust(iclust) = bigcal_all_clstr_ncell64(iclust)

            !write(*,*) 'iclust,ncell,nbad,nx,ny,n8,n64=',iclust,ncellclust(iclust),
c     $           ncellbad(iclust),ncellx(iclust),ncelly(iclust),ncell8clust(iclust),
c     $           ncell64clust(iclust)

            do icell=1,ncellclust(iclust)
               iycell(icell,iclust) = bigcal_all_clstr_iycell(iclust,icell)
               ixcell(icell,iclust) = bigcal_all_clstr_ixcell(iclust,icell)
               xcell(icell,iclust) = bigcal_all_clstr_xcell(iclust,icell)
               ycell(icell,iclust) = bigcal_all_clstr_ycell(iclust,icell)
               eblock(icell,iclust) = bigcal_all_clstr_ecell(iclust,icell)
               cellbad(icell,iclust) = bigcal_clstr_bad_chan(iclust,icell)
               !write(*,*) 'cell,row,col,x,y,e,bad?=',icell,iycell(icell,iclust),
c     $              ixcell(icell,iclust),xcell(icell,iclust),ycell(icell,iclust),
c     $              eblock(icell,iclust),cellbad(icell,iclust)
            enddo
c     zero all cells above ncellclust
            do icell=ncellclust(iclust)+1,bigcal_clstr_ncell_max
               iycell(icell,iclust) = 0
               ixcell(icell,iclust) = 0
               xcell(icell,iclust) = 0.
               ycell(icell,iclust) = 0.
               eblock(icell,iclust) = 0.
               cellbad(icell,iclust) = .false.
            enddo
            
            do icell=1,ncell8clust(iclust)
               irow8hit(icell,iclust) = bigcal_all_clstr_irow8(iclust,icell)
               icol8hit(icell,iclust) = bigcal_all_clstr_icol8(iclust,icell)
               nhit8clust(icell,iclust) = bigcal_all_clstr_nhit8(iclust,icell)

               !write(*,*) 'cell8,row8,col8,nh=',icell,irow8hit(icell,iclust),
c     $              icol8hit(icell,iclust),nhit8clust(icell,iclust)

               do ihit=1,nhit8clust(icell,iclust)
                  !write(*,*) 'hit,time=',ihit,tcell8(icell,ihit,iclust)
                  tcell8(icell,ihit,iclust) = bigcal_all_clstr_tcell8(iclust,icell,ihit)
               enddo
c     zero all hits above nhit8clust(icell,iclust)
               do ihit=nhit8clust(icell,iclust)+1,8
                  tcell8(icell,ihit,iclust) = 0.
               enddo
            enddo
c     zero all cells and all hits of all cells above ncell8clust
            do icell=ncell8clust(iclust)+1,10
               irow8hit(icell,iclust) = 0
               icol8hit(icell,iclust) = 0
               nhit8clust(icell,iclust) = 0
               do ihit=1,8
                  tcell8(icell,ihit,iclust) = 0.
               enddo
            enddo

            do icell=1,ncell64clust(iclust)
               irow64hit(icell,iclust) = bigcal_all_clstr_irow64(iclust,icell)
               icol64hit(icell,iclust) = bigcal_all_clstr_icol64(iclust,icell)
               nhit64clust(icell,iclust) = bigcal_all_clstr_nhit64(iclust,icell)
               a64(icell,iclust) = bigcal_all_clstr_a64(iclust,icell)
               s64(icell,iclust) = bigcal_all_clstr_sum64(iclust,icell)
               !write(*,*) 'cell64,row64,col64,nh,a64,s64=',icell,irow64hit(icell,iclust),
c     $              icol64hit(icell,iclust),nhit64clust(icell,iclust),a64(icell,iclust),
c     $              s64(icell,iclust)
               do ihit=1,nhit64clust(icell,iclust)
                  !write(*,*) 'hit,time=',ihit,tcell64(icell,ihit,iclust)
                  tcell64(icell,ihit,iclust) = bigcal_all_clstr_tcell64(iclust,icell,ihit)
               enddo
c     zero all hits above nhit64clust(icell,iclust)
               do ihit=nhit64clust(icell,iclust)+1,8
                  tcell64(icell,ihit,iclust) = 0.
               enddo
            enddo
c     zero all cells and all hits of all cells above ncell64clust
            do icell=ncell64clust(iclust)+1,6
               irow64hit(icell,iclust) = 0
               icol64hit(icell,iclust) = 0
               nhit64clust(icell,iclust) = 0
               do ihit=1,8
                  tcell64(icell,ihit,iclust) = 0.
               enddo
            enddo

            xmoment(iclust) = bigcal_all_clstr_xmom(iclust)
            ymoment(iclust) = bigcal_all_clstr_ymom(iclust)
            tclust8(iclust) = bigcal_all_clstr_t8mean(iclust)
            tclust64(iclust) = bigcal_all_clstr_t64mean(iclust)
            trms8(iclust) = bigcal_all_clstr_t8rms(iclust)
            trms64(iclust) = bigcal_all_clstr_t64rms(iclust)

            !write(*,*) 'xmom,ymom,t8,t64,trms8,trms64=',xmoment(iclust),
c     $           ymoment(iclust),tclust8(iclust),tclust64(iclust),trms8(iclust),
c     $           trms64(iclust)

            xclust(iclust) = bigcal_all_clstr_x(iclust)
            yclust(iclust) = bigcal_all_clstr_y(iclust)
            eclust(iclust) = bigcal_all_clstr_etot(iclust)

            !write(*,*) 'xclust,yclust,eclust=',xclust(iclust),yclust(iclust),
c     $           eclust(iclust)

            thetarad(iclust) = bigcal_track_thetarad(iclust)
            phirad(iclust) = bigcal_track_phirad(iclust)
            energy(iclust) = bigcal_track_energy(iclust)
            xface(iclust) = bigcal_track_xface(iclust)
            yface(iclust) = bigcal_track_yface(iclust)
            zface(iclust) = bigcal_track_zface(iclust)
            px(iclust) = bigcal_track_px(iclust)
            py(iclust) = bigcal_track_py(iclust)
            pz(iclust) = bigcal_track_pz(iclust)
            ctime_clust(iclust) = bigcal_track_coin_time(iclust)

            !write(*,*) 'theta,phi,E,xf,yf,zf,px,py,pz,t=',thetarad(iclust),phirad(iclust),
c     $           energy(iclust),xface(iclust),yface(iclust),zface(iclust),px(iclust),py(iclust),
c     $           pz(iclust),ctime_clust(iclust)

         enddo
         nmax = bigcal_nmaxima
         !write(*,*) 'nmax=',nmax
         do imax=1,nmax
            edge_max(imax) = bigcal_edge_max(imax)
            not_enough(imax) = bigcal_not_enough(imax)
            too_long_x(imax) = bigcal_too_long_x(imax)
            too_long_y(imax) = bigcal_too_long_y(imax)
            below_thresh(imax) = bigcal_below_cut(imax)
            above_max(imax) = bigcal_above_max(imax)
            second_max(imax) = bigcal_second_max(imax)
            !write(*,*) 'max,edge,small,bigx,bigy,cutlo,cuthi,twomax=',
c     $           edge_max(imax),not_enough(imax),too_long_x(imax),too_long_y(imax),
c     $           below_thresh(imax),above_max(imax),second_max(imax)
         enddo
         ngooda = bigcal_all_ngood
         ngoodt = bigcal_time_ngood
         ngoodta = bigcal_atrig_ngood
         ngoodtt = bigcal_ttrig_ngood
c$$$         write(*,*) '(rowmax,colmax,adcmax)=',bigcal_iymax_adc,
c$$$     $          bigcal_ixmax_adc,bigcal_max_adc
         irowmax = bigcal_iymax_adc
         icolmax = bigcal_ixmax_adc
         max_adc = bigcal_max_adc

c$$$         write(*,*) 'na,nt,nta,ntt,rowmax,colmax,maxadc=',ngooda,ngoodt,ngoodta,
c$$$     $        ngoodtt,irowmax,icolmax,max_adc

         if(bgtype.eq.6) then ! this always assumes elastic kinematics--won't always make sense!
c            E_HMS = gebeam - gep_Q2_H/(2.*Mp)
            E_HMS = gep_E_electron
            X_HMS = gep_bx_expect_H
            Y_HMS = gep_by_expect_H
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
      endif
      
      
      abort=.not.HEXIST(b_ntuple_ID)
      if(abort) then
         call G_build_note(':Ntuple ID#$ does not exist',
     $        '$',b_ntuple_ID,' ',0.,' ',err)
         call G_add_path(here,err)
      else 
         
         call HFNT(b_ntuple_ID)
         
      endif

      return 
      end
