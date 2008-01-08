      subroutine b_reset_event(ABORT,err)

      implicit none
      save
      
      character*13 here
      parameter(here='b_reset_event')

      logical ABORT
      character*(*) err

      include 'bigcal_data_structures.cmn'
      include 'bigcal_gain_parms.cmn'
      include 'bigcal_geometry.cmn'
      include 'bigcal_shower_parms.cmn'
      include 'bigcal_tof_parms.cmn'
      include 'b_ntuple.cmn'
c      include 'gen_units.par'
c      include 'gen_constants.par'
      include 'gen_run_info.cmn'

      integer i,j,k
      integer irow,icol,icell,jrow,jcol,jcell
      integer irow8,icol8,igroup8
      integer igroup64,ihalf64,ilogic


      bigcal_annoying_pulser_event = .false.
      bigcal_tdc_nhit = 0
      bigcal_tdc_ndecoded = 0
      bigcal_time_ngood = 0
      do i=1,bigcal_tdc_maxhits
        bigcal_tdc_raw_irow(i) = 0
        bigcal_tdc_raw_igroup(i) = 0
        bigcal_tdc_raw(i) = 0
        bigcal_tdc_irow(i) = 0
        bigcal_tdc_igroup(i) = 0
        bigcal_tdc(i) = 0
        bigcal_time_irow(i) = 0
        bigcal_time_igroup(i) = 0
        bigcal_tdc_good(i) = 0
        bigcal_hit_time(i) = 0.
      enddo
      
      bigcal_atrig_nhit = 0
      bigcal_atrig_ngood = 0
      bigcal_atrig_nbad = 0
      do i=1,bigcal_atrig_maxhits
        bigcal_atrig_igroup(i) = 0
        bigcal_atrig_ihalf(i) = 0
        bigcal_atrig_adc_raw(i) = 0
        bigcal_atrig_good_igroup(i) = 0
        bigcal_atrig_good_ihalf(i) = 0
        bigcal_atrig_adc_dec(i) = 0.
        bigcal_atrig_adc_good(i) = 0.
        bigcal_atrig_esum(i) = 0.

        bigcal_atrig_igroup_bad(i) = 0
        bigcal_atrig_ihalf_bad(i) = 0
        bigcal_atrig_adc_bad(i) = 0

        bigcal_atrig_nhit_ch(i) = 0
      enddo

      bigcal_ttrig_nhit = 0
      bigcal_ttrig_ndecoded = 0
      bigcal_ttrig_ngood = 0
      do i=1,bigcal_ttrig_maxhits
        bigcal_ttrig_igroup(i) = 0
        bigcal_ttrig_ihalf(i) = 0
        bigcal_ttrig_tdc_raw(i) = 0
        bigcal_ttrig_dec_igroup(i) = 0
        bigcal_ttrig_dec_ihalf(i) = 0
        bigcal_ttrig_tdc_dec(i) = 0
        bigcal_ttrig_good_igroup(i) = 0
        bigcal_ttrig_good_ihalf(i) = 0
        bigcal_ttrig_tdc_good(i) = 0
        bigcal_ttrig_time_good(i) = 0.
      enddo

      bigcal_all_ngood = 0

      bigcal_prot_nhit = 0
      bigcal_prot_ngood = 0
      bigcal_prot_nbad = 0
      do i=1,bigcal_prot_maxhits
        bigcal_prot_iy(i) = 0
        bigcal_prot_ix(i) = 0
        bigcal_prot_adc_raw(i) = 0
        bigcal_prot_iygood(i) = 0
        bigcal_prot_ixgood(i) = 0
        bigcal_prot_adc_decoded(i) = 0.
        bigcal_prot_adc_good(i) = 0.
        bigcal_prot_ecell(i) = 0.
        bigcal_prot_xgood(i) = 0.
        bigcal_prot_ygood(i) = 0.

        bigcal_prot_iybad(i) = 0
        bigcal_prot_ixbad(i) = 0
        bigcal_prot_adc_bad(i) = 0

        bigcal_prot_nhit_ch(i) = 0

        bigcal_all_iygood(i) = 0
        bigcal_all_ixgood(i) = 0
        bigcal_all_adc_good(i) = 0.
        bigcal_all_ecell(i) = 0.
        bigcal_all_xgood(i) = 0.
        bigcal_all_ygood(i) = 0.
        b_all_run_clst_good(i) = 0
        b_all_run_clst_bad(i) = 0
        b_all_run_clst_eff(i) = 0.
        b_all_run_Esum(i) = 0.
        b_all_run_Enum(i) = 0
      enddo
      
      bigcal_rcs_nhit = 0
      bigcal_rcs_ngood = 0
      bigcal_rcs_nbad = 0
      do i=1,bigcal_rcs_maxhits
        bigcal_rcs_iy(i) = 0
        bigcal_rcs_ix(i) = 0
        bigcal_rcs_adc_raw(i) = 0
        bigcal_rcs_iygood(i) = 0
        bigcal_rcs_ixgood(i) = 0
        bigcal_rcs_adc_decoded(i) = 0.
        bigcal_rcs_adc_good(i) = 0.
        bigcal_rcs_ecell(i) = 0.
        bigcal_rcs_xgood(i) = 0.
        bigcal_rcs_ygood(i) = 0.

        bigcal_rcs_iybad(i) = 0
        bigcal_rcs_ixbad(i) = 0
        bigcal_rcs_adc_bad(i) = 0

        bigcal_rcs_nhit_ch(i) = 0

        bigcal_all_iygood(i+bigcal_prot_maxhits) = 0
        bigcal_all_ixgood(i+bigcal_prot_maxhits) = 0
        bigcal_all_adc_good(i+bigcal_prot_maxhits) = 0.
        bigcal_all_ecell(i+bigcal_prot_maxhits) = 0.
        bigcal_all_xgood(i+bigcal_prot_maxhits) = 0.
        bigcal_all_ygood(i+bigcal_prot_maxhits) = 0.
        b_all_run_clst_good(i+bigcal_prot_maxhits) = 0
        b_all_run_clst_bad(i+bigcal_prot_maxhits) = 0
        b_all_run_clst_eff(i+bigcal_prot_maxhits) = 0.
        b_all_run_Esum(i+bigcal_prot_maxhits) = 0.
        b_all_run_Enum(i+bigcal_prot_maxhits) = 0
      enddo

      bigcal_iymax_adc = 0
      bigcal_ixmax_adc = 0
      bigcal_max_adc = 0.

c$$$      bigcal_iymax_final = 0
c$$$      bigcal_ixmax_final = 0
c$$$      bigcal_max_adc_final = 0.

*     zero "detector" arrays:
      
      do i=1,BIGCAL_PROT_MAXHITS
         BIGCAL_PROT_RAW_DET(i) = 0
         BIGCAL_PROT_GOOD_DET(i) = 0.
c         BIGCAL_PROT_GOOD_HIT(i) = .false.
         BIGCAL_ALL_ADC_DET(i) = 0.
         BIGCAL_ALL_GOOD_DET(i) = 0.
      enddo

      do i=1,BIGCAL_RCS_MAXHITS
         BIGCAL_RCS_RAW_DET(i) = 0
         BIGCAL_RCS_GOOD_DET(i) = 0.
c         BIGCAL_RCS_GOOD_HIT(i) = .false.
         BIGCAL_ALL_ADC_DET(i+bigcal_prot_maxhits) = 0.
         BIGCAL_ALL_GOOD_DET(i+bigcal_prot_maxhits) = 0.
      enddo

      do i=1,BIGCAL_MAX_TDC
        bigcal_tdc_det_nhit(i) = 0
        bigcal_tdc_det_ngood(i) = 0
        bigcal_tdc_sum8(i) = 0.
        do j=1,8
         BIGCAL_TDC_RAW_DET(i,j) = 0
         bigcal_tdc_good_det(i,j) = -9999.
       enddo
      enddo

      do i=1,bigcal_atrig_maxhits
        bigcal_atrig_raw_det(i) = 0
        bigcal_atrig_good_det(i) = 0.
        bigcal_atrig_sum64(i) = 0.
      enddo

      do i=1,bigcal_ttrig_maxgroups
        bigcal_ttrig_det_nhit(i) = 0
        bigcal_ttrig_det_ngood(i) = 0
        do j=1,8
          bigcal_ttrig_raw_det(i,j) = 0
          bigcal_ttrig_good_det(i,j) = -9999.
        enddo
      enddo

c$$$      do i=30,35
c$$$        do j=1,32
c$$$          bigcal_mid_ehit(i,j) = 0.
c$$$          bigcal_mid_xhit(i,j) = 0.
c$$$          bigcal_mid_yhit(i,j) = 0.
c$$$        enddo
c$$$      enddo

c      bigcal_prot_bad_clstr_flag(0) = 0

      bigcal_all_nclstr = 0
      bigcal_nmaxima = 0
      do i=1,bigcal_all_nclstr_max
         bigcal_all_clstr_ncell(i) = 0
         bigcal_all_clstr_ncellx(i) = 0
         bigcal_all_clstr_ncelly(i)= 0
         bigcal_all_clstr_nbadlist(i) = 0
         bigcal_all_clstr_iymax(i) = 0
         bigcal_all_clstr_ixmax(i) = 0
         bigcal_all_clstr_iylo(i)=0
         bigcal_all_clstr_iyhi(i) = 0
         do j=1,3
            bigcal_all_clstr_ixlo(i,j) = 0
            bigcal_all_clstr_ixhi(i,j) = 0
         enddo
         bigcal_all_clstr_ncell8(i) = 0
         bigcal_all_clstr_ncell64(i) = 0
         bigcal_all_clstr_xmom(i) = 0.
         bigcal_all_clstr_ymom(i) = 0.
         bigcal_all_clstr_x(i) = 0.
         bigcal_all_clstr_y(i) = 0.
         bigcal_all_clstr_etot(i) = 0.
         bigcal_all_clstr_atot(i) = 0.
         bigcal_all_clstr_t8mean(i) = 0.
         bigcal_all_clstr_t8rms(i) = 0.
         bigcal_all_clstr_t8cut(i) = 0.
         bigcal_all_clstr_t8cut_cor(i) = 0.
         bigcal_all_clstr_t64mean(i) = 0.
         bigcal_all_clstr_t64rms(i) = 0.
         bigcal_all_clstr_t64cut(i) = 0.
         bigcal_all_clstr_t64cut_cor(i) = 0.
         bigcal_all_clstr_chi2(i) = 0.
         do j=1,6
            bigcal_all_clstr_chi2contr(i,j) = 0.
         enddo
         do j=1,bigcal_clstr_ncell_max
            bigcal_all_clstr_iycell(i,j) = 0
            bigcal_all_clstr_ixcell(i,j) = 0
            bigcal_all_clstr_ycell(i,j) = 0.
            bigcal_all_clstr_xcell(i,j) = 0.
            bigcal_all_clstr_ecell(i,j) = 0.
            bigcal_all_clstr_acell(i,j) = 0.
            bigcal_clstr_bad_chan(i,j) = .false.
         enddo
         
         do j=1,10
            bigcal_all_clstr_nhit8(i,j) = 0
            bigcal_all_clstr_irow8(i,j) = 0
            bigcal_all_clstr_icol8(i,j) = 0
            bigcal_all_clstr_s8(i,j) = 0.
            do k=1,8
               bigcal_all_clstr_tcell8(i,j,k) = 0.
            enddo
         enddo

         do j=1,6
            bigcal_all_clstr_nhit64(i,j) = 0
            bigcal_all_clstr_irow64(i,j) = 0
            bigcal_all_clstr_icol64(i,j) = 0
            bigcal_all_clstr_sum64(i,j) = 0.
            do k=1,8
               bigcal_all_clstr_tcell64(i,j,k) = 0.
            enddo
         enddo
         
         bigcal_edge_max(i) = .false.
         bigcal_not_enough(i) = .false.
         bigcal_too_long_x(i) = .false.
         bigcal_too_long_y(i) = .false.
         bigcal_below_cut(i) = .false.
         bigcal_above_max(i) = .false.
         bigcal_second_max(i) = .false.
         
      enddo

      bigcal_phys_ntrack = 0
      bigcal_itrack_best = 0
      
      bigcal_thetarad = 0.
c      bigcal_thetadeg = 0.
      bigcal_phirad = 0.
c      bigcal_phideg = 0.
      bigcal_energy = 0.
      bigcal_time = 0.
      bigcal_xface = 0.
      bigcal_yface = 0.
      bigcal_zface = 0.
      bigcal_px = 0.
      bigcal_py = 0.
      bigcal_pz = 0.
      bigcal_beta = 0.
      bigcal_eloss = 0.
      bigcal_tof_cor = 0.
      bigcal_tof = 0.
      bigcal_ctime = 0.

      do i=1,bigcal_max_ntrack
        bigcal_track_thetarad(i) = 0.
        bigcal_track_thetadeg(i) = 0.
        bigcal_track_phirad(i) = 0.
        bigcal_track_phideg(i) = 0.
        bigcal_track_energy(i) = 0.
        bigcal_track_eloss(i) = 0.
        bigcal_track_time(i) = 0.
        bigcal_track_xface(i) = 0.
        bigcal_track_yface(i) = 0.
        bigcal_track_zface(i) = 0.
        bigcal_track_px(i) = 0.
        bigcal_track_py(i) = 0.
        bigcal_track_pz(i) = 0.
        bigcal_track_beta(i) = 0.
        bigcal_track_tof(i) = 0.
        bigcal_track_tof_cor(i) = 0.
        bigcal_track_coin_time(i) = 0.
      enddo

c$$$      if(gen_bigcal_mc.ne.0) then
c$$$         !override pedestals info from param file: set all to zero for monte carlo events:
c$$$         !also override calibration constants:
c$$$         do i=1,bigcal_prot_maxhits
c$$$            bigcal_prot_ped_mean(i) = 0.0
c$$$            bigcal_prot_ped_rms(i) = 0.0
c$$$            bigcal_prot_adc_threshold(i) = 0.0
c$$$            bigcal_prot_cfac(i) = 1./950.79
c$$$         enddo
c$$$         do i=1,bigcal_rcs_maxhits
c$$$            bigcal_rcs_ped_mean(i) = 0.0
c$$$            bigcal_rcs_ped_rms(i) = 0.0
c$$$            bigcal_rcs_adc_threshold(i) = 0.0
c$$$            bigcal_rcs_cfac(i) = 1./911.57
c$$$         enddo
c$$$      endif

c     clear out cluster ntuple variables:

      nclust = 0
      nclust8 = 0
      nclust64 = 0
      ntrack = 0 
      ibest = 0
      nmax = 0
      do i=1,maxnclust
         ncellclust(i) = 0
         ncellbad(i) = 0
         ncellx(i) = 0
         ncelly(i) = 0
         ncell8clust(i) = 0
         ncell64clust(i) = 0
         xmoment(i) = 0.
         ymoment(i) = 0.
         tclust8(i) = 0.
         tcut8(i) = 0.
         tcut8cor(i) = 0.
         trms8(i) = 0.
         tclust64(i) = 0.
         tcut64(i) = 0.
         tcut64cor(i) = 0.
         trms64(i) = 0.
         xclust(i) = 0.
         yclust(i) = 0.
         eclust(i) = 0.
         aclust(i) = 0.
         do j=1,maxncellclust
            iycell(j,i) = 0
            ixcell(j,i) = 0
            xcell(j,i) = 0.
            ycell(j,i) = 0.
            eblock(j,i) = 0.
            cellbad(j,i) = .false.
         enddo
         do j=1,10
            irow8hit(j,i) = 0
            icol8hit(j,i) = 0
            nhit8clust(j,i) = 0
            s8(j,i) = 0.
            do k=1,8
               tcell8(j,k,i) = 0.
            enddo
         enddo
         do j=1,6
            irow64hit(j,i) = 0
            icol64hit(j,i) = 0
            nhit64clust(j,i) = 0.
            a64(j,i) = 0.
            s64(j,i) = 0.
            do k=1,8
               tcell64(j,k,i) = 0.
            enddo
         enddo
         thetarad(i) = 0.
         phirad(i) = 0.
         xface(i) = 0.
         yface(i) = 0.
         zface(i) = 0.
         px(i) = 0.
         py(i) = 0.
         pz(i) = 0.
         ctime_clust(i) = 0.
         chi2clust(i) = 0.
         do j=1,6
            chi2contr(j,i) = 0.
         enddo
         edge_max(i) = .false.
         not_enough(i) = .false.
         too_long_x(i) = .false.
         too_long_y(i) = .false.
         below_thresh(i) = .false.
         above_max(i) = .false.
         second_max(i) = .false.
      enddo

      ngooda = 0
      ngoodt = 0
      ngoodta = 0
      ngoodtt = 0
      irowmax = 0
      icolmax = 0
      max_adc = 0.

c     clear out monte carlo event info variables:
      
      ntrk_g = 0
      xvertex_g = 0.
      yvertex_g = 0.
      zvertex_g = 0.
      do i=1,maxnclust
         pid_g(i) = 0
         pxgeant(i) = 0.
         pygeant(i) = 0.
         pzgeant(i) = 0.
         xgeant(i) = 0.
         ygeant(i) = 0.
         egeant(i) = 0.
         pgeant(i) = 0.
         gthetarad(i) = 0.
         gphirad(i) = 0.
      enddo

      E_HMS = 0.
      X_HMS = 0.
      Y_HMS = 0.
      TH_HMS = 0.
      PH_HMS = 0.
      dPel_HMS = 0.

      return
      end
