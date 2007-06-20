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
c      include 'gen_units.par'
      include 'gen_constants.par'
      include 'gen_run_info.cmn'

      integer i,j,k
      integer irow,icol,icell,jrow,jcol,jcell
      integer irow8,icol8,igroup8
      integer igroup64,ihalf64,ilogic

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
      do i=1,bigcal_atrig_maxhits
        bigcal_atrig_igroup(i) = 0
        bigcal_atrig_ihalf(i) = 0
        bigcal_atrig_adc_raw(i) = 0
        bigcal_atrig_good_igroup(i) = 0
        bigcal_atrig_good_ihalf(i) = 0
        bigcal_atrig_adc_dec(i) = 0.
        bigcal_atrig_adc_good(i) = 0.
        bigcal_atrig_esum(i) = 0.
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

      bigcal_prot_nhit = 0
      bigcal_prot_ngood = 0
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
      enddo
      
      bigcal_rcs_nhit = 0
      bigcal_rcs_ngood = 0
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
      enddo

      bigcal_iymax_adc = 0
      bigcal_ixmax_adc = 0
      bigcal_max_adc = 0.

*     zero "detector" arrays:
      
      do i=1,BIGCAL_PROT_MAXHITS
         BIGCAL_PROT_RAW_DET(i) = 0
         BIGCAL_PROT_GOOD_DET(i) = 0.
c         BIGCAL_PROT_GOOD_HIT(i) = .false.
      enddo

      do i=1,BIGCAL_RCS_MAXHITS
         BIGCAL_RCS_RAW_DET(i) = 0
         BIGCAL_RCS_GOOD_DET(i) = 0.
c         BIGCAL_RCS_GOOD_HIT(i) = .false.
      enddo

      do i=1,BIGCAL_MAX_TDC
        bigcal_tdc_det_nhit(i) = 0
        bigcal_tdc_det_ngood(i) = 0
        bigcal_tdc_sum8(i) = 0.
        do j=1,8
         BIGCAL_TDC_RAW_DET(i,j) = 0
         bigcal_tdc_good_det(i,j) = 0.
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
          bigcal_ttrig_good_det(i,j) = 0.
        enddo
      enddo

      do i=30,35
        do j=1,32
          bigcal_mid_ehit(i,j) = 0.
          bigcal_mid_xhit(i,j) = 0.
          bigcal_mid_yhit(i,j) = 0.
        enddo
      enddo

      bigcal_prot_nclstr = 0
      do i=1,bigcal_prot_nclstr_max
        bigcal_prot_clstr_ncell(i) = 0
        bigcal_prot_clstr_iymax(i) = 0
        bigcal_prot_clstr_ixmax(i) = 0
        do j=1,bigcal_clstr_ncell_max
          bigcal_prot_clstr_iycell(i,j) = 0
          bigcal_prot_clstr_ixcell(i,j) = 0
          bigcal_prot_clstr_xcell(i,j) = 0.
          bigcal_prot_clstr_ycell(i,j) = 0.
          bigcal_prot_clstr_ecell(i,j) = 0.
        enddo
        bigcal_prot_clstr_ncell8(i) = 0
        bigcal_prot_clstr_ncell64(i) = 0
        do j=1,10
          bigcal_prot_clstr_nhit8(i,j) = 0
          do k=1,8
            bigcal_prot_clstr_tcell8(i,j,k) = 0.
          enddo
        enddo
        do j=1,6
          bigcal_prot_clstr_nhit64(i,j) = 0
          do k=1,8
            bigcal_prot_clstr_tcell64(i,j,k) = 0.
          enddo
        enddo
        bigcal_prot_clstr_xmom(i) = 0.
        bigcal_prot_clstr_ymom(i) = 0.
        bigcal_prot_clstr_t8best(i) = 0.
        bigcal_prot_clstr_t64best(i) = 0.
        bigcal_prot_clstr_x(i) = 0.
        bigcal_prot_clstr_y(i) = 0.
        bigcal_prot_clstr_etot(i) = 0.
        bigcal_prot_clstr_good8(i) = .false.
        bigcal_prot_clstr_good64(i) = .false.
      enddo

      bigcal_rcs_nclstr = 0
      do i=1,bigcal_rcs_nclstr_max
        bigcal_rcs_clstr_ncell(i) = 0
        bigcal_rcs_clstr_iymax(i) = 0
        bigcal_rcs_clstr_ixmax(i) = 0
        do j=1,bigcal_clstr_ncell_max
          bigcal_rcs_clstr_iycell(i,j) = 0
          bigcal_rcs_clstr_ixcell(i,j) = 0
          bigcal_rcs_clstr_xcell(i,j) = 0.
          bigcal_rcs_clstr_ycell(i,j) = 0.
          bigcal_rcs_clstr_ecell(i,j) = 0.
        enddo
        bigcal_rcs_clstr_ncell8(i) = 0
        bigcal_rcs_clstr_ncell64(i) = 0
        do j=1,10
          bigcal_rcs_clstr_nhit8(i,j) = 0
          do k=1,8
            bigcal_rcs_clstr_tcell8(i,j,k) = 0.
          enddo
        enddo
        do j=1,6
          bigcal_rcs_clstr_nhit64(i,j) = 0
          do k=1,8
            bigcal_rcs_clstr_tcell64(i,j,k) = 0.
          enddo
        enddo
        bigcal_rcs_clstr_xmom(i) = 0.
        bigcal_rcs_clstr_ymom(i) = 0.
        bigcal_rcs_clstr_t8best(i) = 0.
        bigcal_rcs_clstr_t64best(i) = 0.
        bigcal_rcs_clstr_x(i) = 0.
        bigcal_rcs_clstr_y(i) = 0.
        bigcal_rcs_clstr_etot(i) = 0.
        bigcal_rcs_clstr_good8(i) = .false.
        bigcal_rcs_clstr_good64(i) = .false.
      enddo

      bigcal_mid_nclstr = 0
      do i=1,bigcal_mid_nclstr_max
        bigcal_mid_clstr_ncell(i) = 0
        bigcal_mid_clstr_iymax(i) = 0
        bigcal_mid_clstr_ixmax(i) = 0
        do j=1,bigcal_clstr_ncell_max
          bigcal_mid_clstr_iycell(i,j) = 0
          bigcal_mid_clstr_ixcell(i,j) = 0
          bigcal_mid_clstr_xcell(i,j) = 0.
          bigcal_mid_clstr_ycell(i,j) = 0.
          bigcal_mid_clstr_ecell(i,j) = 0.
        enddo
        bigcal_mid_clstr_ncell8(i) = 0
        bigcal_mid_clstr_ncell64(i) = 0
        do j=1,10
          bigcal_mid_clstr_nhit8(i,j) = 0
          do k=1,8
            bigcal_mid_clstr_tcell8(i,j,k) = 0.
          enddo
        enddo
        do j=1,6
          bigcal_mid_clstr_nhit64(i,j) = 0
          do k=1,8
            bigcal_mid_clstr_tcell64(i,j,k) = 0.
          enddo
        enddo
        bigcal_mid_clstr_xmom(i) = 0.
        bigcal_mid_clstr_ymom(i) = 0.
        bigcal_mid_clstr_t8best(i) = 0.
        bigcal_mid_clstr_t64best(i) = 0.
        bigcal_mid_clstr_x(i) = 0.
        bigcal_mid_clstr_y(i) = 0.
        bigcal_mid_clstr_etot(i) = 0.
        bigcal_mid_clstr_good8(i) = .false.
        bigcal_mid_clstr_good64(i) = .false.
      enddo

      bigcal_phys_ntrack = 0
      bigcal_best_track = 0
      bigcal_best_clstr_isection = 0
      bigcal_best_clstr_icluster = 0
      
      bigcal_best_thetarad = 0.
      bigcal_best_thetadeg = 0.
      bigcal_best_phirad = 0.
      bigcal_best_phideg = 0.
      bigcal_best_energy = 0.
      bigcal_best_time = 0.
      bigcal_best_xface = 0.
      bigcal_best_yface = 0.
      bigcal_best_zface = 0.
      bigcal_best_px = 0.
      bigcal_best_py = 0.
      bigcal_best_pz = 0.
      bigcal_best_beta = 0.
      bigcal_best_tof = 0.
      bigcal_best_coin_time = 0.

      do i=1,bigcal_max_ntrack
        bigcal_track_thetarad(i) = 0.
        bigcal_track_thetadeg(i) = 0.
        bigcal_track_phirad(i) = 0.
        bigcal_track_phideg(i) = 0.
        bigcal_track_energy(i) = 0.
        bigcal_track_time(i) = 0.
        bigcal_track_xface(i) = 0.
        bigcal_track_yface(i) = 0.
        bigcal_track_zface(i) = 0.
        bigcal_track_px(i) = 0.
        bigcal_track_py(i) = 0.
        bigcal_track_pz(i) = 0.
        bigcal_track_beta(i) = 0.
        bigcal_track_tof(i) = 0.
        bigcal_track_coin_time(i) = 0.
      enddo

      if(gen_bigcal_mc.ne.0) then
         !override pedestals info from param file: set all to zero for monte carlo events:
         !also override calibration constants:
         do i=1,bigcal_prot_maxhits
            bigcal_prot_ped_mean(i) = 0.0
            bigcal_prot_ped_rms(i) = 0.0
            bigcal_prot_adc_threshold(i) = 0.0
            bigcal_prot_cfac(i) = 1./950.79
         enddo
         do i=1,bigcal_rcs_maxhits
            bigcal_rcs_ped_mean(i) = 0.0
            bigcal_rcs_ped_rms(i) = 0.0
            bigcal_rcs_adc_threshold(i) = 0.0
            bigcal_rcs_cfac(i) = 1./911.57
         enddo
      endif

      return
      end
