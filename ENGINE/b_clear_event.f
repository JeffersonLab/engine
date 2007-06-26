      subroutine B_clear_event(ABORT,err)

      IMPLICIT NONE
      SAVE
*
      character*13 here
      parameter (here= 'B_clear_event')
*
      logical ABORT
      character*(*) err

      include 'bigcal_data_structures.cmn'
      include 'b_ntuple.cmn'
      include 'bigcal_gain_parms.cmn'
      include 'bigcal_filenames.cmn'
      include 'bigcal_bypass_switches.cmn'
c      include 'bigcal_statistics.cmn'
      include 'bigcal_shower_parms.cmn'
      
      integer i,j,k

      BIGCAL_TDC_NHIT = 0
      BIGCAL_TDC_NDECODED = 0
      BIGCAL_TIME_NGOOD = 0
      BIGCAL_ATRIG_NHIT = 0
      BIGCAL_ATRIG_NGOOD = 0
      BIGCAL_TTRIG_NHIT = 0
      BIGCAL_TTRIG_NDECODED = 0
      BIGCAL_TTRIG_NGOOD = 0
      BIGCAL_PROT_NHIT = 0
      BIGCAL_RCS_NHIT = 0
      BIGCAL_PROT_NGOOD = 0
      BIGCAL_RCS_NGOOD = 0
      BIGCAL_PROT_NCLSTR = 0
      BIGCAL_RCS_NCLSTR = 0
      BIGCAL_MID_NCLSTR = 0
      bigcal_phys_ntrack = 0

      bigcal_max_adc = 0.
      bigcal_iymax_adc = 0
      bigcal_ixmax_adc = 0
c      bigcal_max_adc_final = 0.
      bigcal_iymax_final = 0
      bigcal_ixmax_final = 0

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

c     we probably need to clear out the cluster arrays too!!!!

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

      nclust = 0

*     don't need to do anything with hit arrays, since they are filled dynamically
*     (other than zeroing the numbers of hits)
      call b_ntuple_clear

      ABORT=.false.
      err = ' '
      return 
      end
