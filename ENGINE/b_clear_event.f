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

      bigcal_annoying_pulser_event = .false.
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
      bigcal_all_ngood = 0
c$$$      bigcal_all_nclstr = 0
c$$$      bigcal_phys_ntrack = 0

      bigcal_prot_nbad = 0
      bigcal_rcs_nbad = 0
      bigcal_atrig_nbad = 0

      bigcal_max_adc = 0.
      bigcal_iymax_adc = 0
      bigcal_ixmax_adc = 0

*     zero "detector" arrays:
      
      do i=1,BIGCAL_PROT_MAXHITS
         BIGCAL_PROT_RAW_DET(i) = 0
         BIGCAL_PROT_GOOD_DET(i) = 0.
c         BIGCAL_PROT_GOOD_HIT(i) = .false.
         bigcal_all_adc_det(i) = 0.
         bigcal_all_good_det(i) = 0.

         bigcal_prot_nhit_ch(i) = 0
      enddo

      do i=1,BIGCAL_RCS_MAXHITS
         BIGCAL_RCS_RAW_DET(i) = 0
         BIGCAL_RCS_GOOD_DET(i) = 0.
c         BIGCAL_RCS_GOOD_HIT(i) = .false.
         bigcal_all_adc_det(i+bigcal_prot_maxhits)=0.
         bigcal_all_good_det(i+bigcal_prot_maxhits) = 0.
         
         bigcal_rcs_nhit_ch(i) = 0
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

        bigcal_atrig_nhit_ch(i) = 0
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

c     we probably need to clear out the cluster arrays too!!!!

      bigcal_all_nclstr = 0
      bigcal_phys_ntrack = 0
      bigcal_nmaxima = 0
      
c$$$      bigcal_best_thetarad = 0.
c$$$      bigcal_best_thetadeg = 0.
c$$$      bigcal_best_phirad = 0.
c$$$      bigcal_best_phideg = 0.
c$$$      bigcal_best_energy = 0.
c$$$      bigcal_best_time = 0.
c$$$      bigcal_best_xface = 0.
c$$$      bigcal_best_yface = 0.
c$$$      bigcal_best_zface = 0.
c$$$      bigcal_best_px = 0.
c$$$      bigcal_best_py = 0.
c$$$      bigcal_best_pz = 0.
c$$$      bigcal_best_beta = 0.
c$$$      bigcal_best_tof = 0.
c$$$      bigcal_best_coin_time = 0.
c$$$
c$$$      do i=1,bigcal_max_ntrack
c$$$        bigcal_track_thetarad(i) = 0.
c$$$        bigcal_track_thetadeg(i) = 0.
c$$$        bigcal_track_phirad(i) = 0.
c$$$        bigcal_track_phideg(i) = 0.
c$$$        bigcal_track_energy(i) = 0.
c$$$        bigcal_track_time(i) = 0.
c$$$        bigcal_track_xface(i) = 0.
c$$$        bigcal_track_yface(i) = 0.
c$$$        bigcal_track_zface(i) = 0.
c$$$        bigcal_track_px(i) = 0.
c$$$        bigcal_track_py(i) = 0.
c$$$        bigcal_track_pz(i) = 0.
c$$$        bigcal_track_beta(i) = 0.
c$$$        bigcal_track_tof(i) = 0.
c$$$        bigcal_track_coin_time(i) = 0.
c$$$      enddo

      nclust = 0
      nclust8 = 0
      nclust64=0
      ntrack = 0
      ibest=0
      nmax = 0
      ntrk_g = 0

      bigcal_itrack_best = 0

*     don't need to do anything with hit arrays, since they are filled dynamically
*     (other than zeroing the numbers of hits)
      call b_ntuple_clear

      ABORT=.false.
      err = ' '
      return 
      end
