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


*     don't need to do anything with hit arrays, since they are filled dynamically
*     (other than zeroing the numbers of hits)
      call b_ntuple_clear

      ABORT=.false.
      err = ' '
      return 
      end
