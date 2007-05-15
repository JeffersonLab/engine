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
      BIGCAL_TRIG_NHIT = 0
      BIGCAL_TRIG_NGOOD = 0
      BIGCAL_TRIG_NDECODED = 0
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
      bigcal_max_adc_final = 0.
      bigcal_iymax_final = 0
      bigcal_ixmax_final = 0

*     zero "detector" arrays:
      
      do i=1,BIGCAL_PROT_MAXHITS
         BIGCAL_PROT_RAW_DET(i) = 0
         BIGCAL_PROT_GOOD_DET(i) = 0.
         BIGCAL_PROT_GOOD_HIT(i) = .false.
      enddo

      do i=1,BIGCAL_RCS_MAXHITS
         BIGCAL_RCS_RAW_DET(i) = 0
         BIGCAL_RCS_GOOD_DET(i) = 0.
         BIGCAL_RCS_GOOD_HIT(i) = .false.
      enddo

      do i=1,BIGCAL_MAX_TDC
         BIGCAL_TDC_DET(i) = 0
         BIGCAL_TIME_DET(i) = 0.
         BIGCAL_TIME_ADC_SUM(i) = 0.
         BIGCAL_TIME_GOOD_HIT(i) = .false.
         BIGCAL_TIME_DET_NHIT(i) = 0
      enddo

      do i=1,BIGCAL_LOGIC_GROUPS
         BIGCAL_TRIG_TDC_DET(i) = 0
         BIGCAL_TRIG_TIME_DET(i) = 0.
         BIGCAL_TRIG_TIME_NHIT(i) = 0
         BIGCAL_TRIG_ADC_DET(i) = 0
         BIGCAL_TRIG_GOOD_DET(i) = 0.
         BIGCAL_TRIG_GOOD_HIT(i) = .false.
         BIGCAL_TRIG_ADC_SUM(i) = 0.
      enddo

*     don't need to do anything with hit arrays, since they are filled dynamically
*     (other than zeroing the numbers of hits)
      call b_ntuple_clear

      ABORT=.false.
      err = ' '
      return 
      end
