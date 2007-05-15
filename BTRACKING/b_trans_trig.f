      subroutine b_trans_trig(ABORT,err)
      
      implicit none
      save
      
      logical ABORT
      character*(*) err
      
      character*12 here
      parameter (here='b_trans_trig')

      include 'bigcal_data_structures.cmn'
      include 'bigcal_tof_parms.cmn'
      include 'bigcal_gain_parms.cmn'

      integer*4 ihit
      real*4 hit_time,ph,esum      
      integer*4 igroup,ihalf,ilogic,ngood
      logical firsthit(BIGCAL_LOGIC_GROUPS)

      ngood = 0
      do ilogic=1,BIGCAL_LOGIC_GROUPS
         firsthit(ilogic) = .true.
      enddo

*     find trigger logic groups with good TDC values
      call b_strip_trig(ABORT,err)
      if(ABORT) then
         call g_add_path(here,err)
         return
      endif
*     if no valid hits, return
      if(BIGCAL_TRIG_NDECODED .le. 0) return

*     here we want ADC above threshold and TDC within limits set by loose "window" 
*     timing cut
      do ihit=1,BIGCAL_TRIG_NDECODED
         igroup = BIGCAL_TRIG_DEC_IGROUP(ihit)
         ihalf = BIGCAL_TRIG_DEC_IHALF(ihit)
         ilogic = igroup + BIGCAL_LOGIC_GROUPS/2 * (ihalf-1)
         if(BIGCAL_TRIG_ADC_DECODED(ihit) .gt. 
     $        BIGCAL_TRIG_ADC_THRESHOLD(ilogic) .and. bigcal_tdc_min.le. 
     $        BIGCAL_TRIG_TDC_DECODED(ihit) .and. 
     $        BIGCAL_TRIG_TDC_DECODED(ihit) .le. bigcal_tdc_max ) then
            ! hit is above threshold AND tdc is in range

            ph = BIGCAL_TRIG_ADC_DECODED(ihit) * 
     $           bigcal_trig_cfac(ilogic) * bigcal_trig_gain_cor(ilogic)
            !esum = BIGCAL_TRIG_ADC_SUM(ilogic)

            hit_time = BIGCAL_TRIG_TDC_DECODED(ihit)*bigcal_tdc_to_time
            hit_time = hit_time - bigcal_g64_phc_coeff(ilogic) * 
     $           sqrt(max(0.,(ph/bigcal_g64_minph(ilogic)-1.)))
            hit_time = hit_time - bigcal_g64_time_offset(ilogic)
            if(abs(hit_time-bigcal_window_center).le.bigcal_window_slop)
     $           then
               ngood = ngood + 1
               BIGCAL_TRIG_GOOD_IGROUP(ngood) = igroup
               BIGCAL_TRIG_GOOD_IHALF(ngood) = ihalf
               BIGCAL_TRIG_TIME_GOOD(ngood) = hit_time
               BIGCAL_TRIG_ADC_GOOD(ngood) = ph
               
               ! fill "detector" array (array over group number as opposed to hits)
               if(firsthit(ilogic) .or. hit_time.lt.
     $              BIGCAL_TRIG_TIME_DET(ilogic)) then
                  BIGCAL_TRIG_TIME_DET(ilogic) = hit_time
                  BIGCAL_TRIG_TIME_NHIT(ilogic) = 
     $                 BIGCAL_TRIG_TIME_NHIT(ilogic) + 1
                  BIGCAL_TRIG_GOOD_DET(ilogic) = ph
                  BIGCAL_TRIG_GOOD_HIT(ilogic) = .true.
                  firsthit(ilogic) = .false.
               endif
            endif
         endif
      enddo
      
      BIGCAL_TRIG_NGOOD = ngood
      
      return 
      end
