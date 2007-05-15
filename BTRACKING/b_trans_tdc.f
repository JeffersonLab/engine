      subroutine b_trans_tdc(ABORT,err)

      implicit none
      save
      
      logical ABORT
      character*(*) err
      
      character*11 here
      parameter (here='b_trans_tdc')

      include 'bigcal_data_structures.cmn'
      include 'bigcal_bypass_switches.cmn'
      include 'bigcal_tof_parms.cmn'

      integer*4 ihit,igroup,irow,itdc,ngood
      real*4 thit ! hit time
      real*4 ph
      logical firsthit(BIGCAL_MAX_TDC)

      call b_strip_tdc(ABORT,err)
      if(ABORT) then
         call g_add_path(here,err)
         return
      endif

      if(BIGCAL_TDC_NDECODED .le. 0) return
      
      ngood = 0
      do itdc=1,BIGCAL_MAX_TDC
         firsthit(itdc) = .true.
      enddo

      ! at this point, use looser cut: "bigcal_window"
      ! do pulse height correction only if we have decoded the ADCs:
      do ihit=1,BIGCAL_TDC_NDECODED
         irow = BIGCAL_TDC_IROW(ihit)
         igroup = BIGCAL_TDC_IGROUP(ihit)
         itdc = igroup + (irow - 1) * BIGCAL_MAX_GROUPS
         if(bbypass_prot.ne.0.and.irow.le.BIGCAL_PROT_NY) then
            ph = 0.
         else if(bbypass_rcs.ne.0.and.irow.gt.BIGCAL_PROT_NY) then
            ph = 0.
         else
            ph = BIGCAL_TIME_ADC_SUM(itdc)
         endif
         thit = BIGCAL_TDC(ihit) * bigcal_tdc_to_time
         thit = thit - bigcal_g8_phc_coeff(itdc) * 
     $        sqrt(max(0.,(ph/bigcal_g8_minph(itdc)-1.)))
         thit = thit - bigcal_g8_time_offset(itdc)
         if(abs(thit - bigcal_window_center).le.bigcal_window_slop)then
            ngood = ngood + 1
            BIGCAL_TIME_IROW(ngood) = irow
            BIGCAL_TIME_IGROUP(ngood) = igroup
            BIGCAL_HIT_TIME(ngood) = thit
            ! also fill "detector" array (arrays over tdc# as opposed to hits)
            if(firsthit(itdc) .or.thit.lt.BIGCAL_TIME_DET(itdc))then
               ! now we are using the earliest hit for the detector array,
               ! assuming we have correctly converted TDC to time
               BIGCAL_TIME_DET(itdc) = thit
               BIGCAL_TIME_GOOD_HIT(itdc) = .true.
               BIGCAL_TIME_DET_NHIT(itdc)=BIGCAL_TIME_DET_NHIT(itdc)+1
               firsthit(itdc) = .false.
            endif
         endif
      enddo
      
      BIGCAL_TIME_NGOOD = ngood

      return 
      end
