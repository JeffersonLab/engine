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

      integer*4 ihit,igroup,irow,itdc,ngood,thitnum
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
          ph = BIGCAL_TDC_SUM8(itdc)
        endif
        thit = BIGCAL_TDC(ihit) * bigcal_tdc_to_time
        thit = thit - bigcal_g8_time_offset(itdc)
        thit = thit - bigcal_g8_phc_coeff(itdc) * 
     $       sqrt(max(0.,(ph/bigcal_g8_minph(itdc)-1.)))
        if(abs(thit - bigcal_window_center).le.bigcal_window_slop)then
          ngood = ngood + 1
          BIGCAL_TIME_IROW(ngood) = irow
          BIGCAL_TIME_IGROUP(ngood) = igroup
          BIGCAL_HIT_TIME(ngood) = thit
          bigcal_tdc_good(ngood) = bigcal_tdc(ihit)
c     ! also fill "detector" array (arrays over tdc# as opposed to hits)
          if(bigcal_tdc_det_ngood(itdc).lt.8)then
            bigcal_tdc_det_ngood(itdc)=bigcal_tdc_det_ngood(itdc) + 1
            thitnum = bigcal_tdc_det_ngood(itdc)
            bigcal_tdc_good_det(itdc,thitnum) = thit
          endif
        endif
      enddo
      
      BIGCAL_TIME_NGOOD = ngood

      return 
      end
