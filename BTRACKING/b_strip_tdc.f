      subroutine b_strip_tdc(ABORT,err)

      implicit none
      save

      logical ABORT
      character*(*) err
      
      character*11 here
      parameter (here='b_strip_tdc')

      include 'bigcal_data_structures.cmn'
      include 'bigcal_tof_parms.cmn'

*     want to calculate time relative to reference time (master trigger)
      
      integer*4 ihit
      integer*4 irow,igroup,itdc,tdc_raw,thitnum
      integer*4 ngood,igood

      do ihit=1,BIGCAL_TDC_MAXHITS
         BIGCAL_TDC(ihit) = -1
         BIGCAL_HIT_TIME(ihit) = -1000.
      enddo
      
c      BIGCAL_GOOD_TRIG = .false.

      if(BIGCAL_TDC_NHIT.lt.0.or.BIGCAL_TDC_NHIT.gt.BIGCAL_TDC_MAXHITS) 
     $     then
         write(6,*) here,':bigcal_tdc_nhit=',BIGCAL_TDC_NHIT
         return
      endif

      ngood = 0

      if(bigcal_tdc_nhit.gt.0) then
        do ihit=1,BIGCAL_TDC_NHIT
          irow = BIGCAL_TDC_RAW_IROW(ihit)
          igroup = BIGCAL_TDC_RAW_IGROUP(ihit)
          itdc = igroup + (irow - 1) * BIGCAL_MAX_GROUPS 
          tdc_raw = bigcal_tdc_raw(ihit)
          
          if(bigcal_tdc_det_nhit(itdc).lt.8) then
            bigcal_tdc_det_nhit(itdc)=bigcal_tdc_det_nhit(itdc) + 1
            thitnum = bigcal_tdc_det_nhit(itdc)
            bigcal_tdc_raw_det(itdc,thitnum) = tdc_raw
          endif

          if(tdc_raw .ge. bigcal_tdc_min .and. 
     $         tdc_raw .le. bigcal_tdc_max ) then
            ngood = ngood + 1
            BIGCAL_TDC(ngood) = BIGCAL_TDC_RAW(ihit)
            BIGCAL_TDC_IROW(ngood) = irow
            BIGCAL_TDC_IGROUP(ngood) = igroup
          endif
        enddo
      endif
      
      BIGCAL_TDC_NDECODED = ngood
      
      return 
      end
