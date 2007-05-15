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
      integer*4 irow,igroup,itdc
      integer*4 ngood,igood
      logical firsthit(BIGCAL_MAX_TDC)

      do itdc=1,BIGCAL_MAX_TDC
         firsthit(itdc) = .true.
      enddo

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

      do ihit=1,BIGCAL_TDC_NHIT
         irow = BIGCAL_TDC_RAW_IROW(ihit)
         igroup = BIGCAL_TDC_RAW_IGROUP(ihit)
         itdc = igroup + (irow - 1) * BIGCAL_MAX_GROUPS 
         if(firsthit(itdc).or.BIGCAL_TDC_RAW(ihit).gt.
     $        BIGCAL_TDC_DET(itdc)) then
            BIGCAL_TDC_DET(itdc) = BIGCAL_TDC_RAW(ihit)
            firsthit(itdc) = .false.
            ! common stop means longer time/larger TDC = earlier hit
         endif
         if(BIGCAL_TDC_RAW(ihit) .ge. bigcal_tdc_min .and. 
     $        BIGCAL_TDC_RAW(ihit) .le. bigcal_tdc_max ) then
            ngood = ngood + 1
            BIGCAL_TDC(ngood) = BIGCAL_TDC_RAW(ihit)
            BIGCAL_TDC_IROW(ngood) = irow
            BIGCAL_TDC_IGROUP(ngood) = igroup
         endif
      enddo
      
      BIGCAL_TDC_NDECODED = ngood
      
      return 
      end
