      subroutine b_strip_trig(ABORT,err)

      implicit none
      save
      
      logical ABORT
      character*(*) err
      
      character*12 here
      parameter (here='b_strip_trig')

      include 'bigcal_data_structures.cmn'
      include 'bigcal_tof_parms.cmn'
      include 'bigcal_gain_parms.cmn'

      integer*4 ihit,igroup,ngood,ihalf,ilogic
      logical firsthit(BIGCAL_LOGIC_GROUPS)
      
      ngood = 0

      do ilogic=1,BIGCAL_LOGIC_GROUPS
         firsthit(ilogic) = .true.
      enddo

*     'zero' 'decoded' hit arrays:
      do ihit=1,BIGCAL_TRIG_MAXHITS
         BIGCAL_TRIG_TDC_DECODED(ihit) = -1
         BIGCAL_TRIG_ADC_DECODED(ihit) = -100.
         BIGCAL_TRIG_ADC_GOOD(ihit) = -100.
         BIGCAL_TRIG_TIME_GOOD(ihit) = -1000. ! depending on distance, e- tof will be anywhere from 
                                              ! 15 ns to 67 ns
      enddo
*     check number of hits, must fall in allowed range:
      if(BIGCAL_TRIG_NHIT.lt.0.or.BIGCAL_TRIG_NHIT.gt.
     $     BIGCAL_TRIG_MAXHITS) then 
         write(6,*) here,':bigcal_trig_nhit=',BIGCAL_TRIG_NHIT
         return
      endif
*     good hit has good tdc value:
      do ihit=1,BIGCAL_TRIG_NHIT
         igroup = BIGCAL_TRIG_IGROUP(ihit)
         ihalf = BIGCAL_TRIG_IHALF(ihit)
         ilogic = igroup + BIGCAL_LOGIC_GROUPS/2 *(ihalf - 1) ! 1 = left, 
                                ! 2 = right,
         if(BIGCAL_TRIG_TDC_RAW(ihit).gt.BIGCAL_TRIG_TDC_DET(ilogic)
     $        .or.firsthit(ilogic)) then
            firsthit(ilogic) = .false.
            BIGCAL_TRIG_TDC_DET(ilogic) = BIGCAL_TRIG_TDC_RAW(ihit)
            BIGCAL_TRIG_ADC_DET(ilogic) = BIGCAL_TRIG_ADC_RAW(ihit)
         endif
         if( BIGCAL_TRIG_TDC_RAW(ihit).ge.bigcal_tdc_min.and.
     $        BIGCAL_TRIG_TDC_RAW(ihit).le.bigcal_tdc_max) then
            
            ngood = ngood + 1
            ! subtract ADC pedestal
            BIGCAL_TRIG_ADC_DECODED(ngood) = 
     $           float(BIGCAL_TRIG_ADC_RAW(ihit)) - 
     $           BIGCAL_TRIG_PED_MEAN(ilogic)
            BIGCAL_TRIG_TDC_DECODED(ngood) = 
     $           BIGCAL_TRIG_TDC_RAW(ihit)
            BIGCAL_TRIG_DEC_IGROUP(ngood) = igroup
            BIGCAL_TRIG_DEC_IHALF(ngood) = ihalf
         endif
      enddo

      BIGCAL_TRIG_NDECODED = ngood

      return 
      end
     
