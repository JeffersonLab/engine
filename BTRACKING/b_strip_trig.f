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

      integer*4 ihit,igroup64,ihalf64,icell64,ngood,adc_raw,tdc_raw
      integer*4 thitnum
      
      ngood = 0
      
*     do atrig first:
      do icell64=1,bigcal_atrig_maxhits
        bigcal_atrig_adc_dec(icell64) = -100.
      enddo

      if(bigcal_atrig_nhit.gt.0.and.bigcal_atrig_nhit.le.
     $     bigcal_atrig_maxhits) then
        do ihit=1,bigcal_atrig_nhit
          igroup64 = bigcal_atrig_igroup(ihit)
          ihalf64 = bigcal_atrig_ihalf(ihit)
          icell64 = ihalf64 + 2*(igroup64 - 1)
          adc_raw = bigcal_atrig_adc_raw(ihit)
          bigcal_atrig_raw_det(icell64) = adc_raw

          if(adc_raw.ge.0) then
            bigcal_atrig_adc_dec(icell64) = float(adc_raw) - 
     $           bigcal_trig_ped_mean(icell64)
          endif
          
          if(bigcal_atrig_adc_dec(icell64) .gt. 
     $         bigcal_trig_adc_threshold(icell64)) then
            ngood = ngood + 1
            bigcal_atrig_adc_good(ngood) = bigcal_atrig_adc_dec(icell64)
            bigcal_atrig_good_igroup(ngood) = igroup64
            bigcal_atrig_good_ihalf(ngood) = ihalf64
          endif
        enddo
      endif
      
      bigcal_atrig_ngood = ngood

      ngood = 0 ! now do tdcs:

      if(bigcal_ttrig_nhit.gt.0.and.bigcal_ttrig_nhit.le.
     $     bigcal_ttrig_maxhits) then
        do ihit=1,bigcal_ttrig_nhit
          igroup64 = bigcal_ttrig_igroup(ihit)
          ihalf64 = bigcal_ttrig_ihalf(ihit)
          icell64 = ihalf64 + 2*(igroup64-1)
          tdc_raw = bigcal_ttrig_tdc_raw(ihit)
          
          if(bigcal_ttrig_det_nhit(icell64).lt.8) then
            bigcal_ttrig_det_nhit(icell64)=
     $           bigcal_ttrig_det_nhit(icell64) + 1
            
            thitnum = bigcal_ttrig_det_nhit(icell64)
            bigcal_ttrig_raw_det(icell64,thitnum) = tdc_raw
          endif
          if(tdc_raw.ge.bigcal_tdc_min.and.tdc_raw.le.bigcal_tdc_max) 
     $         then
            ngood = ngood + 1
            bigcal_ttrig_tdc_dec(ngood) = tdc_raw
            bigcal_ttrig_dec_igroup(ngood) = igroup64
            bigcal_ttrig_dec_ihalf(ngood) = ihalf64
          endif
        enddo
      endif
      
      bigcal_ttrig_ndecoded = ngood

      return 
      end
     
