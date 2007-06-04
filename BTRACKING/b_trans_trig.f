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
      include 'bigcal_bypass_switches.cmn'

      integer*4 ihit
      real*4 hit_time,ph,esum      
      integer*4 irow64,icol64,icell64,ngood,thitnum
      

*     find trigger logic groups with good TDC values
      call b_strip_trig(ABORT,err)
      if(ABORT) then
         call g_add_path(here,err)
         return
      endif

      if(bigcal_atrig_ngood.gt.0)then
        do ihit=1,bigcal_atrig_ngood
          irow64 = bigcal_atrig_good_igroup(ihit)
          icol64 = bigcal_atrig_good_ihalf(ihit)
          icell64 = icol64 + 2*(irow64 - 1)
          bigcal_atrig_esum(ihit) = bigcal_trig_cfac(icell64) *
     $         bigcal_atrig_adc_good(ihit)*bigcal_trig_gain_cor(icell64)

          bigcal_atrig_good_det(icell64) = bigcal_atrig_esum(ihit)
        enddo
      endif

      ngood = 0

      if(bigcal_ttrig_ndecoded.gt.0) then
        do ihit=1,bigcal_ttrig_ndecoded
          irow64 = bigcal_ttrig_dec_igroup(ihit)
          icol64 = bigcal_ttrig_dec_ihalf(ihit)
          icell64 = icol64 + 2*(irow64-1)
          if(bbypass_prot.ne.0.and.bbypass_rcs.ne.0.and.icell64
     $         .le.bigcal_atrig_maxhits) then
            ph = bigcal_atrig_sum64(icell64)
          else
            ph = 0.
          endif
          hit_time = bigcal_ttrig_tdc_dec(ihit) * bigcal_tdc_to_time
          hit_time = hit_time - bigcal_g64_phc_coeff(icell64) * 
     $         sqrt(max(0.,(ph/bigcal_g64_minph(icell64)-1.)))
          hit_time = hit_time - bigcal_g64_time_offset(icell64)
          if(abs(hit_time - bigcal_window_center).le.bigcal_window_slop) 
     $         then
            ngood = ngood + 1
            bigcal_ttrig_good_igroup(ngood) = irow64
            bigcal_ttrig_good_ihalf(ngood) = icol64
            bigcal_ttrig_time_good(ngood) = hit_time
            bigcal_ttrig_tdc_good(ngood) = bigcal_ttrig_tdc_dec(ihit)
            
            if(bigcal_ttrig_det_ngood(icell64).lt.8) then
              bigcal_ttrig_det_ngood(icell64) = 
     $             bigcal_ttrig_det_ngood(icell64) + 1
              thitnum = bigcal_ttrig_det_ngood(icell64)
              bigcal_ttrig_good_det(icell64,thitnum) = hit_time
            endif

          endif
        enddo
      endif

      bigcal_ttrig_ngood = ngood
      
      return 
      end
