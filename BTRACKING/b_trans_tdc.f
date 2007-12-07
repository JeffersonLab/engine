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
      include 'bigcal_hist_id.cmn'
      include 'gep_data_structures.cmn'

      integer*4 ihit,igroup,irow,itdc,ngood,thitnum
      real*4 thit,tphc,ttrig ! hit time
      real*4 ph
      real*4 p0,p1,p2,p3
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
        thit = BIGCAL_TDC(ihit) * bigcal_tdc_to_time ! convert to ns 
        thit = bigcal_end_time - thit ! invert since we are in common-stop mode:
        thit = thit - bigcal_g8_time_offset(itdc) 
        
c$$$        if(ntrigb.gt.0) then ! also subtract trigger time if there was a trigger
c$$$           thit = thit - gep_btime(1)
c$$$        else 
c$$$           thit = thit - gep_btime_elastic
c$$$        endif

        if(ph.ge.bigcal_g8_phc_minph(itdc).and.ph.le.bigcal_g8_phc_maxph(itdc)) then
           p0 = bigcal_g8_phc_p0(itdc)
           p1 = bigcal_g8_phc_p1(itdc)
           p2 = bigcal_g8_phc_p2(itdc)
           p3 = bigcal_g8_phc_p3(itdc)

           tphc = p2 + (p0 + p1*ph)*exp(-p3*ph)
        else
           tphc = 0.
        endif
        
        thit = thit - tphc
        
        if(ntrigb.gt.0) then
           ttrig = bigcal_end_time - gep_btime(1)
        else
           ttrig = bigcal_end_time - gep_btime_elastic
        endif

        if(abs(thit - ttrig).le.bigcal_window_slop)then
          ngood = ngood + 1
          BIGCAL_TIME_IROW(ngood) = irow
          BIGCAL_TIME_IGROUP(ngood) = igroup
          BIGCAL_HIT_TIME(ngood) = thit
          bigcal_tdc_good(ngood) = bigcal_tdc(ihit)
c     fill tdc histogram:
          if(bid_btdc(itdc).gt.0) call hf1(bid_btdc(itdc),float(bigcal_tdc(ihit)),1.0)
          
          if(bid_bcal_row8.gt.0) call hf1(bid_bcal_row8,float(irow),1.)
          if(bid_bcal_col8.gt.0) call hf1(bid_bcal_col8,float(igroup),1.)
          if(bid_bcal_row8vscol8.gt.0) call hf2(bid_bcal_row8vscol8,float(igroup),
     $         float(irow),1.)

c          if(bid_btimewalk(itdc).gt.0) call hf2(bid_btimewalk(itdc),ph,thit,1.0)
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
