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
      include 'bigcal_hist_id.cmn'
      include 'gep_data_structures.cmn'

      integer*4 ihit,jhit,ihitbest,icell64best
      real*4 hit_time,ph,esum,tphc,ttrig,mintdiff 
      real*4 p0,p1,p2,p3
      integer*4 irow64,icol64,icell64,ngood,thitnum
      integer*4 jrow64,jcol64,jcell64
      integer*4 irow8,icol8,icell8
      
*     find trigger logic groups with good ADC/TDC values
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
          
c$$$          if(bid_btadc(icell64).gt.0.and.b_use_peds_in_hist.eq.0) then
c$$$             call hf1(bid_btadc(icell64),
c$$$     $            bigcal_atrig_adc_good(ihit),1.0)
c$$$          endif
          bigcal_atrig_good_det(icell64) = bigcal_atrig_esum(ihit)
c$$$          if(bid_bcal_tadcvsum64.gt.0) then
c$$$             call hf2(bid_bcal_tadcvsum64,bigcal_atrig_sum64(icell64),
c$$$     $            bigcal_atrig_adc_good(ihit),1.0)
c$$$          endif

          if(bid_bcal_arow64.gt.0) call hf1(bid_bcal_arow64,float(irow64),1.)
          if(bid_bcal_acol64.gt.0) call hf1(bid_bcal_acol64,float(icol64),1.)
          if(bid_bcal_arow64vsacol64.gt.0) call hf2(bid_bcal_arow64vsacol64,
     $         float(icol64),float(irow64),1.)

          if(bigcal_iymax_adc.ne.0.and.bigcal_ixmax_adc.ne.0.and.
     $         bid_bcal_trchvmax64.gt.0) then
             jrow64 = (bigcal_iymax_adc-1)/3 + 1
             if(bigcal_iymax_adc.le.bigcal_prot_ny) then
                jcol64 = (bigcal_ixmax_adc-1)/16 + 1
             else
                jcol64 = bigcal_ixmax_adc/16 + 1
             endif

             jcell64 = jcol64 + 2*(jrow64-1)
             
             

             if(mod(bigcal_iymax_adc-1,3).eq.0) then ! overlap row
c     pick closest group between jcell64 and jcell64-2, the other group to which
c     the maximum belongs
                
                if(abs(jcell64-icell64).lt.abs(jcell64-2-icell64)) then
                   call hf2(bid_bcal_trchvmax64,float(jcell64),
     $                  float(icell64),1.0)
                else
                   call hf2(bid_bcal_trchvmax64,float(jcell64-2),
     $                  float(icell64),1.0)
                endif
             else ! not overlap, group of max is unique 
                call hf2(bid_bcal_trchvmax64,float(jcell64),
     $               float(icell64),1.0)
             endif
          endif
        enddo
      endif

      ngood = 0

      ihitbest = 0
      icell64best = 0

      mintdiff = 0.

      if(ntrigb.gt.0) then
         do ihit=1,ntrigb
            if(ihit.eq.1.or.abs(gep_btime(ihit)-gep_btime_elastic).lt.mintdiff) then
               ttrig = bigcal_end_time - gep_btime(ihit)
               mintdiff = abs(gep_btime(ihit)-gep_btime_elastic)
            endif
         enddo
      else
         ttrig = bigcal_end_time - gep_btime_elastic
      endif
      
      gep_btime_raw = ttrig

      mintdiff = 0.

      if(bigcal_ttrig_ndecoded.gt.0) then
         do ihit=1,bigcal_ttrig_ndecoded
            irow64 = bigcal_ttrig_dec_igroup(ihit)
            icol64 = bigcal_ttrig_dec_ihalf(ihit)
            icell64 = icol64 + 2*(irow64-1)
c$$$            if(bbypass_prot.ne.0.and.bbypass_rcs.ne.0.and.icell64
c$$$     $           .le.bigcal_atrig_maxhits) then
c$$$               ph = bigcal_atrig_sum64(icell64)
c$$$            else
c$$$               ph = 0.
c$$$            endif

            ph = bigcal_atrig_good_det(icell64)

            hit_time = bigcal_ttrig_tdc_dec(ihit) * bigcal_tdc_to_time ! convert to ns
            hit_time = bigcal_end_time - hit_time ! invert since we're in common stop mode.
            hit_time = hit_time - bigcal_g64_time_offset(icell64) 
c$$$            if(ntrigb.gt.0) then ! also subtract trigger time if there was a trigger: otherwise, 
c$$$c     subtract center of elastic timing window.
c$$$               hit_time = hit_time - gep_btime(1)
c$$$            else 
c$$$               hit_time = hit_time - gep_btime_elastic
c$$$            endif
c$$$            hit_time = hit_time - bigcal_g64_phc_coeff(icell64) * 
c$$$     $           sqrt(max(0.,(ph/bigcal_g64_minph(icell64)-1.)))

            if(ph.ge.bigcal_g64_phc_minph(icell64).and.ph.le.bigcal_g64_phc_maxph(icell64)
     $           ) then
               p0 = bigcal_g64_phc_p0(icell64)
               p1 = bigcal_g64_phc_p1(icell64)
               p2 = bigcal_g64_phc_p2(icell64)
               p3 = bigcal_g64_phc_p3(icell64)

               tphc = p2 + (p0 + ph*p1)*exp(-p3*ph)
            else
               tphc = 0.
            endif

            hit_time = hit_time - tphc

            if(abs(hit_time - ttrig).le.bigcal_window_slop) 
     $           then
               ngood = ngood + 1
               
               if(ngood.eq.1.or.abs(hit_time - ttrig)<mintdiff) then
                  mintdiff = abs(hit_time - ttrig)
                  ihitbest = ngood
                  icell64best = icell64
               endif

               bigcal_ttrig_good_igroup(ngood) = irow64
               bigcal_ttrig_good_ihalf(ngood) = icol64
               bigcal_ttrig_time_good(ngood) = hit_time
               bigcal_ttrig_tdc_good(ngood) = bigcal_ttrig_tdc_dec(ihit)
c     fill trig tdc histogram
               
               do jhit=1,bigcal_atrig_ngood
                  jrow64 = bigcal_atrig_good_igroup(jhit)
                  jcol64 = bigcal_atrig_good_ihalf(jhit)
                  jcell64 = jcol64 + 2*(jrow64-1)
                  if(bid_bcal_ttchanvstachan.gt.0) call hf2(bid_bcal_ttchanvstachan,
     $                 float(jcell64),float(icell64),1.)
               enddo

               if(bid_bcal_trow64.gt.0) call hf1(bid_bcal_trow64,float(irow64),1.)
               if(bid_bcal_tcol64.gt.0) call hf1(bid_bcal_tcol64,float(icol64),1.)
               if(bid_bcal_trow64vstcol64.gt.0) call hf2(bid_bcal_trow64vstcol64,
     $              float(icol64),float(irow64),1.)

               if(bid_bttdc(icell64).gt.0) call hf1(bid_bttdc(icell64),
     $              float(bigcal_ttrig_tdc_good(ngood)),1.0)
               
               if(bigcal_ttrig_det_ngood(icell64).lt.8) then
                  bigcal_ttrig_det_ngood(icell64) = 
     $                 bigcal_ttrig_det_ngood(icell64) + 1
                  thitnum = bigcal_ttrig_det_ngood(icell64)
                  bigcal_ttrig_good_det(icell64,thitnum) = hit_time
               endif
               
               if(bbypass_sum8.eq.0.and.bigcal_time_ngood.gt.0.and.
     $              bid_bcal_ttdcvtdc.gt.0) then
                  do jhit=1,bigcal_time_ngood
                     irow8 = bigcal_time_irow(jhit)
                     icol8 = bigcal_time_igroup(jhit)
                     icell8 = icol8 + 4*(irow8-1)

                     jcell64 = (icol8-1)/2 + 1 + 2*((irow8-1)/3)

                     if(bid_bcal_ttchanvstgroup.gt.0) then
                        if(mod(irow8-1,3).eq.0) then ! overlap row
                           if(abs(jcell64-icell64).lt.abs(jcell64-2-icell64)) then
                              call hf2(bid_bcal_ttchanvstgroup,float(jcell64),
     $                             float(icell64),1.)
                           else
                              call hf2(bid_bcal_ttchanvstgroup,float(jcell64-2),
     $                             float(icell64),1.)
                           endif
                        else ! not overlap row
                           call hf2(bid_bcal_ttchanvstgroup,float(jcell64),
     $                          float(icell64),1.)
                        endif
                     endif

c     check if the two hits match: 
                     if( (icol8-1)/2 + 1 .eq. icol64 ) then
                        if( (irow8-1)/3 + 1 .eq. irow64 .or.(irow8-1)/3-1
     $                       .eq. irow64) then
                           call hf2(bid_bcal_ttdcvtdc,
     $                          bigcal_hit_time(jhit),hit_time,1.0)
                        endif
                     endif
                  enddo
               endif
            endif
         enddo
      endif
      
c     walk-correct the trigger time now: 

c      write(*,*) 'icell64best=',icell64best

      if(icell64best.gt.0) then
c         write(*,*) 'adc64=',bigcal_atrig_good_det(icell64best)
c         write(*,*) 'minph,maxph=',btrig_phc_minph,btrig_phc_maxph
         if(bigcal_atrig_good_det(icell64best).ge.btrig_phc_minph.and.
     $        bigcal_atrig_good_det(icell64best).le.btrig_phc_maxph) then
            p0 = btrig_phc_p0
            p1 = btrig_phc_p1
            p2 = btrig_phc_p2
            p3 = btrig_phc_p3
            
            ph = bigcal_atrig_good_det(icell64best)
            
            tphc = p2 + (p0 + p1*ph)*exp(-p3*ph)
c            write(*,*) 'adc,phc=',bigcal_atrig_good_det(icell64best),tphc
            
            gep_btime_corr = ttrig - tphc
c            write(*,*) 'walk-corrected BigCal trigger time=',gep_btime_corr
         else 
            gep_btime_corr = ttrig
         endif
      else 
         gep_btime_corr = ttrig
      endif
            

      do icell64=1,bigcal_atrig_maxhits
         if(bigcal_ttrig_det_ngood(icell64).gt.0) then
            if(bid_btadc(icell64).gt.0.and.b_use_peds_in_hist.eq.0) then
               call hf1(bid_btadc(icell64),
     $              bigcal_atrig_good_det(icell64),1.0)
            endif
            
            if(bid_bcal_tadcvsum64.gt.0) then
               call hf2(bid_bcal_tadcvsum64,bigcal_atrig_sum64(icell64),
     $              bigcal_atrig_good_det(icell64),1.0)
            endif
         endif
      enddo

      bigcal_ttrig_ngood = ngood
      
      return 
      end
