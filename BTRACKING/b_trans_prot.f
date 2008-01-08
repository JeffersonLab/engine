      subroutine b_trans_PROT(ABORT,err)

      implicit none
      save

      logical ABORT
      character*(*) err
      
      character*12 here
      parameter (here='b_trans_PROT')
      
      integer ihit,irow,icol
      integer icell,irow8,icol8,ig8
      integer igroup64,ihalf64,ig64
      real sum8

      include 'bigcal_data_structures.cmn'
      include 'bigcal_gain_parms.cmn'
      include 'bigcal_geometry.cmn'
      include 'bigcal_hist_id.cmn'
      include 'bigcal_bypass_switches.cmn'
*
*     start by sparsifying the raw data:       
*
      call b_sparsify_prot(ABORT,err)
      if(ABORT) then
         call g_add_path(here,err)
         return
      endif
      
*
*     for now, only additional step will be to convert the decoded
*     and ped-subtracted data to energies by multiplying with the 
*     calibration constants. Actually, we should also have a database of 
*     x and y positions of cell center relative to calorimeter center
*     and we can fill those arrays here too. What sums should we  
*     accumulate?
*      
*     loop over sparsified "good" hits
      if(bigcal_prot_ngood.gt.0) then
        do ihit=1,BIGCAL_PROT_NGOOD
          irow = BIGCAL_PROT_IYGOOD(ihit)
          icol = BIGCAL_PROT_IXGOOD(ihit)
          icell = icol + BIGCAL_PROT_NX*(irow - 1)
          BIGCAL_PROT_ECELL(ihit) = BIGCAL_PROT_CFAC(icell) *
     $         BIGCAL_PROT_ADC_GOOD(ihit) * BIGCAL_PROT_GAIN_COR(icell)
          
          if(bigcal_prot_adc_good(ihit).gt.bigcal_max_adc) then
            bigcal_max_adc = bigcal_prot_adc_good(ihit)
            bigcal_iymax_adc = irow
            bigcal_ixmax_adc = icol
          endif
          
          if(bid_bcal_row.gt.0) call hf1(bid_bcal_row,float(irow),1.0)
          if(bid_bcal_col.gt.0) call hf1(bid_bcal_col,float(icol),1.0)
          if(bid_bcal_rowcol.gt.0) call hf2(bid_bcal_rowcol,float(icol),float(irow),1.0)
          if(bid_badc(icell).gt.0.and.b_use_peds_in_hist.le.0) then
             if(b_use_peds_in_hist.eq.0) then
                call hf1(bid_badc(icell),bigcal_prot_adc_good(ihit),1.0)
             else
                call hf1(bid_badc(icell),bigcal_prot_adc_good(ihit)+
     $               bigcal_prot_ped_mean(icell),1.)
             endif
          endif
*     question of whether to group by hits or cells. 
*     seems most logical and efficient to go by hits only and not 
*     have to keep passing around the full array of cells with lots of 
*     empties. 
*     
*     Fill also xgood and ygood arrays
          BIGCAL_PROT_XGOOD(ihit) = BIGCAL_PROT_XCENTER(icell)
          BIGCAL_PROT_YGOOD(ihit) = BIGCAL_PROT_YCENTER(icell)

*     Fill detector arrays and increment sums of 8 and sums of 64:
          BIGCAL_PROT_GOOD_DET(icell) = BIGCAL_PROT_ECELL(ihit)
*     Also fill "all detector" array:
          bigcal_all_adc_good(ihit) = bigcal_prot_adc_good(ihit)
          bigcal_all_ecell(ihit) = bigcal_prot_ecell(ihit)
          bigcal_all_xgood(ihit) = bigcal_prot_xgood(ihit)
          bigcal_all_ygood(ihit) = bigcal_prot_ygood(ihit)
          
          bigcal_all_iygood(ihit) = irow
          bigcal_all_ixgood(ihit) = icol

          bigcal_all_adc_det(icell) = bigcal_prot_adc_good(ihit)
          bigcal_all_good_det(icell) = bigcal_prot_ecell(ihit)

c     BIGCAL_PROT_GOOD_HIT(icell) = .true.
          irow8 = irow
          icol8 = (icol - 1)/8 + 1
          ig8 = icol8 + BIGCAL_MAX_GROUPS * (irow8 - 1)
c$$$          bigcal_tdc_sum8(ig8) = bigcal_tdc_sum8(ig8) + 
c$$$     $         BIGCAL_PROT_ECELL(ihit)
          bigcal_tdc_sum8(ig8) = bigcal_tdc_sum8(ig8) + 
     $         BIGCAL_PROT_ADC_GOOD(ihit)
          igroup64 = (irow - 1) / 3 + 1
          ihalf64 = (icol - 1) / 16 + 1
          ig64 = ihalf64 + 2*(igroup64 - 1)
c$$$          bigcal_atrig_sum64(ig64) = bigcal_atrig_sum64(ig64) + 
c$$$     $         BIGCAL_PROT_ECELL(ihit)
          bigcal_atrig_sum64(ig64) = bigcal_atrig_sum64(ig64) + 
     $         BIGCAL_PROT_ADC_GOOD(ihit)
          if( mod(irow-1,3) .eq. 0 .and. irow.gt.1) then ! overlap row, also increment previous sum
c$$$            bigcal_atrig_sum64(ig64-2) = bigcal_atrig_sum64(ig64-2) + 
c$$$     $           bigcal_prot_ecell(ihit)
            bigcal_atrig_sum64(ig64-2) = bigcal_atrig_sum64(ig64-2) + 
     $           bigcal_prot_adc_good(ihit)
          endif
            
        enddo
      endif

      return
      end
      
