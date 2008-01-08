      subroutine b_trans_RCS(ABORT,err)

      implicit none
      save
      
      logical ABORT
      character*(*) err
      
      character*12 here
      parameter (here='b_trans_RCS')
      
      integer ihit,irow,icol,irow8,icol8,ig8
      integer icell,igroup64,ihalf64,ig64

      include 'bigcal_data_structures.cmn'
      include 'bigcal_gain_parms.cmn'
      include 'bigcal_geometry.cmn'
      include 'bigcal_hist_id.cmn'
      include 'bigcal_bypass_switches.cmn'
*
*     start by sparsifying the raw data:       
*
      call b_sparsify_rcs(ABORT,err)
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
      if(bigcal_rcs_ngood.gt.0) then
        do ihit=1,BIGCAL_RCS_NGOOD
          irow = BIGCAL_RCS_IYGOOD(ihit)
          icol = BIGCAL_RCS_IXGOOD(ihit)
          icell = icol + BIGCAL_RCS_NX*(irow - 1)
          BIGCAL_RCS_ECELL(ihit) = BIGCAL_RCS_CFAC(icell) *
     $         BIGCAL_RCS_ADC_GOOD(ihit) * BIGCAL_RCS_GAIN_COR(icell)
          
          if(bigcal_rcs_adc_good(ihit).gt.bigcal_max_adc) then
            bigcal_max_adc = bigcal_rcs_adc_good(ihit)
            bigcal_iymax_adc = irow + bigcal_prot_ny
            bigcal_ixmax_adc = icol
          endif
          
          if(bid_bcal_row.gt.0) call hf1(bid_bcal_row,float(irow+bigcal_prot_ny),1.0)
          if(bid_bcal_col.gt.0) call hf1(bid_bcal_col,float(icol),1.0)
          if(bid_bcal_rowcol.gt.0) call hf2(bid_bcal_rowcol,float(icol),float(irow+
     $         bigcal_prot_ny),1.0)
          if(bid_badc(icell+bigcal_prot_maxhits).gt.0.and.b_use_peds_in_hist
     $         .le.0) then
             if(b_use_peds_in_hist.eq.0) then
                call hf1(bid_badc(icell+bigcal_prot_maxhits),
     $               bigcal_rcs_adc_good(ihit),1.0)
             else
                call hf1(bid_badc(icell+bigcal_prot_maxhits),
     $               bigcal_rcs_adc_good(ihit)+bigcal_rcs_ped_mean(icell),1.)
             endif
          endif
*     question of whether to group by hits or cells. 
*     seems most logical and efficient to go by hits only and not 
*     have to keep passing around the full array of cells with lots of 
*     empties. 
*     
*     Fill also xgood and ygood arrays
          BIGCAL_RCS_XGOOD(ihit) = BIGCAL_RCS_XCENTER(icell)
          BIGCAL_RCS_YGOOD(ihit) = BIGCAL_RCS_YCENTER(icell)
*     Fill detector array and increment sums of 8 and sums of 64
          BIGCAL_RCS_GOOD_DET(icell) = BIGCAL_RCS_ECELL(ihit)
*     Fill "all detector" array
          bigcal_all_adc_good(ihit+bigcal_prot_ngood) = bigcal_rcs_adc_good(ihit)
          bigcal_all_ecell(ihit+bigcal_prot_ngood) = bigcal_rcs_ecell(ihit)
          bigcal_all_xgood(ihit+bigcal_prot_ngood) = bigcal_rcs_xgood(ihit)
          bigcal_all_ygood(ihit+bigcal_prot_ngood) = bigcal_rcs_ygood(ihit)
          
          bigcal_all_iygood(ihit+bigcal_prot_ngood) = irow+bigcal_prot_ny
          bigcal_all_ixgood(ihit+bigcal_prot_ngood) = icol
          
          bigcal_all_adc_det(icell+bigcal_prot_maxhits) = bigcal_rcs_adc_good(ihit)
          bigcal_all_good_det(icell+bigcal_prot_maxhits) = bigcal_rcs_ecell(ihit)

c     BIGCAL_RCS_GOOD_HIT(icell) = .true.
          irow8 = irow + BIGCAL_PROT_NY
          if(icol.lt.16) then 
            icol8 = (icol - 1)/8 + 1
          else 
            icol8 = icol / 8 + 1
          endif
          ig8 = icol8 + (irow8 - 1)*BIGCAL_MAX_GROUPS
          
          igroup64 = (irow8 - 1) / 3 + 1
          ihalf64 = icol / 16 + 1
          ig64 = ihalf64 + 2*(igroup64-1)
          
c$$$          bigcal_tdc_sum8(ig8) = bigcal_tdc_sum8(ig8) + 
c$$$     $         BIGCAL_RCS_ECELL(ihit)
c$$$          bigcal_atrig_sum64(ig64) = bigcal_atrig_sum64(ig64) + 
c$$$     $         BIGCAL_RCS_ECELL(ihit)
c$$$          if( mod(irow8-1,3) .eq.0)then ! overlap row, also increment previous group
c$$$            bigcal_atrig_sum64(ig64-2) = bigcal_atrig_sum64(ig64-2) +
c$$$     $           bigcal_rcs_ecell(ihit)
c$$$          endif
          
          bigcal_tdc_sum8(ig8) = bigcal_tdc_sum8(ig8) + 
     $         BIGCAL_RCS_ADC_GOOD(ihit)
          bigcal_atrig_sum64(ig64) = bigcal_atrig_sum64(ig64) + 
     $         BIGCAL_RCS_ADC_GOOD(ihit)
          if( mod(irow8-1,3) .eq.0)then ! overlap row, also increment previous group
            bigcal_atrig_sum64(ig64-2) = bigcal_atrig_sum64(ig64-2) +
     $           bigcal_rcs_adc_good(ihit)
          endif

        enddo
      endif

      return
      end
      
