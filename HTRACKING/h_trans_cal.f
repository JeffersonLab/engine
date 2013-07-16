      subroutine h_trans_cal(abort,errmsg)
*=======================================================================
*-
*-      Purpose: Computes the energy deposited in each of the hit
*-               counters, the energy deposition in calorimeter
*-               columns and the total energy deposition, using only
*-               the calorimeter information.
*-               The energy depositions are not corrected yet for
*-               impact point coordinate dependence.
*-               The subroutine also returns the X and Z coordinates
*-               of the hit block centers.
*-
*-      Input Banks: HMS_SPARSIFIED_CAL, HMS_CAL_CONST,HMS_CAL_MONITOR
*-
*-      Output Bank: HMS_DECODED_CAL
*-
*-      Created: 15 Mar 1994      Tsolak A. Amatuni
* $Log: h_trans_cal.f,v $
* Revision 1.8  2004/03/03 19:26:25  jones
* Initialize  hsshsum and hsshtrk to zero.
*
* Revision 1.7  1999/02/04 18:18:14  saw
* Fix calculation of energy for blocks with two tubes
*
* Revision 1.6  1999/02/03 21:13:24  saw
* Code for new Shower counter tubes
*
* Revision 1.5  1999/01/29 17:33:57  saw
* Cosmetic changes
*
* Revision 1.4  1998/12/17 22:02:40  saw
* Support extra set of tubes on HMS shower counter
*
* Revision 1.3  1995/05/22 19:39:31  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.2  1994/04/13  17:36:40  cdaq
* (DFG) Change name of print routine
*
* Revision 1.1  1994/02/19  06:21:11  cdaq
* Initial revision
*
*--------------------------------------------------------
      implicit none
      save
*
      logical abort
      character*(*) errmsg
      character*11 here
      parameter (here='H_TRANS_CAL')
*
      integer*4 nb      !Block number
      integer*4 nh      !Hit number
      integer*4 row     !Row number
      integer*4 col     !Column number
      real*4 adc_pos, adc_neg !ADC-PED value
*
      include 'hms_data_structures.cmn'
      include 'hms_calorimeter.cmn'
*
*      Sparsify the raw data
*
      call h_sparsify_cal(abort,errmsg)
      if(abort) then
         call g_add_path(here,errmsg)
         return
      endif
*
      hnhits_cal =0
      hcal_e1    =0.
      hcal_e2    =0.
      hcal_e3    =0.
      hcal_e4    =0.
      hcal_et    =0.
      hsshsum = 0.
      hsshtrk = 0.
*
      hcal_e1_pos    =0.
      hcal_e1_neg    =0.
*
      hcal_e2_pos    =0.
      hcal_e2_neg    =0.

      if(hcal_num_hits.le.0) go to 100   !Return
*
*      Loop over hits
*
      do nh=1,hcal_num_hits
        row=hcal_rows(nh)
        col=hcal_cols(nh)
        adc_pos=hcal_adcs_pos(nh)
        adc_neg=hcal_adcs_neg(nh)
        nb =row+hmax_cal_rows*(col-1)
*
*------Determine position and energy deposition for each block
        hblock_xc(nh)=hcal_block_xc(nb)
        hblock_zc(nh)=hcal_block_zc(nb)
        if(col.le.hcal_num_neg_columns) then ! Blocks with two tubes
          hblock_de_pos(nh)=adc_pos*hcal_pos_cal_const(nb)
     $         *hcal_pos_gain_cor(nb)
          hblock_de_neg(nh)=adc_neg*hcal_neg_cal_const(nb)
     $         *hcal_neg_gain_cor(nb)
          hblock_de(nh)=hblock_de_pos(nh)+hblock_de_neg(nh)
        else                            ! Blocks with single tube
          hblock_de(nh)=adc_pos*hcal_pos_cal_const(nb)*hcal_pos_gain_cor(nb)
          hblock_de_pos(nh)=hblock_de(nh)
        endif
*
*------Accumulate the integral energy depositions
        if(col.eq.1) then
          hcal_e1=hcal_e1+hblock_de(nh)
          if(hcal_num_neg_columns.ge.1) then
            hcal_e1_pos=hcal_e1_pos+hblock_de_pos(nh)
            hcal_e1_neg=hcal_e1_neg+hblock_de_neg(nh)
          endif
        else if (col.eq.2) then
          hcal_e2=hcal_e2+hblock_de(nh)
          if(hcal_num_neg_columns.ge.2) then
            hcal_e2_pos=hcal_e2_pos+hblock_de_pos(nh)
            hcal_e2_neg=hcal_e2_neg+hblock_de_neg(nh)
          endif
        else if(col.eq.3) then
          hcal_e3=hcal_e3+hblock_de(nh)
        else if(col.eq.4) then
          hcal_e4=hcal_e4+hblock_de(nh)
        endif
        hcal_et=hcal_et+hblock_de(nh)  ! Is hblock_de de_pos+de_neg?
*
      enddo      !End loop over hits
      hnhits_cal=hcal_num_hits
*
  100 continue
      if(hdbg_decoded_cal.gt.0) call h_prt_cal_decoded
*
      return
      end
