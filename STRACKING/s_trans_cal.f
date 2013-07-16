      subroutine s_trans_cal(abort,errmsg)
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
*-      Input Banks: SOS_SPARSIFIED_CAL, SOS_CAL_CONST,SOS_CAL_MONITOR
*-
*-      Output Bank: SOS_DECODED_CAL
*-
*-      Created: 15 Mar 1994      Tsolak A. Amatuni
* $Log: s_trans_cal.f,v $
* Revision 1.8  2004/05/12 15:38:59  jones
* Initialize ssshsum and ssshtrk to zero.
*
* Revision 1.7  2003/04/03 00:45:01  jones
* Update to calorimeter calibration (V. Tadevosyan)
*
* Revision 1.6  1999/02/04 18:18:30  saw
* Fix calculation of energy for blocks with two tubes
*
* Revision 1.5  1999/02/03 21:13:45  saw
* Code for new Shower counter tubes
*
* Revision 1.4  1999/01/29 17:34:59  saw
* Add variables for second tubes on shower counter
*
* Revision 1.3  1995/05/22 19:46:02  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.2  1994/11/23  14:45:40  cdaq
* * (SPB) Recopied from hms file and modified names for SOS
*
* Revision 1.1  1994/02/21  16:42:44  cdaq
* Initial revision
*
*--------------------------------------------------------
      implicit none
      save
*
      logical abort
      character*(*) errmsg
      character*11 here
      parameter (here='S_TRANS_CAL')
*
      integer*4 nb      !Block number
      integer*4 nh      !Hit number
      integer*4 row     !Row number
      integer*4 col     !Column number
      real*4 adc_pos, adc_neg !ADC-PED value
*
      include 'sos_data_structures.cmn'
      include 'sos_calorimeter.cmn'
*
*      Sparsify the raw data
*
      call s_sparsify_cal(abort,errmsg)
      if(abort) then
        call g_add_path(here,errmsg)
        return
      endif
*
      snhits_cal =0
      scal_e1    =0.
      scal_e2    =0.
      scal_e3    =0.
      scal_e4    =0.
      scal_et    =0.
      ssshsum = 0.
      ssshtrk = 0.
*
      scal_e1_pos    =0.
      scal_e1_neg    =0.
*
      scal_e2_pos    =0.
      scal_e2_neg    =0.

      if(scal_num_hits.le.0) go to 100   !Return
*
*      Loop over hits
*
      do nh=1,scal_num_hits
        row=scal_rows(nh)
        col=scal_cols(nh)
        adc_pos=scal_adcs_pos(nh)
        adc_neg=scal_adcs_neg(nh)
        nb =row+smax_cal_rows*(col-1)
*
*------Determine position and energy deposition for each block
        sblock_xc(nh)=scal_block_xc(nb)
        sblock_zc(nh)=scal_block_zc(nb)
        if(col.le.scal_num_neg_columns) then ! Blocks with two tubes
          sblock_de_pos(nh)=adc_pos*scal_pos_cal_const(nb)
     $         *scal_pos_gain_cor(nb)
          sblock_de_neg(nh)=adc_neg*scal_neg_cal_const(nb)
     $         *scal_neg_gain_cor(nb)
          sblock_de(nh)=sblock_de_pos(nh)+sblock_de_neg(nh)
        else                            ! Blocks with single tube
          sblock_de(nh)=adc_pos*scal_pos_cal_const(nb)*scal_pos_gain_cor(nb)
          sblock_de_pos(nh)=sblock_de(nh)
        endif
*
*------Accumulate the integral energy depositions
        if(col.eq.1) then
          scal_e1=scal_e1+sblock_de(nh)
          if(scal_num_neg_columns.ge.1) then
            scal_e1_pos=scal_e1_pos+sblock_de_pos(nh)
            scal_e1_neg=scal_e1_neg+sblock_de_neg(nh)
          endif
        else if (col.eq.2) then
          scal_e2=scal_e2+sblock_de(nh)
          if(scal_num_neg_columns.ge.2) then
            scal_e2_pos=scal_e2_pos+sblock_de_pos(nh)
            scal_e2_neg=scal_e2_neg+sblock_de_neg(nh)
          endif
        else if(col.eq.3) then
          scal_e3=scal_e3+sblock_de(nh)
        else if(col.eq.4) then
          scal_e4=scal_e4+sblock_de(nh)
        endif
        scal_et=scal_et+sblock_de(nh)  ! Is sblock_de de_pos+de_neg?
      enddo                             !End loop over hits
      snhits_cal=scal_num_hits
*
  100 continue
      if(sdbg_decoded_cal.gt.0) call s_prt_cal_decoded
*
      return
      end
