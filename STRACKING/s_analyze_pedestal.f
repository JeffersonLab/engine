      subroutine s_analyze_pedestal(ABORT,err)
*
* $Log$
* Revision 1.7  1996/11/07 19:49:10  saw
* (WH) Add pedestal analysis for lucite cerenkov
*
* Revision 1.6  1996/04/30 17:00:53  saw
* (JRA) Change some aerogel variable names
*
* Revision 1.5  1995/10/09 20:07:55  cdaq
* (JRA) Use scer_raw_adc instead of hcer_adc
*
* Revision 1.4  1995/07/20 14:45:42  cdaq
* (???) Fix typo in Gas Cerenkov Pedestals section
*
* Revision 1.3  1995/05/22  19:45:30  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.2  1995/05/17  16:41:08  cdaq
* (JRA) Add Cernekov pedestals, cosmetic changes
*
* Revision 1.1  1995/04/01  19:35:31  cdaq
* Initial revision
*
*
      implicit none
      save
*
      character*18 here
      parameter (here='s_analyze_pedestal')
*
      logical ABORT
      character*(*) err
*
      integer*4 ihit
      integer*4 pln,cnt
      integer*4 row,col
      integer*4 blk
      integer*4 pmt
*
      INCLUDE 'sos_data_structures.cmn'
      INCLUDE 'sos_pedestals.cmn'
*
*
* HODOSCOPE PEDESTALS
*

      do ihit = 1 , sscin_all_tot_hits
        pln = sscin_all_plane_num(ihit)
        cnt = sscin_all_counter_num(ihit)
        shodo_pos_ped_sum2(pln,cnt) = shodo_pos_ped_sum2(pln,cnt) +
     &          sscin_all_adc_pos(ihit)*sscin_all_adc_pos(ihit)
        shodo_neg_ped_sum2(pln,cnt) = shodo_neg_ped_sum2(pln,cnt) +
     &          sscin_all_adc_neg(ihit)*sscin_all_adc_neg(ihit)
        shodo_pos_ped_sum(pln,cnt) = shodo_pos_ped_sum(pln,cnt) +
     &          sscin_all_adc_pos(ihit)
        shodo_neg_ped_sum(pln,cnt) = shodo_neg_ped_sum(pln,cnt) +
     &          sscin_all_adc_neg(ihit)
        shodo_pos_ped_num(pln,cnt) = shodo_pos_ped_num(pln,cnt) + 1
        shodo_neg_ped_num(pln,cnt) = shodo_neg_ped_num(pln,cnt) + 1
      enddo
*
* CALORIMETER PEDESTALS
*
      do ihit = 1 , scal_tot_hits
        row = scal_row(ihit)
        col = scal_column(ihit)
        blk = row + (col-1)*smax_cal_rows
        scal_ped_sum2(blk) = scal_ped_sum2(blk) + scal_adc(ihit)*scal_adc(ihit)
        scal_ped_sum(blk) = scal_ped_sum(blk) + scal_adc(ihit)
        scal_ped_num(blk) = scal_ped_num(blk) + 1
      enddo
*
*
* GAS CERENKOV PEDESTALS
*
      do ihit = 1 , scer_tot_hits
        pmt=scer_tube_num(ihit)       ! no sparsification yet - NEED TO FIX!!!!
        scer_ped_sum2(pmt) = scer_ped_sum2(pmt) +
     $       scer_raw_adc(ihit)*scer_raw_adc(ihit)
        scer_ped_sum(pmt) = scer_ped_sum(pmt) + scer_raw_adc(ihit)
        scer_ped_num(pmt) = scer_ped_num(pmt) + 1
      enddo
*
*
* AEROGEL CERENKOV PEDESTALS
*
      do ihit = 1 , saer_tot_hits
        blk = saer_pair_num(ihit)
        saer_pos_ped_sum2(blk) = saer_pos_ped_sum2(blk) + saer_adc_pos(ihit)*saer_adc_pos(ihit)
        saer_neg_ped_sum2(blk) = saer_neg_ped_sum2(blk) + saer_adc_neg(ihit)*saer_adc_neg(ihit)
        saer_pos_ped_sum(blk) = saer_pos_ped_sum(blk) + saer_adc_pos(ihit)
        saer_neg_ped_sum(blk) = saer_neg_ped_sum(blk) + saer_adc_neg(ihit)
        saer_pos_ped_num(blk) = saer_pos_ped_num(blk) + 1
        saer_neg_ped_num(blk) = saer_neg_ped_num(blk) + 1
      enddo
*
*
* LUCITE CERENKOV PEDESTALS
*
      do ihit = 1 , sluc_tot_hits
        blk = sluc_pair_num(ihit)
        sluc_pos_ped_sum2(blk) = sluc_pos_ped_sum2(blk) + sluc_adc_pos(ihit)*sluc_adc_pos(ihit)
        sluc_neg_ped_sum2(blk) = sluc_neg_ped_sum2(blk) + sluc_adc_neg(ihit)*sluc_adc_neg(ihit)
        sluc_pos_ped_sum(blk) = sluc_pos_ped_sum(blk) + sluc_adc_pos(ihit)
        sluc_neg_ped_sum(blk) = sluc_neg_ped_sum(blk) + sluc_adc_neg(ihit)
        sluc_pos_ped_num(blk) = sluc_pos_ped_num(blk) + 1
        sluc_neg_ped_num(blk) = sluc_neg_ped_num(blk) + 1
      enddo
 
      return
      end
