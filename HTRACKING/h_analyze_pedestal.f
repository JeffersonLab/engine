      subroutine h_analyze_pedestal(ABORT,err)
*
* $Log$
* Revision 1.4  1995/10/09 20:07:35  cdaq
* (JRA) Use hcer_raw_adc instead of hcer_adc
*
* Revision 1.3  1995/05/22 19:37:05  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.2  1995/05/17  13:56:04  cdaq
* (JRA) Add Cernekov pedestals, cosmetic changes
*
* Revision 1.1  1995/04/01  19:36:40  cdaq
* Initial revision
*
*
      implicit none
      save
*
      character*18 here
      parameter (here='h_analyze_pedestal')
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
      INCLUDE 'hms_data_structures.cmn'
      INCLUDE 'hms_pedestals.cmn'
*
*
* HODOSCOPE PEDESTALS
*

      do ihit = 1 , hscin_all_tot_hits
        pln = hscin_all_plane_num(ihit)
        cnt = hscin_all_counter_num(ihit)
        hhodo_pos_ped_sum2(pln,cnt) = hhodo_pos_ped_sum2(pln,cnt) +
     &          hscin_all_adc_pos(ihit)*hscin_all_adc_pos(ihit)
        hhodo_neg_ped_sum2(pln,cnt) = hhodo_neg_ped_sum2(pln,cnt) +
     &          hscin_all_adc_neg(ihit)*hscin_all_adc_neg(ihit)
        hhodo_pos_ped_sum(pln,cnt) = hhodo_pos_ped_sum(pln,cnt) +
     &          hscin_all_adc_pos(ihit)
        hhodo_neg_ped_sum(pln,cnt) = hhodo_neg_ped_sum(pln,cnt) +
     &          hscin_all_adc_neg(ihit)
        hhodo_pos_ped_num(pln,cnt) = hhodo_pos_ped_num(pln,cnt) + 1
        hhodo_neg_ped_num(pln,cnt) = hhodo_neg_ped_num(pln,cnt) + 1
      enddo
*
*
* CALORIMETER PEDESTALS
*

      do ihit = 1 , hcal_tot_hits
        row = hcal_row(ihit)
        col = hcal_column(ihit)
        blk = row + (col-1)*hmax_cal_rows
        hcal_ped_sum2(blk) = hcal_ped_sum2(blk) + hcal_adc(ihit)*hcal_adc(ihit)
        hcal_ped_sum(blk) = hcal_ped_sum(blk) + hcal_adc(ihit)
        hcal_ped_num(blk) = hcal_ped_num(blk) + 1
      enddo
*
*
* CERENKOV PEDESTALS
*
      do ihit = 1 , hcer_tot_hits
        pmt=hcer_tube_num(ihit)      ! no sparsification yet - NEED TO FIX!!!!
        hcer_ped_sum2(pmt) = hcer_ped_sum2(pmt) +
     $       hcer_raw_adc(ihit)*hcer_raw_adc(ihit)
        hcer_ped_sum(pmt) = hcer_ped_sum(pmt) + hcer_raw_adc(ihit)
        hcer_ped_num(pmt) = hcer_ped_num(pmt) + 1
      enddo

      return
      end
