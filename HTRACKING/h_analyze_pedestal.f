      subroutine h_analyze_pedestal(ABORT,err)
*
* $Log$
* Revision 1.1  1995/04/01 19:36:40  cdaq
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
      integer*4 blk
*
      INCLUDE 'gen_data_structures.cmn'
      INCLUDE 'hms_pedestals.cmn'
*
*
* HODOSCOPE PEDESTALS
*

      do ihit = 1 , hscin_all_tot_hits
        pln = hscin_all_plane_num(ihit)
        cnt = hscin_all_counter_num(ihit)
        hhodo_pos_ped_sum2(pln,cnt) = hhodo_pos_ped_sum2(pln,cnt) +
     &          hscin_all_adc_pos(ihit) * hscin_all_adc_pos(ihit)
        hhodo_neg_ped_sum2(pln,cnt) = hhodo_neg_ped_sum2(pln,cnt) +
     &          hscin_all_adc_neg(ihit) * hscin_all_adc_neg(ihit)
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

      do blk = 1 , hcal_tot_hits
        hcal_ped_sum2(blk) = hcal_ped_sum2(blk) +
     &          hscin_all_adc_pos(blk) * hscin_all_adc_pos(blk)
        hcal_ped_sum(blk) = hcal_ped_sum(blk) + hscin_all_adc_pos(blk)
        hcal_ped_num(blk) = hcal_ped_num(blk) + 1
      enddo

      return
      end
