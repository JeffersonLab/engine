      subroutine s_analyze_pedestal(ABORT,err)
*
* $Log$
* Revision 1.1  1995/04/01 19:35:31  cdaq
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
      integer*4 blk
*
      INCLUDE 'gen_data_structures.cmn'
      INCLUDE 'sos_pedestals.cmn'
*
*
* HODOSCOPE PEDESTALS
*

      do ihit = 1 , sscin_all_tot_hits
        pln = sscin_all_plane_num(ihit)
        cnt = sscin_all_counter_num(ihit)
        shodo_pos_ped_sum2(pln,cnt) = shodo_pos_ped_sum2(pln,cnt) +
     &          sscin_all_adc_pos(ihit) * sscin_all_adc_pos(ihit)
        shodo_neg_ped_sum2(pln,cnt) = shodo_neg_ped_sum2(pln,cnt) +
     &          sscin_all_adc_neg(ihit) * sscin_all_adc_neg(ihit)
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

      do blk = 1 , scal_tot_hits
        scal_ped_sum2(blk) = scal_ped_sum2(blk) +
     &          sscin_all_adc_pos(blk) * sscin_all_adc_pos(blk)
        scal_ped_sum(blk) = scal_ped_sum(blk) + sscin_all_adc_pos(blk)
        scal_ped_num(blk) = scal_ped_num(blk) + 1
      enddo

      return
      end
