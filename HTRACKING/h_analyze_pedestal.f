      subroutine h_analyze_pedestal(ABORT,err)
*
* $Log: h_analyze_pedestal.f,v $
* Revision 1.9  2002/12/20 21:53:34  jones
* Modified by Hamlet for new HMS aerogel
*
* Revision 1.9  2002/09/24 
* (Hamlet) Add pedestals for HMS Aerogel
*
* Revision 1.8  1999/06/10 16:46:06  csa
* (JRA) Removed two calorimeter debugging statements
*
* Revision 1.7  1999/02/23 18:33:31  csa
* (JRA) Implement improved pedestal calcs
*
* Revision 1.6  1998/12/17 22:02:37  saw
* Support extra set of tubes on HMS shower counter
*
* Revision 1.5  1996/01/24 15:55:06  saw
* (JRA) Add ped analysis for misc channels
*
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
      integer*4 ind
*
      INCLUDE 'hms_data_structures.cmn'
      INCLUDE 'hms_pedestals.cmn'
      INCLUDE 'hms_scin_parms.cmn'
*
*
* HODOSCOPE PEDESTALS
*
* Update ped limits after have enough events (h*_min_peds/5)
*
      do ihit = 1 , hscin_all_tot_hits
        pln = hscin_all_plane_num(ihit)
        cnt = hscin_all_counter_num(ihit)
        if (hscin_all_adc_pos(ihit).le.hhodo_pos_ped_limit(pln,cnt)) then
          hhodo_pos_ped_sum2(pln,cnt) = hhodo_pos_ped_sum2(pln,cnt) +
     &          hscin_all_adc_pos(ihit)*hscin_all_adc_pos(ihit)
          hhodo_pos_ped_sum(pln,cnt) = hhodo_pos_ped_sum(pln,cnt) +
     &          hscin_all_adc_pos(ihit)
          hhodo_pos_ped_num(pln,cnt) = hhodo_pos_ped_num(pln,cnt) + 1
          if (hhodo_pos_ped_num(pln,cnt).eq.nint(hhodo_min_peds/5.)) then
            hhodo_pos_ped_limit(pln,cnt) = 100 +
     &               hhodo_pos_ped_sum(pln,cnt) / hhodo_pos_ped_num(pln,cnt)
          endif
        endif
        if (hscin_all_adc_neg(ihit).le.hhodo_neg_ped_limit(pln,cnt)) then
          hhodo_neg_ped_sum2(pln,cnt) = hhodo_neg_ped_sum2(pln,cnt) +
     &          hscin_all_adc_neg(ihit)*hscin_all_adc_neg(ihit)
          hhodo_neg_ped_sum(pln,cnt) = hhodo_neg_ped_sum(pln,cnt) +
     &          hscin_all_adc_neg(ihit)
          hhodo_neg_ped_num(pln,cnt) = hhodo_neg_ped_num(pln,cnt) + 1
          if (hhodo_neg_ped_num(pln,cnt).eq.nint(hhodo_min_peds/5.)) then
            hhodo_neg_ped_limit(pln,cnt) = 100 +
     &               hhodo_neg_ped_sum(pln,cnt) / hhodo_neg_ped_num(pln,cnt)
          endif
        endif

* fill pedestal histograms.
c        histval = hscin_all_adc_pos(ihit)-hscin_all_ped_pos(pln,cnt)
c        call hf1(hidsumposadc(pln),histval,1.)
c        histval = hscin_all_adc_neg(ihit)-hscin_all_ped_neg(pln,cnt)
c        call hf1(hidsumnegadc(pln),histval,1.)

      enddo
*
*
* CALORIMETER PEDESTALS
*

      do ihit = 1 , hcal_tot_hits
        row = hcal_row(ihit)
        col = hcal_column(ihit)
        blk = row + (col-1)*hmax_cal_rows

        if (hcal_adc_pos(ihit) .le. hcal_pos_ped_limit(blk)) then
          hcal_pos_ped_sum2(blk) = hcal_pos_ped_sum2(blk) +
     &       hcal_adc_pos(ihit)*hcal_adc_pos(ihit)
          hcal_pos_ped_sum(blk) = hcal_pos_ped_sum(blk) + hcal_adc_pos(ihit)
          hcal_pos_ped_num(blk) = hcal_pos_ped_num(blk) + 1
          if (hcal_pos_ped_num(blk).eq.nint(hcal_min_peds/5.)) then
            hcal_pos_ped_limit(blk) = 100 +
     &              hcal_pos_ped_sum(blk) / hcal_pos_ped_num(blk)
          endif
        endif

        if (hcal_adc_neg(ihit) .le. hcal_neg_ped_limit(blk)) then
          hcal_neg_ped_sum2(blk) = hcal_neg_ped_sum2(blk) +
     &       hcal_adc_neg(ihit)*hcal_adc_neg(ihit)
          hcal_neg_ped_sum(blk) = hcal_neg_ped_sum(blk) + hcal_adc_neg(ihit)
          hcal_neg_ped_num(blk) = hcal_neg_ped_num(blk) + 1
          if (hcal_neg_ped_num(blk).eq.nint(hcal_min_peds/5.)) then
            hcal_neg_ped_limit(blk) = 100 +
     &              hcal_neg_ped_sum(blk) / hcal_neg_ped_num(blk)
          endif
        endif
      enddo
*
*
* CERENKOV PEDESTALS
*
      do ihit = 1 , hcer_tot_hits
        pmt=hcer_tube_num(ihit)      ! no sparsification yet - NEED TO FIX!!!!
        if (hcer_raw_adc(ihit) .le. hcer_ped_limit(pmt)) then
          hcer_ped_sum2(pmt) = hcer_ped_sum2(pmt) +
     $       hcer_raw_adc(ihit)*hcer_raw_adc(ihit)
          hcer_ped_sum(pmt) = hcer_ped_sum(pmt) + hcer_raw_adc(ihit)
          hcer_ped_num(pmt) = hcer_ped_num(pmt) + 1
          if (hcer_ped_num(pmt).eq.nint(hcer_min_peds/5.)) then
            hcer_ped_limit(pmt) = 100 +
     &              hcer_ped_sum(pmt) / hcer_ped_num(pmt)
          endif
        endif
      enddo
*
*.............................................................................
*
* AEROGEL CERENKOV PEDESTALS
*
      do ihit = 1 , haero_tot_hits
        blk = haero_pair_num(ihit)
        if (haero_adc_pos(ihit) .le. haero_pos_ped_limit(blk)) then
          haero_pos_ped_sum2(blk) = haero_pos_ped_sum2(blk) + haero_adc_pos(ihit)*haero_adc_pos(ihit)
          haero_pos_ped_sum(blk) = haero_pos_ped_sum(blk) + haero_adc_pos(ihit)
          haero_pos_ped_num(blk) = haero_pos_ped_num(blk) + 1
          if (haero_pos_ped_num(blk).eq.nint(haero_min_peds/5.)) then
            haero_pos_ped_limit(blk) = 100 +
     &              haero_pos_ped_sum(blk) / haero_pos_ped_num(blk)
          endif
        endif
        if (haero_adc_neg(ihit) .le. haero_neg_ped_limit(blk)) then
          haero_neg_ped_sum2(blk) = haero_neg_ped_sum2(blk) + haero_adc_neg(ihit)*haero_adc_neg(ihit)
          haero_neg_ped_sum(blk) = haero_neg_ped_sum(blk) + haero_adc_neg(ihit)
          haero_neg_ped_num(blk) = haero_neg_ped_num(blk) + 1
          if (haero_neg_ped_num(blk).eq.nint(haero_min_peds/5.)) then
            haero_neg_ped_limit(blk) = 100 +
     &              haero_neg_ped_sum(blk) / haero_neg_ped_num(blk)
          endif
        endif
      enddo
*
*............................................................................
*
* MISC PEDESTALS
*
      do ihit = 1 , hmisc_tot_hits
        if (hmisc_raw_addr1(ihit).eq.2) then   !ADCs
          ind=hmisc_raw_addr2(ihit)      ! no sparsification yet - NEED TO FIX!!!!
          if (hmisc_raw_data(ihit) .le. hmisc_ped_limit(ind)) then
            hmisc_ped_sum2(ind) = hmisc_ped_sum2(ind) +
     $         hmisc_raw_data(ihit)*hmisc_raw_data(ihit)
            hmisc_ped_sum(ind) = hmisc_ped_sum(ind) + hmisc_raw_data(ihit)
            hmisc_ped_num(ind) = hmisc_ped_num(ind) + 1
            if (hmisc_ped_num(ind).eq.nint(hmisc_min_peds/5.)) then
              hmisc_ped_limit(ind) = 100 +
     &                hmisc_ped_sum(ind) / hmisc_ped_num(ind)
            endif
          endif
        endif
      enddo

      return
      end
