      subroutine h_calc_pedestal(ABORT,err)
*
* $Log$
* Revision 1.3  1995/05/22 19:39:06  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.2  1995/05/17  13:56:54  cdaq
* (JRA) Float integer accumulators before arithmetic
*
* Revision 1.1  1995/04/01  19:36:25  cdaq
* Initial revision
*
*
      implicit none
      save
*
      character*18 here
      parameter (here='h_calc_pedestal')
*
      logical ABORT
      character*(*) err
*
      integer*4 pln,cnt
      integer*4 blk
      integer*4 pmt
      real*4 sig2
*
      INCLUDE 'hms_data_structures.cmn'
      INCLUDE 'hms_pedestals.cmn'
      INCLUDE 'hms_scin_parms.cmn'
      INCLUDE 'hms_calorimeter.cmn'
*
*
* HODOSCOPE PEDESTALS
*
      do pln = 1 , hnum_scin_planes
        do cnt = 1 , hnum_scin_elements
          if (hhodo_pos_ped_num(pln,cnt) .ge. hhodo_min_peds .and.
     &        hhodo_min_peds .ne. 0) then
            hscin_all_ped_pos(pln,cnt) = float(hhodo_pos_ped_sum(pln,cnt)) /
     &              float(hhodo_pos_ped_num(pln,cnt))
c            sig2 = hhodo_pos_ped_sum2(pln,cnt) / hhodo_pos_ped_num(pln,cnt) -
c     &              hscin_all_ped_pos(pln,cnt)**2
c            if (sig2.le.0) write(6,*) 'pos ped(',pln,',',cnt,') =',sig2
c            hscin_all_sig_pos(pln,cnt) = sqrt(max(0.,sig2))
          endif
          if (hhodo_neg_ped_num(pln,cnt) .ge. hhodo_min_peds .and.
     &        hhodo_min_peds .ne. 0) then
            hscin_all_ped_neg(pln,cnt) = float(hhodo_neg_ped_sum(pln,cnt)) /
     &              float(hhodo_neg_ped_num(pln,cnt))
c            sig2 = hhodo_neg_ped_sum2(pln,cnt) / hhodo_neg_ped_num(pln,cnt) -
c     &              hscin_all_ped_neg(pln,cnt)**2
c            if (sig2.le.0) write(6,*) 'neg ped(',pln,',',cnt,') =',sig2
c            hscin_all_sig_neg(pln,cnt) = sqrt(max(0.,sig2))
          endif
        enddo
      enddo
*
*
* CALORIMETER PEDESTALS
*
      do blk = 1 , hmax_cal_blocks
        if (hcal_ped_num(blk) .ge. hcal_min_peds .and.
     &      hcal_min_peds .ne. 0) then
          hcal_ped_mean(blk) = hcal_ped_sum(blk) / hcal_ped_num(blk)
          sig2 = float(hcal_ped_sum2(blk))/float(hcal_ped_num(blk))
     $         /float(hcal_ped_num(blk)) - float(hcal_ped_mean(blk))**2
!          if (sig2.le.0) write(6,*) 'cal ped(',blk,') =',sig2
          hcal_ped_rms(blk) = sqrt(max(0.,sig2))
          hcal_threshold(blk) = max(4.,3.*hcal_ped_rms(blk))
        endif
      enddo

*
*
* GAS CERENKOV PEDESTALS
*
      do pmt = 1 , hmax_cer_hits
        if (hcer_ped_num(pmt) .ge. hcer_min_peds .and.
     &      hcer_min_peds .ne. 0) then
          hcer_ped_mean(pmt) = float(hcer_ped_sum(pmt))
     $         / float(hcer_ped_num(pmt))
          sig2 = float(hcer_ped_sum2(pmt))/float(hcer_ped_num(pmt))
     $         /float(hcer_ped_num(pmt)) - float(hcer_ped_mean(pmt))**2
!          if (sig2.le.0) write(6,*) 'cer ped(',pmt,') =',sig2
          hcer_ped_rms(pmt) = sqrt(max(0.,sig2))
          hcer_threshold(pmt) = max(4.,3.*hcer_ped_rms(pmt))
        endif
      enddo

      return
      end
