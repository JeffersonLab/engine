      subroutine h_calc_pedestal(ABORT,err)
*
* $Log$
* Revision 1.1  1995/04/01 19:36:25  cdaq
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
      real*4 sig2
*
      INCLUDE 'gen_data_structures.cmn'
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
            hscin_all_ped_pos(pln,cnt) = hhodo_pos_ped_sum(pln,cnt) /
     &              hhodo_pos_ped_num(pln,cnt)
            hscin_all_ped_neg(pln,cnt) = hhodo_neg_ped_sum(pln,cnt) /
     &              hhodo_neg_ped_num(pln,cnt)
c            sig2 = hhodo_pos_ped_sum2(pln,cnt) / hhodo_pos_ped_num(pln,cnt) -
c     &              hscin_all_ped_pos(pln,cnt)**2
c            if (sig2.le.0) write(6,*) 'pos ped(',pln,',',cnt,') =',sig2
c            hscin_all_sig_pos(pln,cnt) = sqrt(max(0.,sig2))
c            sig2 = hhodo_neg_ped_sum2(pln,cnt) / hhodo_neg_ped_num(pln,cnt) -
c     &              hscin_all_ped_neg(pln,cnt)**2
c!            if (sig2.le.0) write(6,*) 'neg ped(',pln,',',cnt,') =',sig2
c            hscin_all_sig_neg(pln,cnt) = sqrt(max(0.,sig2))
          endif
        enddo
      enddo
*
*
* CALORIMETER PEDESTALS
*

      do blk = 1 , hcal_tot_hits
        if (hcal_ped_num(blk) .ge. hcal_min_peds .and.
     &      hcal_min_peds .ne. 0) then
          hcal_ped_mean(blk) = hcal_ped_sum(blk) / hcal_ped_num(blk)
          sig2 = hcal_ped_sum2(blk)/hcal_ped_num(blk)/hcal_ped_num(blk) -
     &              hcal_ped_mean(blk)**2
!          if (sig2.le.0) write(6,*) 'cal ped(',blk,') =',sig2
          hcal_ped_rms(blk) = sqrt(max(0.,sig2))
          hcal_threshold(blk) = max(4.,3.*hcal_ped_rms(blk))
        endif
      enddo

      return
      end
