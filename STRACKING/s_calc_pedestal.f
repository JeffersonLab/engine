      subroutine s_calc_pedestal(ABORT,err)
*
* $Log$
* Revision 1.5  1995/08/31 18:04:55  cdaq
* (JRA) Change threshold limits
*
* Revision 1.4  1995/07/20  14:46:39  cdaq
* (JRA) Cleanup statistics calculations
*
* Revision 1.3  1995/05/22  19:45:32  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.2  1995/05/17  16:42:40  cdaq
* (JRA) Add gas cerenkov and Aerogel, float integer accumulators before arithmetic
*
* Revision 1.1  1995/04/01  19:36:03  cdaq
* Initial revision
*
*
      implicit none
      save
*
      character*18 here
      parameter (here='s_calc_pedestal')
*
      logical ABORT
      character*(*) err
*
      integer*4 pln,cnt
      integer*4 blk
      integer*4 pmt
      real*4 sig2
*
      INCLUDE 'sos_data_structures.cmn'
      INCLUDE 'sos_pedestals.cmn'
      INCLUDE 'sos_scin_parms.cmn'
      INCLUDE 'sos_calorimeter.cmn'
*
*
* HODOSCOPE PEDESTALS
*
      do pln = 1 , snum_scin_planes
        do cnt = 1 , snum_scin_elements
          if (shodo_pos_ped_num(pln,cnt) .ge. shodo_min_peds .and.
     &        shodo_min_peds .ne. 0) then
            sscin_all_ped_pos(pln,cnt) = shodo_pos_ped_sum(pln,cnt)/
     &              float(shodo_pos_ped_num(pln,cnt))
            sig2 = float(shodo_pos_ped_sum2(pln,cnt))/
     &              float(shodo_pos_ped_num(pln,cnt))-
     &              sscin_all_ped_pos(pln,cnt)**2
!            if (sig2.le.0) write(6,*) 'pos ped(',pln,',',cnt,')**2 =',sig2
            shodo_all_sig_pos(pln,cnt) = sqrt(max(0.,sig2))
           endif
          if (shodo_neg_ped_num(pln,cnt) .ge. shodo_min_peds .and.
     &        shodo_min_peds .ne. 0) then
            sscin_all_ped_neg(pln,cnt) = shodo_neg_ped_sum(pln,cnt)/
     &              float(shodo_neg_ped_num(pln,cnt))
            sig2 = float(shodo_neg_ped_sum2(pln,cnt))/
     &              float(shodo_neg_ped_num(pln,cnt))-
     &              sscin_all_ped_neg(pln,cnt)**2
!            if (sig2.le.0) write(6,*) 'neg ped(',pln,',',cnt,')**2 =',sig2
            shodo_all_sig_neg(pln,cnt) = sqrt(max(0.,sig2))
          endif
        enddo
      enddo
*
*
* CALORIMETER PEDESTALS
*

      do blk = 1 , smax_cal_blocks
        if (scal_ped_num(blk) .ge. scal_min_peds .and.
     &      scal_min_peds .ne. 0) then
          scal_ped_mean(blk) = scal_ped_sum(blk) / float(scal_ped_num(blk))
          sig2 = float(scal_ped_sum2(blk))/float(scal_ped_num(blk)) -
     &              scal_ped_mean(blk)**2
!          if (sig2.le.0) write(6,*) 'cal ped(',blk,')**2 =',sig2
          scal_ped_rms(blk) = sqrt(max(0.,sig2))
          scal_threshold(blk) = min(50.,max(10.,3.*scal_ped_rms(blk)))
        endif
      enddo
*
*
* GAS CERENKOV PEDESTALS
*
      do pmt = 1 , smax_cer_hits
        if (scer_ped_num(pmt) .ge. scer_min_peds .and.
     &      scer_min_peds .ne. 0) then
          scer_ped_mean(pmt) = scer_ped_sum(pmt) / float(scer_ped_num(pmt))
          sig2 = float(scer_ped_sum2(pmt))/
     &           float(scer_ped_num(pmt))-
     &           scer_ped_mean(pmt)**2
!          if (sig2.le.0) write(6,*) 'cer ped(',pmt,')**2 =',sig2
          scer_ped_rms(pmt) = sqrt(max(0.,sig2))
          scer_threshold(pmt) = max(4.,3.*scer_ped_rms(pmt))
        endif
      enddo

*
*
* AEROGEL CERENKOV PEDESTALS
*
      do pmt = 1 , (smax_aer_hits-1)
        if (saer_pos_ped_num(pmt) .ge. saer_min_peds .and.
     &      saer_min_peds .ne. 0) then
          saer_pos_ped_mean(pmt) = saer_pos_ped_sum(pmt) /
     &      float(saer_pos_ped_num(pmt))
          sig2 = float(saer_pos_ped_sum2(pmt))/
     &            float(saer_pos_ped_num(pmt))-
     &            saer_pos_ped_mean(pmt)**2
!          if (sig2.le.0) write(6,*) 'aer pos ped(',pmt,') =',sig2
          saer_pos_ped_rms(pmt) = sqrt(max(0.,sig2))
          saer_pos_threshold(pmt) = max(4.,3.*saer_pos_ped_rms(pmt))
        endif
        if (saer_neg_ped_num(pmt) .ge. saer_min_peds .and.
     &      saer_min_peds .ne. 0) then
          saer_neg_ped_mean(pmt) = saer_neg_ped_sum(pmt) /
     &      float(saer_neg_ped_num(pmt))
          sig2 = float(saer_neg_ped_sum2(pmt))/
     &            float(saer_neg_ped_num(pmt))-
     &            saer_neg_ped_mean(pmt)**2
!          if (sig2.le.0) write(6,*) 'aer neg ped(',pmt,') =',sig2
          saer_neg_ped_rms(pmt) = sqrt(max(0.,sig2))
          saer_neg_threshold(pmt) = max(4.,3.*saer_neg_ped_rms(pmt))
        endif
      enddo

      return
      end
