      subroutine s_calc_pedestal(ABORT,err)
*
* $Log$
* Revision 1.6  1995/10/09 20:12:30  cdaq
* (JRA) Note pedestals that differ by 2 sigma from parameter file
*
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
      integer*4 ind
      real*4 sig2
      real*4 num
*
      INCLUDE 'sos_data_structures.cmn'
      INCLUDE 'sos_pedestals.cmn'
      INCLUDE 'sos_scin_parms.cmn'
      INCLUDE 'sos_calorimeter.cmn'
*
*
* HODOSCOPE PEDESTALS
*
      ind = 0
      do pln = 1 , snum_scin_planes
        do cnt = 1 , snum_scin_elements

*calculate new pedestal values, positive tubes first.
          num=max(1.,float(shodo_pos_ped_num(pln,cnt)))
          shodo_new_ped_pos(pln,cnt) = float(shodo_pos_ped_sum(pln,cnt)) / num
          sig2 = float(shodo_pos_ped_sum2(pln,cnt))/num -
     $           shodo_new_ped_pos(pln,cnt)**2
          shodo_new_sig_pos(pln,cnt) = sqrt(max(0.,sig2))

*note channels with 2 sigma difference from paramter file values.
          if (abs(sscin_all_ped_pos(pln,cnt)-shodo_new_ped_pos(pln,cnt))
     &            .ge.(2.*shodo_new_sig_pos(pln,cnt))) then
            ind = ind + 1     !final value of 'ind' is saved at end of loop
            shodo_changed_plane(ind)=pln
            shodo_changed_element(ind)=cnt
            shodo_changed_sign(ind)= 1       !1=pos,2=neg.
            shodo_ped_change(ind) = shodo_new_ped_pos(pln,cnt) -
     &                              sscin_all_ped_pos(pln,cnt)
          endif   !large pedestal change

*replace old peds (from param file) with calculated pedestals
          if (num.gt.shodo_min_peds .and. shodo_min_peds.ne.0) then
            sscin_all_ped_pos(pln,cnt)=shodo_new_ped_pos(pln,cnt)
          endif

*do it all again for negative tubes.
          num=max(1.,float(shodo_neg_ped_num(pln,cnt)))
          shodo_new_ped_neg(pln,cnt) = float(shodo_neg_ped_sum(pln,cnt)) / num
          sig2 = float(shodo_neg_ped_sum2(pln,cnt))/num -
     $           shodo_new_ped_neg(pln,cnt)**2
          shodo_new_sig_neg(pln,cnt) = sqrt(max(0.,sig2))

          if (abs(sscin_all_ped_neg(pln,cnt)-shodo_new_ped_neg(pln,cnt))
     &            .ge.(2.*shodo_new_sig_neg(pln,cnt))) then
            ind = ind + 1
            shodo_changed_plane(ind)=pln
            shodo_changed_element(ind)=cnt
            shodo_changed_sign(ind)= 2       !1=pos, 2=neg.
            shodo_ped_change(ind) = shodo_new_ped_neg(pln,cnt) -
     &                              sscin_all_ped_neg(pln,cnt)
          endif   !large pedestal change

          if (num.gt.shodo_min_peds .and. shodo_min_peds.ne.0) then
            sscin_all_ped_neg(pln,cnt)=shodo_new_ped_neg(pln,cnt)
          endif

        enddo                      !counters
      enddo                        !planes
      shodo_num_ped_changes = ind
*
*
* CALORIMETER PEDESTALS
*
      ind = 0
      do blk = 1 , smax_cal_blocks
        num=max(1.,float(scal_ped_num(blk)))
        scal_new_ped(blk) = scal_ped_sum(blk) / num
        sig2 = float(scal_ped_sum2(blk))/num - scal_new_ped(blk)**2
        scal_new_rms(blk) = sqrt(max(0.,sig2))

        if (abs(scal_ped_mean(blk)-scal_new_ped(blk))
     &                 .ge.(2.*scal_new_rms(blk))) then
          ind = ind + 1
          scal_changed_block(ind)=blk
          scal_ped_change(ind)=scal_new_ped(blk)-scal_ped_mean(blk)
        endif

        if (num.gt.scal_min_peds .and. scal_min_peds.ne.0) then
          scal_ped_mean(blk)=scal_new_ped(blk)
          scal_ped_rms(blk)=scal_new_rms(blk)
          scal_threshold(blk) = min(50.,max(10.,3.*scal_new_rms(blk)))
        endif

      enddo
      scal_num_ped_changes = ind


*
*
* GAS CERENKOV PEDESTALS
*
      ind = 0
      do pmt = 1 , smax_cer_hits
        num=max(1.,float(scer_ped_num(pmt)))
        scer_new_ped(pmt) = float(scer_ped_sum(pmt)) / num
        sig2 = float(scer_ped_sum2(pmt))/ num - scer_new_ped(pmt)**2
        scer_new_rms(pmt) = sqrt(max(0.,sig2))
      enddo
      if (abs(scer_ped_mean(pmt)-scer_new_ped(pmt))
     &              .ge.(2.*scer_new_rms(pmt))) then
        ind = ind + 1
        scer_changed_tube(ind)=pmt
        scer_ped_change(ind)=scer_new_ped(pmt)-scer_ped_mean(pmt)
      endif
      scer_num_ped_changes = ind

      if (num.gt.scer_min_peds .and. scer_min_peds.ne.0) then
        scer_ped_mean(pmt)=scer_new_ped(pmt)
        scer_ped_rms(pmt)=scer_new_rms(pmt)
      endif

c      scer_threshold(pmt) = max(4.,3.*scer_ped_rms(pmt))

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
