      subroutine h_calc_pedestal(ABORT,err)
*
* $Log$
* Revision 1.6  1995/10/09 20:12:10  cdaq
* (JRA) Note pedestals that differ by 2 sigma from parameter file
*
* Revision 1.5  1995/08/31 14:58:48  cdaq
* (JRA) Change threshold limits
*
* Revision 1.4  1995/07/19  18:09:51  cdaq
* (JRA) Cleanup statistics calculations
*
* Revision 1.3  1995/05/22  19:39:06  cdaq
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
      integer*4 ind
      real*4 sig2
      real*4 num
*
      INCLUDE 'hms_data_structures.cmn'
      INCLUDE 'hms_pedestals.cmn'
      INCLUDE 'hms_scin_parms.cmn'
      INCLUDE 'hms_calorimeter.cmn'
*
*
* HODOSCOPE PEDESTALS
*
      ind = 0
      do pln = 1 , hnum_scin_planes
        do cnt = 1 , hnum_scin_elements

*calculate new pedestal values, positive tubes first.
          num=max(1.,float(hhodo_pos_ped_num(pln,cnt)))
          hhodo_new_ped_pos(pln,cnt) = float(hhodo_pos_ped_sum(pln,cnt)) / num
          sig2 = float(hhodo_pos_ped_sum2(pln,cnt))/num -
     $           hhodo_new_ped_pos(pln,cnt)**2
          hhodo_new_sig_pos(pln,cnt) = sqrt(max(0.,sig2))

*note channels with 2 sigma difference from paramter file values.
          if (abs(hscin_all_ped_pos(pln,cnt)-hhodo_new_ped_pos(pln,cnt))
     &            .ge.(2.*hhodo_new_sig_pos(pln,cnt))) then
            ind = ind + 1     !final value of 'ind' is saved at end of loop
            hhodo_changed_plane(ind)=pln
            hhodo_changed_element(ind)=cnt
            hhodo_changed_sign(ind)= 1       !1=pos,2=neg.
            hhodo_ped_change(ind) = hhodo_new_ped_pos(pln,cnt) - 
     &                              hscin_all_ped_pos(pln,cnt)
          endif   !large pedestal change

*replace old peds (from param file) with calculated pedestals
          if (num.gt.hhodo_min_peds .and. hhodo_min_peds.ne.0) then
            hscin_all_ped_pos(pln,cnt)=hhodo_new_ped_pos(pln,cnt)
          endif

*do it all again for negative tubes.
          num=max(1.,float(hhodo_neg_ped_num(pln,cnt)))
          hhodo_new_ped_neg(pln,cnt) = float(hhodo_neg_ped_sum(pln,cnt)) / num
          sig2 = float(hhodo_neg_ped_sum2(pln,cnt))/num -
     $           hhodo_new_ped_neg(pln,cnt)**2
          hhodo_new_sig_neg(pln,cnt) = sqrt(max(0.,sig2))

          if (abs(hscin_all_ped_neg(pln,cnt)-hhodo_new_ped_neg(pln,cnt))
     &            .ge.(2.*hhodo_new_sig_neg(pln,cnt))) then
            ind = ind + 1
            hhodo_changed_plane(ind)=pln
            hhodo_changed_element(ind)=cnt
            hhodo_changed_sign(ind)= 2       !1=pos, 2=neg.
            hhodo_ped_change(ind) = hhodo_new_ped_neg(pln,cnt) - 
     &                              hscin_all_ped_neg(pln,cnt)
          endif   !large pedestal change

          if (num.gt.hhodo_min_peds .and. hhodo_min_peds.ne.0) then
            hscin_all_ped_neg(pln,cnt)=hhodo_new_ped_neg(pln,cnt)
          endif

        enddo                      !counters
      enddo                        !planes
      hhodo_num_ped_changes = ind
*
*
* CALORIMETER PEDESTALS
*
      ind = 0
      do blk = 1 , hmax_cal_blocks
        num=max(1.,float(hcal_ped_num(blk)))
        hcal_new_ped(blk) = hcal_ped_sum(blk) / num
        sig2 = float(hcal_ped_sum2(blk))/num - hcal_new_ped(blk)**2
        hcal_new_rms(blk) = sqrt(max(0.,sig2))

        if (abs(hcal_ped_mean(blk)-hcal_new_ped(blk))
     &                 .ge.(2.*hcal_new_rms(blk))) then
          ind = ind + 1
          hcal_changed_block(ind)=blk
          hcal_ped_change(ind)=hcal_new_ped(blk)-hcal_ped_mean(blk) 
        endif

        if (num.gt.hcal_min_peds .and. hcal_min_peds.ne.0) then
          hcal_ped_mean(blk)=hcal_new_ped(blk)
          hcal_ped_rms(blk)=hcal_new_rms(blk)
          hcal_threshold(blk) = min(50.,max(10.,3.*hcal_new_rms(blk)))
        endif

      enddo
      hcal_num_ped_changes = ind


*
*
* GAS CERENKOV PEDESTALS
*
      ind = 0
      do pmt = 1 , hmax_cer_hits
        num=max(1.,float(hcer_ped_num(pmt)))
        hcer_new_ped(pmt) = float(hcer_ped_sum(pmt)) / num
        sig2 = float(hcer_ped_sum2(pmt))/ num - hcer_new_ped(pmt)**2
        hcer_new_rms(pmt) = sqrt(max(0.,sig2))
      enddo
      if (abs(hcer_ped_mean(pmt)-hcer_new_ped(pmt))
     &              .ge.(2.*hcer_new_rms(pmt))) then
        ind = ind + 1
        hcer_changed_tube(ind)=pmt
        hcer_ped_change(ind)=hcer_new_ped(pmt)-hcer_ped_mean(pmt) 
      endif
      hcer_num_ped_changes = ind

      if (num.gt.hcer_min_peds .and. hcer_min_peds.ne.0) then
        hcer_ped_mean(pmt)=hcer_new_ped(pmt)
        hcer_ped_rms(pmt)=hcer_new_rms(pmt)
      endif

c      hcer_threshold(pmt) = max(4.,3.*hcer_ped_rms(pmt))

      return
      end
