      subroutine s_calc_pedestal(ABORT,err)
*
* $Log$
* Revision 1.7  1996/01/17 19:03:49  cdaq
* (JRA) Fixes, write results to file.
*
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
      integer*4 roc,slot
      integer*4 signalcount
      real*4 sig2
      real*4 num
      character*80 file
*
      INCLUDE 'sos_data_structures.cmn'
      INCLUDE 'sos_pedestals.cmn'
      INCLUDE 'sos_scin_parms.cmn'
      INCLUDE 'sos_calorimeter.cmn'
      INCLUDE 'sos_cer_parms.cmn'
      INCLUDE 'sos_filenames.cmn'
      INCLUDE 'gen_run_info.cmn'
*
      integer SPAREID
      parameter (SPAREID=67)
*
*
* HODOSCOPE PEDESTALS
*
      ind = 0
      do pln = 1 , snum_scin_planes
        do cnt = 1 , snum_scin_counters(pln)

*calculate new pedestal values, positive tubes first.
          num=max(1.,float(shodo_pos_ped_num(pln,cnt)))
          shodo_new_ped_pos(pln,cnt) = float(shodo_pos_ped_sum(pln,cnt)) / num
          sig2 = float(shodo_pos_ped_sum2(pln,cnt))/num -
     $           shodo_new_ped_pos(pln,cnt)**2
          shodo_new_sig_pos(pln,cnt) = sqrt(max(0.,sig2))
c          shodo_new_threshold_pos(pln,cnt) = shodo_new_ped_pos(pln,cnt)+
c     &            2.*shodo_new_sig_pos(pln,cnt)
          shodo_new_threshold_pos(pln,cnt) = shodo_new_ped_pos(pln,cnt)+10.

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
c          shodo_new_threshold_neg(pln,cnt) = shodo_new_ped_neg(pln,cnt)+
c     &            2.*shodo_new_sig_neg(pln,cnt)
          shodo_new_threshold_neg(pln,cnt) = shodo_new_ped_neg(pln,cnt)+10.

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
c        scal_new_adc_threshold(blk) = scal_new_ped(blk)+2.*scal_new_rms(blk)
        scal_new_adc_threshold(blk) = scal_new_ped(blk)+10.

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
c        scer_new_adc_threshold(pmt) = scer_new_ped(pmt)+2.*scer_new_rms(pmt)
        scer_new_adc_threshold(pmt) = scer_new_ped(pmt)+10.
      if (abs(scer_ped(pmt)-scer_new_ped(pmt))
     &              .ge.(2.*scer_new_rms(pmt))) then
        ind = ind + 1
        scer_changed_tube(ind)=pmt
        scer_ped_change(ind)=scer_new_ped(pmt)-scer_ped(pmt)
      endif

      if (num.gt.scer_min_peds .and. scer_min_peds.ne.0) then
        scer_ped(pmt)=scer_new_ped(pmt)
      endif

      enddo
      scer_num_ped_changes = ind
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


*
* WRITE THRESHOLDS TO FILE FOR HARDWARE SPARCIFICATION
*
      if (s_threshold_output_filename.ne.' ') then
        file=s_threshold_output_filename
        call g_sub_run_number(file, gen_run_number)
      
        open(unit=SPAREID,file=file,status='unknown')

        write(SPAREID,*) '# This is the ADC threshold file generated automatically'
        write(SPAREID,*) 'from the pedestal data from run number ',gen_run_number

        roc=3

        slot=1
        signalcount=1
        write(SPAREID,*) 'slot=',slot
        call g_output_thresholds(SPAREID,roc,slot,signalcount,smax_cal_rows,
     &      scal_new_adc_threshold,0,scal_new_rms,0)

c Want aero as well.  For now, don't sparsify at all.
c        slot=3
c        signalcount=1
c        write(SPAREID,*) 'slot=',slot
c        call g_output_thresholds(SPAREID,roc,slot,signalcount,smax_cer_hits,
c     &      scer_new_adc_threshold,0,scer_new_rms,0)
c
        slot=3
        write(SPAREID,*) 'slot=',slot
        do ind=1,4
         write(SPAREID,*) '0'
        enddo
        do ind=17,34
         write(SPAREID,*) '4000'
        enddo
        do ind=35,64
         write(SPAREID,*) '0'
        enddo

        slot=7
        signalcount=2
        write(SPAREID,*) 'slot=',slot
        call g_output_thresholds(SPAREID,roc,slot,signalcount,snum_scin_planes,
     &      shodo_new_threshold_pos,shodo_new_threshold_neg,shodo_new_sig_pos,
     &      shodo_new_sig_neg)

        slot=9
        signalcount=2
        write(SPAREID,*) 'slot=',slot
        call g_output_thresholds(SPAREID,roc,slot,signalcount,snum_scin_planes,
     &      shodo_new_threshold_pos,shodo_new_threshold_neg,shodo_new_sig_pos,
     &      shodo_new_sig_neg)

        close (unit=SPAREID)

      endif

      return
      end
