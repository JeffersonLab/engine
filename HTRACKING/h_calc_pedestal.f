      subroutine h_calc_pedestal(ABORT,err)
*
* $Log$
* Revision 1.13.2.4  2003/04/16 12:10:27  cdaq
* Modified max(1.,haero_pos_ped_num(pmt)) to max(1.,float(haero_pos_ped_num(pmt)))
* and same for haero_neg_ped_num to compile on the Alpha machine (EB)
*
* Revision 1.13.2.3  2003/04/09 16:56:40  cdaq
* Modified to force haero_new_threshold_neg = 400 and pos = 400 (MKJ)
*
* Revision 1.13.2.2  2003/04/09 02:45:33  cdaq
* hardwire gas cerenkov thresholds to zero
*
* Revision 1.13.2.1  2003/04/06 06:21:28  cdaq
* Added (hardwired) output of beamline ADC thresholds and automatic output of aerogel pedestals
*
* Revision 1.13  2002/12/20 21:53:32  jones
* Modified by Hamlet for new HMS aerogel
*
* Revision 1.13  2002/09/24
* (Hamlet) Add Aerogel
*
* Revision 1.12  1999/02/23 18:36:12  csa
* (JRA) Cleanup
*
* Revision 1.11  1999/02/03 21:13:23  saw
* Code for new Shower counter tubes
*
* Revision 1.10  1998/12/17 22:02:38  saw
* Support extra set of tubes on HMS shower counter
*
* Revision 1.9  1996/08/30 19:53:01  saw
* (JRA) Up thresholds from 10 channels to 15 chans above pedestal
*
* Revision 1.8  1996/01/24 15:56:28  saw
* (JRA) Cleanup
*
* Revision 1.7  1996/01/16 21:44:03  cdaq
* (JRA) Improve Gas Cerenkov pedestals, add misc pedestals, write results to file.
*
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
      integer*4 imisc
      integer*4 ind,ihit
      integer*4 roc,slot
      integer*4 signalcount
      real*4 sig2
      real*4 num
!      character*132 file
*
      INCLUDE 'hms_data_structures.cmn'
      INCLUDE 'hms_pedestals.cmn'
      INCLUDE 'hms_scin_parms.cmn'
      INCLUDE 'hms_calorimeter.cmn'
      INCLUDE 'hms_cer_parms.cmn'
      INCLUDE 'hms_filenames.cmn'
      INCLUDE 'gen_run_info.cmn'
      INCLUDE 'hms_aero_parms.cmn'
*
      integer SPAREID
      parameter (SPAREID=67)

*
* HODOSCOPE PEDESTALS
*
      ind = 0
      do pln = 1 , hnum_scin_planes
        do cnt = 1 , hnum_scin_counters(pln)

*calculate new pedestal values, positive tubes first.
          num=max(1.,float(hhodo_pos_ped_num(pln,cnt)))
          hhodo_new_ped_pos(pln,cnt) = float(hhodo_pos_ped_sum(pln,cnt)) / num
          sig2 = float(hhodo_pos_ped_sum2(pln,cnt))/num -
     &           hhodo_new_ped_pos(pln,cnt)**2
          hhodo_new_sig_pos(pln,cnt) = sqrt(max(0.,sig2))
          hhodo_new_threshold_pos(pln,cnt)=hhodo_new_ped_pos(pln,cnt)+15.

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
          hhodo_new_threshold_neg(pln,cnt)=hhodo_new_ped_neg(pln,cnt)+15.

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
* CALORIMETER PEDESTALS
*
      ind = 0
      do blk = 1 , hmax_cal_blocks
      
* calculate new pedestal values, positive tubes first.      
       num=max(1.,float(hcal_pos_ped_num(blk)))
        hcal_new_ped_pos(blk)=hcal_pos_ped_sum(blk)/num
        sig2 = float(hcal_pos_ped_sum2(blk))/num - hcal_new_ped_pos(blk)**2
        hcal_new_rms_pos(blk)=sqrt(max(0.,sig2))
        hcal_new_adc_threshold_pos(blk)=hcal_new_ped_pos(blk)+15.
        if (abs(hcal_pos_ped_mean(blk)-hcal_new_ped_pos(blk))
     &                 .ge.(2.*hcal_new_rms_pos(blk))) then
          ind = ind + 1
          hcal_changed_block(ind)=blk
          hcal_changed_sign(ind)=1         ! 1=pos,2=neg.
          hcal_ped_change(ind)=hcal_new_ped_pos(blk)-
     &                         hcal_pos_ped_mean(blk) 
        endif
        

        if (num.gt.hcal_min_peds .and. hcal_min_peds.ne.0) then
          hcal_pos_ped_mean(blk)=hcal_new_ped_pos(blk)
          hcal_pos_ped_rms(blk)=hcal_new_rms_pos(blk)
          hcal_pos_threshold(blk)=min(50.,max(10.,3.*hcal_new_rms_pos(blk)))
        endif
        
*do it all again for negative tubes.
       num=max(1.,float(hcal_neg_ped_num(blk)))
        hcal_new_ped_neg(blk)=hcal_neg_ped_sum(blk)/num
        sig2 = float(hcal_neg_ped_sum2(blk))/num-hcal_new_ped_neg(blk)**2
        hcal_new_rms_neg(blk)=sqrt(max(0.,sig2))
        hcal_new_adc_threshold_neg(blk)=hcal_new_ped_neg(blk)+15.
        if (abs(hcal_neg_ped_mean(blk)-hcal_new_ped_neg(blk))
     &                 .ge.(2.*hcal_new_rms_neg(blk))) then
          ind = ind + 1
          hcal_changed_block(ind)=blk
          hcal_changed_sign(ind)=2         ! 1=pos,2=neg.
          hcal_ped_change(ind)=hcal_new_ped_neg(blk)-
     &                         hcal_neg_ped_mean(blk) 
        endif
  
        if (num.gt.hcal_min_peds .and. hcal_min_peds.ne.0) then
          hcal_neg_ped_mean(blk)=hcal_new_ped_neg(blk)
          hcal_neg_ped_rms(blk)=hcal_new_rms_neg(blk)
          hcal_neg_threshold(blk)=min(50.,max(10.,3.*hcal_new_rms_neg(blk)))
        endif
  
      enddo
      hcal_num_ped_changes = ind

*
* GAS CERENKOV PEDESTALS
*
      ind = 0
      do pmt = 1 , hmax_cer_hits
        num=max(1.,float(hcer_ped_num(pmt)))
        hcer_new_ped(pmt) = float(hcer_ped_sum(pmt)) / num
        sig2 = float(hcer_ped_sum2(pmt))/ num - hcer_new_ped(pmt)**2
        hcer_new_rms(pmt) = sqrt(max(0.,sig2))
        hcer_new_adc_threshold(pmt)=hcer_new_ped(pmt)+15.
        if (abs(hcer_ped(pmt)-hcer_new_ped(pmt))
     &                 .ge.(2.*hcer_new_rms(pmt))) then
          ind = ind + 1
          hcer_changed_tube(ind)=pmt
          hcer_ped_change(ind)=hcer_new_ped(pmt)-hcer_ped(pmt) 
        endif
        if (num.gt.hcer_min_peds .and. hcer_min_peds.ne.0) then
          hcer_ped(pmt)=hcer_new_ped(pmt)
          hcer_ped_rms(pmt)=hcer_new_rms(pmt)
        endif
      enddo
      hcer_num_ped_changes = ind
*........................................................................
*
*
* AEROGEL CERENKOV PEDESTALS
*
*
      ind = 0
      do pmt = 1 , hmax_aero_hits

*calculate new pedestal values, positive tubes first
        num=max(1.,float(haero_pos_ped_num(pmt)))
        haero_new_ped_pos(pmt) = float(haero_pos_ped_sum(pmt)) / num
        sig2 = float(haero_pos_ped_sum2(pmt))/num - 
     &            haero_new_ped_pos(pmt)**2
        haero_new_rms_pos(pmt) = sqrt(max(0.,sig2))
        haero_new_threshold_pos(pmt) = haero_new_ped_pos(pmt)+15.
c
        haero_new_threshold_pos(pmt) = 400. ! mkj 4/9/03 force to 400 
c
*note channels with 2 sigma difference from parameter file values.
* JRA - don't have the necessary variables (e.g. haero_all_ped_pos),
* and as far as I can tell, this code doesn't work for any detector,
* since the h*_all_ped* variables are not filled as far as I can tell

*replace old peds with calculated peds
        if (num.gt.haero_min_peds .and. haero_min_peds.ne.0) then
           haero_pos_ped_mean(pmt) = haero_new_ped_pos(pmt)
           haero_pos_ped_rms(pmt) = haero_new_rms_pos(pmt)
        endif

*do it all again for negative tubes.
        num=max(1.,float(haero_neg_ped_num(pmt)))
        haero_new_ped_neg(pmt) = float(haero_neg_ped_sum(pmt)) / num
        sig2 = float(haero_neg_ped_sum2(pmt))/num - 
     &            haero_new_ped_neg(pmt)**2
        haero_new_rms_neg(pmt) = sqrt(max(0.,sig2))
        haero_new_threshold_neg(pmt) = haero_new_ped_neg(pmt)+15.
c
        haero_new_threshold_neg(pmt) = 400. ! mkj 4/9/03 force to 400 
 
        if (num.gt.haero_min_peds .and. haero_min_peds.ne.0) then
          haero_neg_ped_mean(pmt) = haero_new_ped_neg(pmt)
          haero_neg_ped_rms(pmt) = haero_new_rms_neg(pmt)
        endif

      enddo

*      print *, ' '
*      print *, 'haero_pos_ped_mean =', haero_neg_ped_mean
*      print *, ' '
*      print *, 'haero_neg_ped_mean =', haero_pos_ped_mean
*      print *, ' '
*      print *, 'haero_pos_adc_threshold =', haero_new_threshold_pos
*      print *, ' '
*      print *, 'haero_neg_adc_threshold =', haero_new_threshold_neg
*      print *, ' '

*.........................................................................
*
* MISC. PEDESTALS
*
      ind = 0
      do ihit = 1 , hmax_misc_hits
        if (hmisc_raw_addr1(ihit).eq.2) then     !  ADC data.
          imisc = hmisc_raw_addr2(ihit)
          num=max(1.,float(hmisc_ped_num(imisc)))
          hmisc_new_ped(imisc) = float(hmisc_ped_sum(imisc)) / num
          sig2 = float(hmisc_ped_sum2(imisc))/ num - hmisc_new_ped(imisc)**2
          hmisc_new_rms(imisc) = sqrt(max(0.,sig2))
          hmisc_new_adc_threshold(imisc)=hmisc_new_ped(imisc)+15.
          if (abs(hmisc_ped(imisc)-hmisc_new_ped(imisc))
     &                 .ge.(2.*hmisc_new_rms(imisc))) then
            ind = ind + 1
            hmisc_changed_tube(ind)=imisc
            hmisc_ped_change(ind)=hmisc_new_ped(imisc)-hmisc_ped(imisc)
          endif
          if (num.gt.hmisc_min_peds .and. hmisc_min_peds.ne.0) then
            hmisc_ped(imisc)=hmisc_new_ped(imisc)
            hmisc_ped_rms(imisc)=hmisc_new_rms(imisc)
          endif
        endif    !chose ADC hits.
      enddo
      hmisc_num_ped_changes = ind

*
*
* WRITE THRESHOLDS TO FILE FOR HARDWARE SPARCIFICATION
*
      if (h_threshold_output_filename.ne.' ') then

* file opened in g_calc_beam_pedestal.f -- if needed
!        file=h_threshold_output_filename
!        call g_sub_run_number(file, gen_run_number)
!        open(unit=SPAREID,file=file,status='unknown')


 
        write(SPAREID,*) '# This is the ADC threshold file generated automatically'
        write(SPAREID,*) '# from the pedestal data from run number ',gen_run_number
        write(SPAREID,*) '# Slot 13 (beamline stuff) is hardwired in h_calc_pedestal.f'
        write(SPAREID,*) 'slot=  13'
        do ind=1,16
           write(SPAREID,*) '    0'    !BPM and raster stuff (4blank,2x4BPM,4raster)
        enddo
        do ind=17,20
           write(SPAREID,*) '    0'    !?? "Paul Gueye" cable (4BPM)
        enddo
        do ind=21,32
           write(SPAREID,*) ' 4000'    !empty
        enddo
        do ind=33,64
           write(SPAREID,*) ' 4000'
        enddo




        roc=1

        slot=1
        signalcount=2
        write(SPAREID,*) 'slot=',slot
        call g_output_thresholds(SPAREID,roc,slot,signalcount,hmax_cal_rows,
     &      hcal_new_adc_threshold_pos,hcal_new_adc_threshold_neg,
     &      hcal_new_rms_pos,hcal_new_rms_neg)

*
* JRA - 4/8/03 - Gaskell says he want's unsparsified gas cerernkov for
* the '03 running:

        slot=3
        write(SPAREID,*) 'slot=',slot
        do ind=1,2
          write(SPAREID,'(a6)') '     0'
        enddo
        do ind=3,64
          write(SPAREID,'(a6)') '  4000'
        enddo

*        slot=3
*        signalcount=1
*        write(SPAREID,*) 'slot=',slot
*        call g_output_thresholds(SPAREID,roc,slot,signalcount,hmax_cer_hits,
*     &      hcer_new_adc_threshold,0,hcer_new_rms,0)

        slot=5
        signalcount=2
        write(SPAREID,*) 'slot=',slot
        call g_output_thresholds(SPAREID,roc,slot,signalcount,hmax_cal_rows,
     &      hcal_new_adc_threshold_pos,hcal_new_adc_threshold_neg,
     &      hcal_new_rms_pos,hcal_new_rms_neg)

        slot=7
        signalcount=2
        write(SPAREID,*) 'slot=',slot
        call g_output_thresholds(SPAREID,roc,slot,signalcount,hnum_scin_planes,
     &      hhodo_new_threshold_pos,hhodo_new_threshold_neg,hhodo_new_sig_pos,
     &      hhodo_new_sig_neg)

        slot=9
        signalcount=2
        write(SPAREID,*) 'slot=',slot
        call g_output_thresholds(SPAREID,roc,slot,signalcount,hnum_scin_planes,
     &      hhodo_new_threshold_pos,hhodo_new_threshold_neg,hhodo_new_sig_pos,
     &      hhodo_new_sig_neg)

        slot=11
        signalcount=1
        write(SPAREID,*) 'slot=',slot
        call g_output_thresholds(SPAREID,roc,slot,signalcount,hmax_aero_hits,
     &       haero_new_threshold_pos,haero_new_threshold_neg,haero_pos_ped_rms,
     &       haero_neg_ped_rms)


        close(unit=SPAREID)
      endif

      return
      end
