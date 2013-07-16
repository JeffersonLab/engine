       SUBROUTINE S_AERO(ABORT,err)
*--------------------------------------------------------
*-
*-   Purpose and Methods : Analyze aerogel information for each track
*-
*-      Required Input BANKS     SOS_RAW_AER
*-
*-      Output BANKS             SOS_TRACK_TESTS
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*-
*-   Created 13-MAY-1995     H. Breuer and R. Mohring, UMD
*-                           SOS Aerogel detector calibration routine
*-
* $Log: s_aero.f,v $
* Revision 1.3  1996/11/07 19:48:28  saw
* (JRA) Handle over and underflows
*
* Revision 1.2  1996/09/05 13:13:14  saw
* (JRA) ??
*
* Revision 1.1  1996/04/30 17:12:40  saw
* Initial revision
*
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*6 here
      parameter (here= 'S_AERO')
*
      logical ABORT
      character*(*) err
*
      integer*4 ind,npmt
*
      INCLUDE 'sos_data_structures.cmn'
      INCLUDE 'sos_pedestals.cmn'
      INCLUDE 'sos_aero_parms.cmn'
      INCLUDE 'sos_scin_parms.cmn'  ! For smisc_dec_data bank
*
*--------------------------------------------------------
*
      ABORT= .FALSE.
      err= ' '

      saer_neg_npe_sum = 0.0
      saer_pos_npe_sum = 0.0
      saer_tot_good_hits = 0

      do ind = 1,smax_aer_hits
        saer_pos_npe(ind)=0.0
        saer_neg_npe(ind)=0.0
      enddo

      do ind = 1,saer_tot_hits
* pedestal subtraction and gain adjustment
* Note: rmm 28-aug-96
* An ADC value of less than zero occurs when that particular
* channel has been sparsified away and has not been read. The NPE
* for that tube will be assigned zero by this code (the NPE
* variables are initialized above.) An ADC value of greater than
* 8000 (more specifically, 8192) occurs when the ADC overflows on
* an input that is too large. Tubes with this characteristic will
* be assigned NPE = 100.0.

        npmt=saer_pair_num(ind)

       if (saer_adc_pos(ind).gt.0) then
         if (saer_adc_pos(ind).lt.8000) then
           saer_pos_npe(npmt) =
     &       (saer_adc_pos(ind)-saer_pos_ped_mean(npmt))*saer_pos_gain(npmt)
          else
           saer_pos_npe(npmt) = 100.0
         endif                         ! pos.lt.8000
       endif                           ! gt.0

        if (saer_adc_neg(ind).gt.0) then
          if (saer_adc_neg(ind).lt.8000) then
           saer_neg_npe(npmt) =
     &       (saer_adc_neg(ind)-saer_neg_ped_mean(npmt))*saer_neg_gain(npmt)
          else
            saer_neg_npe(npmt) = 100.0
          endif                         ! neg.lt.8000
        endif                           ! gt.0

* sum positive and negative hits if above software threshold
* also, fill saer_tot_good_hits for those events that have
* more than 0.3 npe (mostly cuts out scintillation)

        if (saer_neg_npe(npmt).ge.saer_neg_threshold(npmt)) then
          saer_neg_npe_sum = saer_neg_npe_sum + saer_neg_npe(npmt)
         if (saer_neg_npe(npmt).ge.0.3)
     &         saer_tot_good_hits = saer_tot_good_hits + 1
        endif
        if (saer_pos_npe(npmt).ge.saer_pos_threshold(npmt)) then
          saer_pos_npe_sum = saer_pos_npe_sum + saer_pos_npe(npmt)
         if (saer_pos_npe(npmt).ge.0.3)
     &         saer_tot_good_hits = saer_tot_good_hits + 1
        endif
      enddo

      saer_npe_sum = saer_neg_npe_sum + saer_pos_npe_sum

* If the total hits are 0, then make sure NPE=0
      if (saer_tot_hits.lt.1) then
       saer_npe_sum=0.
      endif


* Next, fill the rawadc variables with the actual tube values
*      mainly for diagnostic purposes.

      do ind=1,7
       saer_rawadc_neg(ind)=-100
       saer_rawadc_pos(ind)=-100
      enddo

      do ind=1,saer_tot_hits
       npmt=saer_pair_num(ind)
        saer_rawadc_neg(npmt)=saer_adc_neg(ind)
        saer_rawadc_pos(npmt)=saer_adc_pos(ind)
      enddo

*RMM added 28-aug-96 summing amp outputs:
      saer_sumA = smisc_dec_data(17,2)
      saer_sumB = smisc_dec_data(18,2)

      return
      end
