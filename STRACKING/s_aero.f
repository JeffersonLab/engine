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
* $Log$
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
      integer*4 ind
*
      INCLUDE 'sos_data_structures.cmn'
      INCLUDE 'sos_pedestals.cmn'
      INCLUDE 'sos_aero_parms.cmn'
*
*--------------------------------------------------------
*
      ABORT= .FALSE.
      err= ' '

      saer_neg_npe_sum = 0.0
      saer_pos_npe_sum = 0.0
      saer_tot_good_hits = 0

      do ind = 1,saer_tot_hits
* pedestal subtraction and gain adjustment
        saer_pos_npe(ind) =
     &     (saer_adc_pos(ind)-saer_pos_ped_mean(ind))*saer_pos_gain(ind)
        saer_neg_npe(ind) =
     &     (saer_adc_neg(ind)-saer_neg_ped_mean(ind))*saer_neg_gain(ind)

* sum positive and negative hits if above threshold
        if (saer_neg_npe(ind).ge.saer_neg_threshold(ind)) then
          saer_neg_npe_sum = saer_neg_npe_sum + saer_neg_npe(ind)
          saer_tot_good_hits = saer_tot_good_hits + 1
        endif
        if (saer_pos_npe(ind).ge.saer_pos_threshold(ind)) then
          saer_pos_npe_sum = saer_pos_npe_sum + saer_pos_npe(ind)
          saer_tot_good_hits = saer_tot_good_hits + 1
        endif

      enddo

      saer_npe_sum = saer_neg_npe_sum + saer_pos_npe_sum

      return
      end
