       SUBROUTINE S_LUCITE(ABORT,err)
*--------------------------------------------------------
*-
*-   Purpose and Methods : Analyze lucite aerogel information for each track
*-
*-      Required Input BANKS     SOS_RAW_LUC
*-
*-      Output BANKS             SOS_TRACK_TESTS
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*-
* $Log: s_lucite.f,v $
* Revision 1.1  1996/11/07 19:50:56  saw
* Initial revision
*
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*8 here
      parameter (here= 'S_LUCITE')
*
      logical ABORT
      character*(*) err
*
     
      integer*4 ind,npmt
*
      INCLUDE 'sos_data_structures.cmn'
      INCLUDE 'sos_pedestals.cmn'
      INCLUDE 'sos_lucite_parms.cmn'

*
*--------------------------------------------------------
*
      ABORT= .FALSE.
      err= ' '

      sluc_neg_npe_sum = 0.0
      sluc_pos_npe_sum = 0.0
      sluc_tot_good_hits = 0

      do ind = 1,smax_luc_hits
        sluc_pos_npe(ind)=0.
        sluc_neg_npe(ind)=0.
      enddo

      do ind = 1,sluc_tot_hits
* pedestal subtraction and gain adjustment
        npmt=sluc_pair_num(ind)
        sluc_pos_npe(npmt) =
     &     (sluc_adc_pos(ind)-sluc_pos_ped_mean(npmt))*sluc_pos_gain(npmt)
        sluc_neg_npe(npmt) =
     &     (sluc_adc_neg(ind)-sluc_neg_ped_mean(npmt))*sluc_neg_gain(npmt)

* sum positive and negative hits 
* also, fill sluc_tot_good_hits

          sluc_neg_npe_sum = sluc_neg_npe_sum + sluc_neg_npe(npmt)
	  if (sluc_neg_npe(npmt).ge.1.0) 
     &		sluc_tot_good_hits = sluc_tot_good_hits + 1


          sluc_pos_npe_sum = sluc_pos_npe_sum + sluc_pos_npe(npmt)
	  if (sluc_pos_npe(npmt).ge.0.3) 
     &          sluc_tot_good_hits = sluc_tot_good_hits + 1
      enddo


      sluc_npe_sum = sluc_neg_npe_sum + sluc_pos_npe_sum

* If the total hits are 0, then give a noticable ridiculous NPE.

      if (sluc_tot_hits.lt.1) then
	sluc_npe_sum=100.
      endif


* Next, fill the rawadc variables with the actual tube values
*	mainly for diagnostic purposes.

      do ind=1,8
	sluc_rawadc_neg(ind)=-100
	sluc_rawadc_pos(ind)=-100
      enddo

      do ind=1,sluc_tot_hits
	npmt=sluc_pair_num(ind)
        sluc_rawadc_neg(npmt)=sluc_adc_neg(ind)
        sluc_rawadc_pos(npmt)=sluc_adc_pos(ind)
      enddo

      return
      end

