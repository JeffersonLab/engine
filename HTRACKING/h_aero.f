       SUBROUTINE H_AERO(ABORT,err)
*-
* $Log$
* Revision 1.1.2.2  2003/04/09 02:46:11  cdaq
* Update variable names for the thresholds to match the modified common block
*
* Revision 1.1.2.1  2003/04/06 06:20:40  cdaq
* updated variables for haero, cleaned up a few of the tests
*
* Revision 1.1  2002/12/20 21:54:29  jones
* New files by Hamlet for new HMS aerogel
*
*
* Revision 1.1  2002/10/21 (Hamlet)
* Initial revision
*
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*8 here
      parameter (here= 'H_AERO')
*
      logical ABORT
      character*(*) err
*
     
      integer*4 ind,npmt
*
      INCLUDE 'hms_data_structures.cmn'
      INCLUDE 'hms_pedestals.cmn'
      INCLUDE 'hms_aero_parms.cmn'

*
*--------------------------------------------------------
*
      ABORT= .FALSE.
      err= ' '

      haero_neg_npe_sum = 0.0
      haero_pos_npe_sum = 0.0
      haero_npe_sum = 0.0

      aero_pos = 0.0
      aero_neg = 0.0
      aero_tot = 0.0

      haero_tot_good_hits = 0
      haero_adc_pos_hits = 0
      haero_adc_neg_hits = 0
      haero_tdc_pos_hits = 0
      haero_tdc_neg_hits = 0

      do ind = 1,hmax_aero_hits

        haero_pos_npe(ind)=0.
        haero_neg_npe(ind)=0.

      enddo

      do ind = 1,haero_tot_hits

* pedestal subtraction and gain adjustment

* An ADC value of less than zero occurs when that particular
* channel has been sparsified away and has not been read. 
* The NPE for that tube will be assigned zero by this code.
* An ADC value of greater than 8192 occurs when the ADC overflows on
* an input that is too large. Tubes with this characteristic will
* be assigned NPE = 100.0.

        npmt=haero_pair_num(ind)

        if (haero_adc_pos(ind).gt.haero_new_threshold_pos(npmt)) then
           if (haero_adc_pos(ind).lt.8000.) then
              haero_pos_npe(npmt) = haero_pos_gain(npmt) *
     &             (haero_adc_pos(ind)-haero_pos_ped_mean(npmt))
           else
              haero_pos_npe(npmt) = 100.
           endif
        endif
        
        if (haero_adc_neg(ind).gt.haero_new_threshold_pos(npmt)) then
           if (haero_adc_neg(ind).lt.8000.) then
              haero_neg_npe(npmt) = haero_neg_gain(npmt) * 
     &             (haero_adc_neg(ind)-haero_neg_ped_mean(npmt))
           else
              haero_neg_npe(npmt) = 100.
           endif
        endif
        haero_pos_npe_sum = haero_pos_npe_sum + haero_pos_npe(npmt)
        haero_neg_npe_sum = haero_neg_npe_sum + haero_neg_npe(npmt)

*
* sum positive and negative hits 
* To fill haero_tot_good_hits

        if (haero_pos_npe(npmt).ge.0.1) then
            haero_adc_pos_hits = haero_adc_pos_hits + 1
            haero_tot_good_hits = haero_tot_good_hits + 1
        endif

        if (haero_neg_npe(npmt).ge.0.1) then
            haero_adc_neg_hits = haero_adc_neg_hits + 1
            haero_tot_good_hits = haero_tot_good_hits + 1
        endif

        if (haero_tdc_pos(npmt).ge.0.and.haero_tdc_pos(npmt).le.8000.)
     &       haero_tdc_pos_hits = haero_tdc_pos_hits + 1 
        
        if (haero_tdc_neg(npmt).ge.0.and.haero_tdc_neg(npmt).le.8000.)
     &       haero_tdc_neg_hits = haero_tdc_neg_hits + 1

      enddo

      haero_npe_sum = haero_neg_npe_sum + haero_pos_npe_sum

* Next, fill the rawadc variables with the actual tube values
*	mainly for diagnostic purposes.

      do ind=1,haero_tot_hits

         npmt=haero_pair_num(ind)

         haero_rawadc_pos(npmt)=haero_adc_pos(ind)
         aero_ep(npmt)=haero_rawadc_pos(ind)        

         haero_rawadc_neg(npmt)=haero_adc_neg(ind)
         aero_en(npmt)=haero_rawadc_neg(ind)

         haero_rawtdc_neg(npmt)=haero_tdc_neg(ind)
         aero_tn(npmt)= haero_tdc_neg(ind)

         haero_rawtdc_pos(npmt)=haero_tdc_pos(ind)
         aero_tp(npmt)= haero_tdc_pos(ind)

      enddo

      return
      end



