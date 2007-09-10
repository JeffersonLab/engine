       SUBROUTINE H_AERO(ABORT,err)
*-
* $Log$
* Revision 1.4.14.1  2007/09/10 20:28:00  pcarter
* Implemented changes to allow compilation on RHEL 3,4,5 and MacOSX
*
* Revision 1.4  2004/02/27 14:35:20  jones
* Summed npe is incremented for all pmts regardless of whether they are
* above or below the pedestal.  Summing only pmts above the pedestal results
* in an incorrect Summed npe for low beta particles.  Consult the Fpi-2
* calibration notes for details. ( G. Huber)
*
* Revision 1.3  2004/02/02 19:23:55  jones
* 1) When filling haero_adc_pos_hits,haero_tot_good_hits,haero_adc_neg_hits
* changed cut on npe from 0.1 to 0.3 .
* 2) When filling haero_npe_sum changed cut on npe_sum from 0.1 to 0.5
*
* Revision 1.2  2003/09/05 16:48:01  jones
* Merge in online03 changes (mkj)
*
* Revision 1.1.2.5  2003/07/28 18:01:38  cdaq
* Use haero_new_ped_pos and haero_new_ped_neg instead of aero_new_threshold_neg
* and haero_new_threshold_pos in IF statement (mkj)
*
* Revision 1.1.2.4  2003/07/18 18:22:49  cdaq
* Fix bug that haero_adc_neg was compared to  instead
* of haero_new_threshold_neg (Vardan)
*
* Revision 1.1.2.3  2003/04/15 21:47:35  cdaq
* Changed ind to ihit for better readability
* add checks on haero_npe_sum  (MKJ)
*
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
     
      integer*4 ihit,npmt
*
      INCLUDE 'hms_data_structures.cmn'
      INCLUDE 'hms_pedestals.cmn'
      INCLUDE 'hms_aero_parms.cmn'


*
*--------------------------------------------------------
*

c      print*,'h_aero: haero_pos_gain =',haero_pos_gain
c      print*,'h_aero: haero_neg_gain =',haero_neg_gain
c      pause

      ABORT= .FALSE.
      err= ' '

      haero_neg_npe_sum = 0.0
      haero_pos_npe_sum = 0.0
      haero_npe_sum = 0.0

***      aero_pos = 0.0            !not in use any more
***      aero_neg = 0.0            !not in use any more
***      aero_tot = 0.0            !not in use any more

      haero_tot_good_hits = 0
      haero_adc_pos_hits = 0
      haero_adc_neg_hits = 0
      haero_tdc_pos_hits = 0
      haero_tdc_neg_hits = 0

      do ihit = 1,hmax_aero_hits

        haero_pos_npe(ihit)=0.
        haero_neg_npe(ihit)=0.

      enddo



      do ihit = 1,haero_tot_hits

* pedestal subtraction and gain adjustment

* An ADC value of less than zero occurs when that particular
* channel has been sparsified away and has not been read. 
* The NPE for that tube will be assigned zero by this code.
* An ADC value of greater than 8192 occurs when the ADC overflows on
* an input that is too large. Tubes with this characteristic will
* be assigned NPE = 100.0.

        npmt=haero_pair_num(ihit)

           if (haero_adc_pos(ihit).lt.8000.) then
              haero_pos_npe(npmt) = haero_pos_gain(npmt) *
     &             (haero_adc_pos(ihit)-haero_pos_ped_mean(npmt))
           else
              haero_pos_npe(npmt) = 100.
           endif

           if (haero_adc_neg(ihit).lt.8000.) then
              haero_neg_npe(npmt) = haero_neg_gain(npmt) * 
     &             (haero_adc_neg(ihit)-haero_neg_ped_mean(npmt))
           else
              haero_neg_npe(npmt) = 100.
           endif
c
        haero_pos_npe_sum = haero_pos_npe_sum + haero_pos_npe(npmt)
        haero_neg_npe_sum = haero_neg_npe_sum + haero_neg_npe(npmt)

*



*
* sum positive and negative hits 
* To fill haero_tot_good_hits

        if (haero_pos_npe(npmt).ge.0.3) then
            haero_adc_pos_hits = haero_adc_pos_hits + 1
            haero_tot_good_hits = haero_tot_good_hits + 1
        endif

        if (haero_neg_npe(npmt).ge.0.3) then
            haero_adc_neg_hits = haero_adc_neg_hits + 1
            haero_tot_good_hits = haero_tot_good_hits + 1
        endif

        if (haero_tdc_pos(npmt).ge.0.and.haero_tdc_pos(npmt).le.8000.)
     &       haero_tdc_pos_hits = haero_tdc_pos_hits + 1 
        
        if (haero_tdc_neg(npmt).ge.0.and.haero_tdc_neg(npmt).le.8000.)
     &       haero_tdc_neg_hits = haero_tdc_neg_hits + 1

      enddo

      if (haero_neg_npe_sum.ge.0.5.or.haero_pos_npe_sum.ge.0.5) then
         haero_npe_sum = haero_neg_npe_sum + haero_pos_npe_sum
      else
         haero_npe_sum = 0.0
      endif

* If the total hits are 0, then give a noticable ridiculous NPE.

      if (haero_tot_hits.lt.1) then

         haero_npe_sum=0.0
      endif



* Next, fill the rawadc variables with the actual tube values
*       mainly for diagnostic purposes.

      do ihit=1,haero_tot_hits

         npmt=haero_pair_num(ihit)

         haero_rawadc_pos(npmt)=haero_adc_pos(ihit)
         aero_ep(npmt)=haero_rawadc_pos(ihit)        

         haero_rawadc_neg(npmt)=haero_adc_neg(ihit)
         aero_en(npmt)=haero_rawadc_neg(ihit)

         haero_rawtdc_neg(npmt)=haero_tdc_neg(ihit)
         aero_tn(npmt)= haero_tdc_neg(ihit)

         haero_rawtdc_pos(npmt)=haero_tdc_pos(ihit)
         aero_tp(npmt)= haero_tdc_pos(ihit)

      enddo

      return
      end

