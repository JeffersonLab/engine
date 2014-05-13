       SUBROUTINE H_AERO(ABORT,err)
*-
* $Log: h_aero.f,v $
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

!!! TH - haero_tot_hits = hmax_hits = 8 
!!! which is the number of PMT's on each side
!!! 
!!! TH  - we are looping over the number of PMTs here. 
!!! Note: haero_pos_ped_mean is identical with haero_pos_ped_sum/num
!!! as calculated in h_calc_pedestal.f

      do ihit = 1,haero_tot_hits

* pedestal subtraction and gain adjustment

* An ADC value of less than zero occurs when that particular
* channel has been sparsified away and has not been read. 
* The NPE for that tube will be assigned zero by this code.
* An ADC value of greater than 8192 occurs when the ADC overflows on
* an input that is too large. Tubes with this characteristic will
* be assigned NPE = 100.0.

!!! TH - For ADC value less than zero assume what is said above is true
!!!      and assign haero_npe=0 for these pmts.

!!! TH - Make this algorithm even more identical to the cerenkov
!!!      npe calculation by including a test on the pedestal width
!!!      Note: haero_pos_ped_rms is the calculated pedestal width - an
!!!            array of size hmax_aero_hits (8)

        npmt=haero_pair_num(ihit)
           if(haero_adc_pos(ihit).gt.0) then
              if (haero_adc_pos(ihit).lt.8000.) then
                if (haero_adc_pos(ihit).gt.haero_pos_ped_rms(ihit)) then
                    haero_pos_npe(npmt) = haero_pos_gain(npmt) *
     >              (haero_adc_pos(ihit)-haero_pos_ped_mean(npmt))
                    haero_adc_pos_hits = haero_adc_pos_hits + 1
                    haero_tot_good_hits = haero_tot_good_hits + 1
                 endif
              else
                 haero_pos_npe(npmt) = 100.
              endif
           else
! TH - Check default value for ADC info if PMT not there
!              write(*,*) '$$$$ ',haero_adc_pos(ihit),
!     >                           haero_pos_ped_mean(npmt)
                 haero_pos_npe(npmt)=0.
           endif
           if(haero_adc_neg(ihit).gt.0) then
              if (haero_adc_neg(ihit).lt.8000.) then
                if (haero_adc_neg(ihit).gt.haero_neg_ped_rms(ihit)) then
                    haero_neg_npe(npmt) = haero_neg_gain(npmt) * 
     >              (haero_adc_neg(ihit)-haero_neg_ped_mean(npmt))
                    haero_adc_neg_hits = haero_adc_neg_hits + 1
                    haero_tot_good_hits = haero_tot_good_hits + 1
                 endif
              else
                 haero_neg_npe(npmt) = 100.
              endif
           else
                 haero_neg_npe(npmt) = 0.
           endif
c
          
        haero_pos_npe_sum = haero_pos_npe_sum + haero_pos_npe(npmt)
        haero_neg_npe_sum = haero_neg_npe_sum + haero_neg_npe(npmt)

*
        if (haero_tdc_pos(npmt).ge.0.and.haero_tdc_pos(npmt).le.8000.)
     &       haero_tdc_pos_hits = haero_tdc_pos_hits + 1 
        
        if (haero_tdc_neg(npmt).ge.0.and.haero_tdc_neg(npmt).le.8000.)
     &       haero_tdc_neg_hits = haero_tdc_neg_hits + 1

      enddo

!!! TH - sum everything since already take into account adc 
!!       values less than zero above.

      haero_npe_sum = haero_neg_npe_sum + haero_pos_npe_sum


* If the total hits are 0, then give a noticable ridiculous NPE.

      if (haero_tot_hits.lt.1) then
         haero_npe_sum=-10.0
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

