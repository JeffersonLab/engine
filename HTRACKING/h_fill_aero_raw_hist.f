      subroutine h_fill_aero_raw_hist(Abort,err)
*
*     routine to fill aerogel raw data histograms and hit pattern
*
* Revision 1.0  2002/10/05 (Hamlet)
* Initial version
*
*
*--------------------------------------------------------
      IMPLICIT NONE
*
      external thgetid
      integer*4 thgetid
      character*20 here
      parameter (here='h_fill_aero_raw_hist')
*
      logical ABORT
      character*(*) err

      real*4 histval
      integer*4 ind


      include 'hms_data_structures.cmn'
      include 'hms_aero_parms.cmn'
      include 'hms_pedestals.cmn'
      include 'hms_id_histid.cmn'          
*
      SAVE
*--------------------------------------------------------
*
      ABORT= .FALSE.
      err= ' '

* Make sure there is at least 1 hit
      if(hmax_aero_hits .LT. 1 ) then
         return
      endif
* Loop over all PMT's

*      if(haero_tot_hits .gt. 0 ) then
        do ind=1,HNUM_AERO_BLOCKS
           
* Fill ADC hits

          if(haero_pos_npe(ind).gt.0.1) then
           histval=float(ind)
           call hf1(hidhaero_adc_pos_hits,histval,1.)
           endif

          if(haero_neg_npe(ind).gt.0.1) then 
           histval=float(ind)
           call hf1(hidhaero_adc_neg_hits,histval,1.)
           endif

* Fill TDC hits

          if(haero_rawtdc_pos(ind).gt.0.and.haero_rawtdc_pos(ind).le.8000.) then
           histval=float(ind) 
           call hf1(hidhaero_tdc_pos_hits,histval,1.)
           endif

          if(haero_rawtdc_neg(ind).gt.0.and.haero_rawtdc_neg(ind).le.8000.) then
           histval=float(ind)  
           call hf1(hidhaero_tdc_neg_hits,histval,1.)
         endif
        enddo

      return
      end
