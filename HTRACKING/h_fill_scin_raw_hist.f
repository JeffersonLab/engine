
      subroutine h_fill_scin_raw_hist(Abort,err)
*
*     routine to fill histograms with hms_raw_scin varibles
*     In the future ID numbers are stored in hms_histid
*
*     Author:	D. F. Geesaman
*     Date:     4 April 1994
*
*     Modified  9 April 1994     DFG
*                                Add CTP flag to turn on histogramming
*                                id's in hms_id_histid
* $Log$
* Revision 1.5  1995/05/22 19:39:12  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.4  1995/05/11  19:04:53  cdaq
* (JRA) Modifications to user histograms
*
* Revision 1.3  1995/02/10  19:03:46  cdaq
* (JRA) Change hscin_num_counters to hnum_scin_counters
*
* Revision 1.2  1995/02/02  13:13:46  cdaq
* (JRA) Make hscin_all_adc_pos/neg floating
*
* Revision 1.1  1994/04/13  20:08:03  cdaq
* Initial revision
*
*--------------------------------------------------------
      IMPLICIT NONE
*
      external thgetid
      integer*4 thgetid
      character*50 here
      parameter (here= 'h_fill_scin_raw_hist_')
*
      logical ABORT
      character*(*) err
      real*4  histval
      integer*4 plane,counter,ihit
      include 'hms_data_structures.cmn'
      include 'hms_scin_parms.cmn'
      include 'hms_id_histid.cmn'          
*
      SAVE
*--------------------------------------------------------
*
      ABORT= .FALSE.
      err= ' '
*
        histval = HSCIN_TOT_HITS
        call hf1(hidscinrawtothits,histval,1.)
* Make sure there is at least 1 hit
        if(HSCIN_TOT_HITS .gt. 0 ) then
* Loop over all hits
         do ihit=1,HSCIN_TOT_HITS
           plane=HSCIN_PLANE_NUM(ihit)
           counter=HSCIN_COUNTER_NUM(ihit)
* Fill plane map                  
           histval = FLOAT(HSCIN_PLANE_NUM(ihit))
           call hf1(hidscinplane,histval,1.)
* Check for valid plane
           if(plane.gt.0 .and. plane .le. hnum_scin_planes) then
* Fill counter map
             histval = float(counter)
             call hf1(hidscincounters(plane),histval,1.)
* Fill ADC and TDC histograms
             if((counter .gt. 0) .and.
     $            (counter.le.hnum_scin_counters(plane)))then
               
               if (hscin_tdc_pos(ihit).ne.-1) then
                 histval = counter
                 call hf1(hidscinallpostdc(plane),histval,1.)
               endif
               if (hscin_tdc_neg(ihit).ne.-1) then
                 histval = counter
                 call hf1(hidscinallnegtdc(plane),histval,1.)
               endif
               if (hscin_adc_pos(ihit).ge.50) then
                 histval = counter
                 call hf1(hidscinallposadc(plane),histval,1.)
               endif
               if (hscin_adc_neg(ihit).ge.50) then
                 histval = counter
                 call hf1(hidscinallnegadc(plane),histval,1.)
               endif

* Do we want to histogram raw scintillators
              if(hturnon_scin_raw_hist .ne. 0 ) then
                histval = HSCIN_ADC_POS(ihit)
                call hf1(hidscinposadc(plane,counter),histval,1.)
                histval = HSCIN_ADC_NEG(ihit)
                call hf1(hidscinnegadc(plane,counter),histval,1.)
                histval = FLOAT(HSCIN_TDC_POS(ihit))
                call hf1(hidscinpostdc(plane,counter),histval,1.)
                histval = FLOAT(HSCIN_TDC_NEG(ihit))
                call hf1(hidscinnegtdc(plane,counter),histval,1.)
              endif                     ! end test on histogramming flag
            endif                       ! end test on valid counter number
          endif                         ! end test on valid plane number
        enddo                           ! end loop over hits
      endif                             ! end test on zero hits       

      RETURN
      END

