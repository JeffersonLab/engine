
      subroutine s_fill_scin_raw_hist(Abort,err)
*
*     routine to fill histograms with sos_raw_scin varibles
*
*     Author:	D. F. Geesaman
*     Date:     4 April 1994
*
*     Modified  9 April 1994     DFG
*                                Add CTP flag to turn on histogramming
*                                id's in sos_id_histid
* $Log$
* Revision 1.4  1995/05/22 19:45:39  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.3  1995/05/11  21:04:14  cdaq
* (JRA) Modifications to user histograms
*
* Revision 1.2  1995/02/10  19:11:36  cdaq
* (JRA) Change sscin_num_counters to snum_scin_counters
*
* Revision 1.1  1994/04/13  20:07:48  cdaq
* Initial revision
*
*--------------------------------------------------------
       IMPLICIT NONE
*
       external thgetid
       integer*4 thgetid
       character*50 here
       parameter (here= 's_fill_scin_raw_hist_')
*
       logical ABORT
       character*(*) err
       real*4  histval
       integer*4 plane,counter,ihit
       include 'sos_data_structures.cmn'
       include 'sos_scin_parms.cmn'
       include 'sos_id_histid.cmn'          
*
       SAVE
*--------------------------------------------------------
*
       ABORT= .FALSE.
       err= ' '
* Do we want to histogram raw scintillators
 
       histval = SSCIN_TOT_HITS
       call hf1(sidscinrawtothits,histval,1.)
* Make sure there is at least 1 hit
       if(SSCIN_TOT_HITS .gt. 0 ) then
* Loop over all hits
         do ihit=1,SSCIN_TOT_HITS
           plane=SSCIN_PLANE_NUM(ihit)
           counter=SSCIN_COUNTER_NUM(ihit)
* Fill plane map                  
           histval = FLOAT(SSCIN_PLANE_NUM(ihit))
           call hf1(sidscinplane,histval,1.)
* Check for valid plane
           if(plane.gt.0 .and. plane .le. snum_scin_planes) then
* Fill counter map
             histval = FLOAT(SSCIN_COUNTER_NUM(ihit))
             call hf1(sidscincounters(plane),histval,1.)
* Fill ADC and TDC histograms
             if((counter .gt. 0) .and.
     $            (counter.le.snum_scin_counters(plane)))then

               if (sscin_tdc_pos(ihit).ne.-1) then
                 histval = counter
                 call hf1(sidscinallpostdc(plane),histval,1.)
               endif
               if (sscin_tdc_neg(ihit).ne.-1) then
                 histval = counter
                 call hf1(sidscinallnegtdc(plane),histval,1.)
               endif
               if (sscin_adc_pos(ihit).ge.50) then
                 histval = counter
                 call hf1(sidscinallposadc(plane),histval,1.)
               endif
               if (sscin_adc_neg(ihit).ge.50) then
                 histval = counter
                 call hf1(sidscinallnegadc(plane),histval,1.)
               endif

               if(sturnon_scin_raw_hist .ne. 0 ) then
                 histval = SSCIN_ADC_POS(ihit)
                 call hf1(sidscinposadc(plane,counter),histval,1.)
                 histval = SSCIN_ADC_NEG(ihit)
                 call hf1(sidscinnegadc(plane,counter),histval,1.)
                 histval = FLOAT(SSCIN_TDC_POS(ihit))
                 call hf1(sidscinpostdc(plane,counter),histval,1.)
                 histval = FLOAT(SSCIN_TDC_NEG(ihit))
                 call hf1(sidscinnegtdc(plane,counter),histval,1.)
               endif                    ! end test on histogramming flag
             endif                      ! end test on valid counter number
           endif                        ! end test on valid plane number
         enddo                          ! end loop over hits
       endif                            ! end test on zero hits       

       RETURN
       END

