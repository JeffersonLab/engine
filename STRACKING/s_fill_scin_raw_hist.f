
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
* Revision 1.5  1995/07/20 14:52:20  cdaq
* (JRA) Fill hist's from "all" data structures
*
* Revision 1.4  1995/05/22  19:45:39  cdaq
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
      integer*4 pln,cnt,ihit
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
 
      histval = SSCIN_ALL_TOT_HITS
      call hf1(sidscinrawtothits,histval,1.)
* Make sure there is at least 1 hit
      if(SSCIN_ALL_TOT_HITS .gt. 0 ) then
* Loop over all hits
        do ihit=1,SSCIN_ALL_TOT_HITS
          pln=SSCIN_ALL_PLANE_NUM(ihit)
          cnt=SSCIN_ALL_COUNTER_NUM(ihit)
* Fill plane map                  
          histval = FLOAT(SSCIN_ALL_PLANE_NUM(ihit))
          call hf1(sidscinplane,histval,1.)
* Check for valid plane
          if(pln.gt.0 .and. pln .le. snum_scin_planes) then
* Fill counter map
            histval = FLOAT(SSCIN_ALL_COUNTER_NUM(ihit))
            call hf1(sidscincounters(pln),histval,1.)
* Fill ADC and TDC histograms
            if((cnt .gt. 0) .and.
     $           (cnt.le.snum_scin_counters(pln)))then

              if (sscin_all_tdc_pos(ihit).ne.-1) then
                histval = cnt
                call hf1(sidscinallpostdc(pln),histval,1.)
              endif
              if (sscin_all_tdc_neg(ihit).ne.-1) then
                histval = cnt
                call hf1(sidscinallnegtdc(pln),histval,1.)
              endif
              if ((sscin_all_adc_pos(ihit)-sscin_all_ped_pos(pln,cnt))
     $             .ge.50) then
                histval = cnt
                call hf1(sidscinallposadc(pln),histval,1.)
              endif
              if ((sscin_all_adc_neg(ihit)-sscin_all_ped_neg(pln,cnt))
     $             .ge.50) then
                histval = cnt
                call hf1(sidscinallnegadc(pln),histval,1.)
              endif

              if (sscin_all_tdc_pos(ihit).eq.-1) then
                histval = SSCIN_ALL_ADC_POS(ihit)-sscin_all_ped_pos(pln,cnt)
                call hf1(sidsumposadc(pln),histval,1.)
              endif
              if (sscin_all_tdc_neg(ihit).eq.-1) then
                histval = SSCIN_ALL_ADC_NEG(ihit)-sscin_all_ped_neg(pln,cnt)
                call hf1(sidsumnegadc(pln),histval,1.)
              endif
              histval = FLOAT(SSCIN_ALL_TDC_POS(ihit))
              call hf1(sidsumpostdc(pln),histval,1.)
              histval = FLOAT(SSCIN_ALL_TDC_NEG(ihit))
              call hf1(sidsumnegtdc(pln),histval,1.)

              if(sturnon_scin_raw_hist .ne. 0 ) then
                histval = SSCIN_ALL_ADC_POS(ihit)-sscin_all_ped_pos(pln,cnt)
                call hf1(sidscinposadc(pln,cnt),histval,1.)
                histval = SSCIN_ALL_ADC_NEG(ihit)-sscin_all_ped_neg(pln,cnt)
                call hf1(sidscinnegadc(pln,cnt),histval,1.)
                histval = FLOAT(SSCIN_ALL_TDC_POS(ihit))
                call hf1(sidscinpostdc(pln,cnt),histval,1.)
                histval = FLOAT(SSCIN_ALL_TDC_NEG(ihit))
                call hf1(sidscinnegtdc(pln,cnt),histval,1.)
              endif                     ! end test on histogramming flag
            endif                       ! end test on valid counter number
          endif                         ! end test on valid plane number
        enddo                           ! end loop over hits
      endif                             ! end test on zero hits       

      RETURN
      END
