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
* $Log: s_fill_scin_raw_hist.f,v $
* Revision 1.7  1996/01/17 19:04:54  cdaq
* (JRA)
*
* Revision 1.6  1995/10/10 13:27:45  cdaq
* (JRA) Remove some unneeded validity tests
*
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
      character*20 here
      parameter (here='s_fill_scin_raw_hist')
*
      logical ABORT
      character*(*) err
      real*4 histval
      real*4 rcnt
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
 
* Make sure there is at least 1 hit
      if(sscin_all_tot_hits .gt. 0 ) then
* Loop over all hits
        do ihit=1,sscin_all_tot_hits
          pln=sscin_all_plane_num(ihit)
          cnt=sscin_all_counter_num(ihit)
          rcnt=float(cnt)
* Fill plane map                  
c          histval = float(pln)
c          call hf1(sidscinplane,histval,1.)
* Fill counter map
          histval = rcnt
          call hf1(sidscincounters(pln),histval,1.)
* Fill ADC and TDC histograms for positive tubes.
          if (sscin_all_tdc_pos(ihit).ne.-1) then   !tube was hit.
            histval = rcnt
            call hf1(sidscinallpostdc(pln),histval,1.)
            histval = float(sscin_all_tdc_pos(ihit))
            call hf1(sidsumpostdc(pln),histval,1.)
          else                                          !tube was NOT hit.
            histval = sscin_all_adc_pos(ihit)-sscin_all_ped_pos(pln,cnt)
            call hf1(sidsumposadc(pln),histval,1.)
          endif

          if ((sscin_all_adc_pos(ihit)-sscin_all_ped_pos(pln,cnt))
     $         .ge.50) then
            histval = rcnt
            call hf1(sidscinallposadc(pln),histval,1.)
          endif

* Fill ADC and TDC histograms for negative tubes.
          if (sscin_all_tdc_neg(ihit).ne.-1) then       !tube was hit.
            histval = rcnt
            call hf1(sidscinallnegtdc(pln),histval,1.)
            histval = float(sscin_all_tdc_neg(ihit))
            call hf1(sidsumnegtdc(pln),histval,1.)
          else                                          !tube was NOT hit.
            histval = sscin_all_adc_neg(ihit)-sscin_all_ped_neg(pln,cnt)
            call hf1(sidsumnegadc(pln),histval,1.)
          endif

          if ((sscin_all_adc_neg(ihit)-sscin_all_ped_neg(pln,cnt))
     $         .ge.50) then
            histval = rcnt
            call hf1(sidscinallnegadc(pln),histval,1.)
          endif


          if(sturnon_scin_raw_hist .ne. 0 ) then
            histval = sscin_all_adc_pos(ihit)-sscin_all_ped_pos(pln,cnt)
            call hf1(sidscinposadc(pln,cnt),histval,1.)
            histval = sscin_all_adc_neg(ihit)-sscin_all_ped_neg(pln,cnt)
            call hf1(sidscinnegadc(pln,cnt),histval,1.)
            histval = float(sscin_all_tdc_pos(ihit))
            call hf1(sidscinpostdc(pln,cnt),histval,1.)
            histval = float(sscin_all_tdc_neg(ihit))
            call hf1(sidscinnegtdc(pln,cnt),histval,1.)
          endif                         ! end test on histogramming flag
        enddo                           ! end loop over hits
      endif                             ! end test on zero hits       

      return
      end
