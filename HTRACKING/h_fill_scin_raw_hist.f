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
* $Log: h_fill_scin_raw_hist.f,v $
* Revision 1.9  2002/07/31 20:17:52  saw
* Check that user hists are defined before filling
*
* Revision 1.8  1996/01/16 21:49:45  cdaq
* (JRA)
*
* Revision 1.7  1995/10/10 13:11:20  cdaq
* (JRA) Remove some unneeded validity tests
*
* Revision 1.6  1995/07/19 18:16:29  cdaq
* (JRA) Fill hist's from "all" data structures
*
* Revision 1.5  1995/05/22  19:39:12  cdaq
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
      real*4 histval
      real*4 rcnt
      integer*4 pln,cnt,ihit
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
* Make sure there is at least 1 hit
      if(hscin_all_tot_hits .gt. 0 ) then
* Loop over all hits
        do ihit=1,hscin_all_tot_hits
          pln=hscin_all_plane_num(ihit)
          cnt=hscin_all_counter_num(ihit)
          rcnt=float(cnt)
* Fill plane map
c          histval = float(pln)
c          call hf1(hidscinplane,histval,1.)
* Fill counter map
          histval = rcnt
          if (hidscincounters(pln).gt.0)
     $         call hf1(hidscincounters(pln),histval,1.)
* Fill ADC and TDC histograms for positive tubes.
          if (hscin_all_tdc_pos(ihit).ne.-1) then !tube was hit
            histval = rcnt
            if (hidscinallpostdc(pln).gt.0)
     $           call hf1(hidscinallpostdc(pln),histval,1.)
            histval = FLOAT(hscin_all_tdc_pos(ihit))
            if (hidsumpostdc(pln).gt.0)
     $           call hf1(hidsumpostdc(pln),histval,1.)
          else                                        !tube was NOT hit
            histval = hscin_all_adc_pos(ihit)-hscin_all_ped_pos(pln,cnt)
            if (hidsumposadc(pln).gt.0)
     $           call hf1(hidsumposadc(pln),histval,1.)
          endif

          if ((hscin_all_adc_pos(ihit)-hscin_all_ped_pos(pln,cnt))
     $         .ge. 50) then
            histval = rcnt
            if (hidscinallposadc(pln).gt.0)
     $           call hf1(hidscinallposadc(pln),histval,1.)
          endif

* Fill ADC and TDC histograms for negative tubes.
          if (hscin_all_tdc_neg(ihit).ne.-1) then     !tube was hit
            histval = rcnt
            if (hidscinallnegtdc(pln).gt.0)
     $           call hf1(hidscinallnegtdc(pln),histval,1.)
            histval = FLOAT(hscin_all_tdc_neg(ihit))
            if (hidsumnegtdc(pln).gt.0)
     $           call hf1(hidsumnegtdc(pln),histval,1.)
          else                          !tube was NOT hit
            histval = hscin_all_adc_neg(ihit)-hscin_all_ped_neg(pln,cnt)
            if (hidsumnegadc(pln).gt.0)
     $           call hf1(hidsumnegadc(pln),histval,1.)
          endif

          if ((hscin_all_adc_neg(ihit)-hscin_all_ped_neg(pln,cnt))
     $         .ge. 50) then
            histval = rcnt
            if (hidscinallnegadc(pln).gt.0)
     $           call hf1(hidscinallnegadc(pln),histval,1.)
          endif


* Do we want to histogram raw scintillators

          if(hturnon_scin_raw_hist .ne. 0 ) then
            histval = hscin_all_adc_pos(ihit)-hscin_all_ped_pos(pln,cnt)
            if (hidscinposadc(pln,cnt).gt.0)
     $           call hf1(hidscinposadc(pln,cnt),histval,1.)
            histval = hscin_all_adc_neg(ihit)-hscin_all_ped_neg(pln,cnt)
            if (hidscinnegadc(pln,cnt).gt.0)
     $           call hf1(hidscinnegadc(pln,cnt),histval,1.)
            histval = FLOAT(hscin_all_tdc_pos(ihit))
            if (hidscinpostdc(pln,cnt).gt.0)
     $           call hf1(hidscinpostdc(pln,cnt),histval,1.)
            histval = FLOAT(hscin_all_tdc_neg(ihit))
            if (hidscinnegtdc(pln,cnt).gt.0)
     $           call hf1(hidscinnegtdc(pln,cnt),histval,1.)
          endif                         ! end test on histogramming flag
        enddo                           ! end loop over hits
      endif                             ! end test on zero hits       

      return
      end
