      subroutine h_fill_dc_dec_hist(Abort,err)
*
*     routine to fill histograms with hms_decoded_dc varibles
*     In the future ID numbers are stored in hms_histid
*
*     Author:	D. F. Geesaman
*     Date:     30 March 1994
*     Modified:  9 April 1994     D. F. Geesaman
*                                 Put id's in hms_tracking_histid
*                                 implement flag to turn block off
* $Log: h_fill_dc_dec_hist.f,v $
* Revision 1.6  2002/10/02 13:42:43  saw
* Check that user hists are defined before filling
*
* Revision 1.5  1996/04/30 12:36:43  saw
* (JRA) Comment out HDC_DRIFT_DIS and HDC_DRIFT_TIME histograms
*
* Revision 1.4  1995/08/31 15:02:26  cdaq
* (JRA) Comment out filling of hiddcwirecent (wire center) histogram
*
* Revision 1.3  1995/05/22  19:39:11  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.2  1994/08/18  04:26:03  cdaq
* (SAW) Indentation changes
*
* Revision 1.1  1994/04/13  15:38:24  cdaq
* Initial revision
*
*-
*--------------------------------------------------------
      IMPLICIT NONE
*
      character*50 here
      parameter (here= 'h_fill_dc_dec_hist_')
*
      logical ABORT
      character*(*) err
      real*4  histval
      integer*4 planeoff,ihit
*
      include 'hms_data_structures.cmn'
      include 'hms_tracking.cmn'
      include 'hms_track_histid.cmn'
      include 'gen_event_info.cmn'
*
      SAVE
*--------------------------------------------------------
*
      ABORT= .FALSE.
      err= ' '
*
* Is histogramming flag set
      if(hturnon_decoded_dc_hist.ne.0) then
* Make sure there is at least 1 hit
        if(HDC_TOT_HITS .gt. 0 ) then
* Loop over all hits
          do ihit=1,HDC_TOT_HITS 
            planeoff=HDC_PLANE_NUM(ihit)
            histval=HDC_WIRE_NUM(ihit)
* Is plane number valid
            if( (planeoff .gt. 0) .and. (planeoff.le. hdc_num_planes)
     $           .and. hiddcwiremap(planeoff).gt.0) then
              call hf1(hiddcwiremap(planeoff),histval,1.)
c              call hf1(hiddcwirecent(planeoff),HDC_WIRE_CENTER(ihit),1.)
c              call hf1(hiddcdriftdis(planeoff),HDC_DRIFT_DIS(ihit),1.)
c              call hf1(hiddcdrifttime(planeoff),HDC_DRIFT_TIME(ihit),1.)
            endif                       ! end test on valid plane number
          enddo                         ! end loop over hits
        endif                           ! end test on zero hits       
      endif                             ! end test on histogram block turned on.
      RETURN
      END

