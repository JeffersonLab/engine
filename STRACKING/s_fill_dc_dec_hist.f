      subroutine s_fill_dc_dec_hist(Abort,err)
*
*     routine to fill histograms with sos_decoded_dc varibles
*
*     Author:	D. F. Geesaman
*     Date:     30 March 1994
*     Modified:  9 April 1994     D. F. Geesaman
*                                 Put id's in sos_tracking_histid
*                                 implement flag to turn block off
* $Log: s_fill_dc_dec_hist.f,v $
* Revision 1.6  2002/07/31 20:20:58  saw
* Only try to fill user hists that are defined
*
* Revision 1.5  1996/04/30 17:12:04  saw
* (JRA) Comment out SDC_DRIFT_DIS and SDC_DRIFT_TIME histograms
*
* Revision 1.4  1995/08/31 18:42:28  cdaq
* (JRA) Comment out filling of siddcwirecent (wire center) histogram
*
* Revision 1.3  1995/05/22  19:45:38  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.2  1994/08/18  04:33:13  cdaq
* (SAW) Indentation changes
*
* Revision 1.1  1994/04/13  18:10:22  cdaq
* Initial revision
*
*--------------------------------------------------------
      IMPLICIT NONE
*
      character*50 here
      parameter (here= 's_fill_dc_dec_hist_')
*
      logical ABORT
      character*(*) err
      real*4  histval
      integer*4 plane,ihit
*
      include 'sos_data_structures.cmn'
      include 'sos_tracking.cmn'
      include 'sos_track_histid.cmn'
      include 'gen_event_info.cmn'
*
      SAVE
*--------------------------------------------------------
*
      ABORT= .FALSE.
      err= ' '
*
* Is histogramming flag set
      if(sturnon_decoded_dc_hist.ne.0 ) then
* Make sure there is at least 1 hit
        if(SDC_TOT_HITS .gt. 0 ) then
* Loop over all hits
          do ihit=1,SDC_TOT_HITS 
            plane=SDC_PLANE_NUM(ihit)
            histval=SDC_WIRE_NUM(ihit)
* Is plane number valid
            if( (plane .gt. 0) .and. (plane.le. sdc_num_planes)) then
              if(siddcwiremap(plane).gt.0)
     $             call hf1(siddcwiremap(plane),histval,1.)
c              call hf1(siddcwirecent(plane),SDC_WIRE_CENTER(ihit),1.)
c              call hf1(siddcdriftdis(plane),SDC_DRIFT_DIS(ihit),1.)
c              call hf1(siddcdrifttime(plane),SDC_DRIFT_TIME(ihit),1.)
            endif                       ! end test on valid plane number
          enddo                         ! end loop over hits
        endif                           ! end test on zero hits       
      endif                             ! end test on histogram block turned on.
      RETURN
      END

