      subroutine s_fill_dc_fp_hist(Abort,err)
*
*     routine to fill histograms with sos_focal_plane varibles
*
*     Author:	D. F. Geesaman
*     Date:     30 March 1994
*     Modified: 9 April 1994        DFG
*                                   Transfer ID in common block
*                                   Implement flag to turn block on
* $Log: s_fill_dc_fp_hist.f,v $
* Revision 1.5  2002/07/31 20:20:58  saw
* Only try to fill user hists that are defined
*
* Revision 1.4  1995/05/22 19:45:38  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.3  1995/04/06  19:35:40  cdaq
* (JRA) Add WC residual histograms
*
* Revision 1.2  1994/08/18  04:34:23  cdaq
* (SAW) Indentation changes
*
* Revision 1.1  1994/04/13  18:10:39  cdaq
* Initial revision
*
*--------------------------------------------------------
      IMPLICIT NONE
*
      character*50 here
      parameter (here= 's_fill_dc_fp_hist')
*
      logical ABORT
      character*(*) err
      real*4  histval
      integer*4 itrk
      integer*4 plane
*
      include 'sos_data_structures.cmn'
      include 'sos_track_histid.cmn'
      include 'sos_tracking.cmn'
*
      SAVE
*--------------------------------------------------------
*
      ABORT= .FALSE.
      err= ' '
*
* Is this histogram flag turned on
      if(sturnon_focal_plane_hist .ne. 0 ) then
* Make sure there is at least 1 track
        if(SNTRACKS_FP .gt. 0 ) then
* Loop over all hits
          do itrk=1,SNTRACKS_FP
            if(sidsx_fp.gt.0)
     $           call hf1(sidsx_fp,SX_FP(itrk),1.)
            if(sidsy_fp.gt.0)
     $           call hf1(sidsy_fp,SY_FP(itrk),1.)
            if(sidsxp_fp.gt.0)
     $           call hf1(sidsxp_fp,SXP_FP(itrk),1.)
            if(sidsyp_fp.gt.0)
     $           call hf1(sidsyp_fp,SYP_FP(itrk),1.)
            if(SCHI2_FP(itrk) .gt. 0 ) then
              histval=log10(SCHI2_FP(itrk))            
            else 
              histval = 10.
            endif
            if(sidslogchi2_fp.gt.0)
     $           call hf1(sidslogchi2_fp,histval,1.)
            histval= SNFREE_FP(itrk)
            if(sidsnfree_fp.gt.0)
     $           call hf1(sidsnfree_fp,histval,1.)
            if( SNFREE_FP(itrk) .ne.0) then
              histval= SCHI2_FP(itrk) /  SNFREE_FP(itrk)
            else
              histval = -1.
            endif
            if(sidschi2perdeg_fp.gt.0)
     $           call hf1(sidschi2perdeg_fp,histval,1.)
*
            do plane = 1,sdc_num_planes
              if(sidres_fp(plane).gt.0)
     $             call hf1(sidres_fp(plane),
     $             sdc_double_residual(itrk,plane),1.)
              if(sidsingres_fp(plane).gt.0)
     $             call hf1(sidsingres_fp(plane),
     $             sdc_single_residual(itrk,plane),1.)
            enddo

          enddo                         ! end loop over hits
        endif                           ! end test on zero hits       
      endif                             ! end test on histogramming flag
      RETURN
      END
