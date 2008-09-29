      subroutine h_fill_dc_target_hist(Abort,err)
*
*     routine to fill histograms with HMS_TARGET varibles
*
*     Author:	D. F. Geesaman
*     Date:     3 May 1994
* $Log$
* Revision 1.3.24.1  2008/09/29 16:01:20  puckett
* added checking for existence of histograms, prevents analyzer crash when histograms are turned off to save memory
*
* Revision 1.3  1995/05/22 19:39:11  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.2  1994/08/18  04:28:53  cdaq
* (SAW) Indentation changes
*
* Revision 1.1  1994/05/12  19:02:40  cdaq
* Initial revision
*
*-
*--------------------------------------------------------
      IMPLICIT NONE
*
      character*50 here
      parameter (here= 'h_fill_dc_target_hist')
*
      logical ABORT
      character*(*) err
      real*4  histval
      integer*4 itrk

*
      include 'hms_data_structures.cmn'
      include 'hms_track_histid.cmn'
*
      SAVE
*--------------------------------------------------------
*
      ABORT= .FALSE.
      err= ' '
*
* Make sure there is at least 1 track
      if(HNTRACKS_FP .gt. 0 ) then
* Loop over all hits
        do itrk=1,HNTRACKS_FP
          if(hidhx_tar.gt.0) call hf1(hidhx_tar,HX_TAR(itrk),1.)
          if(hidhy_tar.gt.0) call hf1(hidhy_tar,HY_TAR(itrk),1.)
          if(hidhz_tar.gt.0) call hf1(hidhz_tar,HZ_TAR(itrk),1.)
          if(hidhxp_tar.gt.0) call hf1(hidhxp_tar,HXP_TAR(itrk),1.)
          if(hidhyp_tar.gt.0) call hf1(hidhyp_tar,HYP_TAR(itrk),1.)
          if(hidhdelta_tar.gt.0) call hf1(hidhdelta_tar,HDELTA_TAR(itrk),1.)
          if(hidhp_tar.gt.0) call hf1(hidhp_tar,HP_TAR(itrk),1.)
*
* 
        enddo                           ! end loop over hits
      endif                             ! end test on zero hits       
      RETURN
      END
