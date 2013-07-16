      subroutine s_fill_dc_target_hist(Abort,err)
*
*     routine to fill histograms with SOS_TARGET varibles
*
*     Author:	D. F. Geesaman
*     Date:     3 May 1994
* $Log: s_fill_dc_target_hist.f,v $
* Revision 1.4  2002/07/31 20:20:58  saw
* Only try to fill user hists that are defined
*
* Revision 1.3  1995/05/22 19:45:38  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.2  1994/08/18  04:31:47  cdaq
* (SAW) Indentation changes
*
* Revision 1.1  1994/05/13  03:04:19  cdaq
* Initial revision
*
*-
*--------------------------------------------------------
      IMPLICIT NONE
*
      character*50 here
      parameter (here= 's_fill_dc_target_hist')
*
      logical ABORT
      character*(*) err
      real*4  histval
      integer*4 itrk

*
      include 'sos_data_structures.cmn'
      include 'sos_track_histid.cmn'
*     
      SAVE
*--------------------------------------------------------
*
      ABORT= .FALSE.
      err= ' '
*
* Make sure there is at least 1 track
      if(SNTRACKS_FP .gt. 0 ) then
* Loop over all hits
        do itrk=1,SNTRACKS_FP
          if (sidsx_tar.gt.0)
     $         call hf1(sidsx_tar,SX_TAR(itrk),1.)
          if (sidsy_tar.gt.0)
     $         call hf1(sidsy_tar,SY_TAR(itrk),1.)
          if (sidsz_tar.gt.0)
     $         call hf1(sidsz_tar,SZ_TAR(itrk),1.)
          if (sidsxp_tar.gt.0)
     $         call hf1(sidsxp_tar,SXP_TAR(itrk),1.)
          if (sidsyp_tar.gt.0)
     $         call hf1(sidsyp_tar,SYP_TAR(itrk),1.)
          if (sidsdelta_tar.gt.0)
     $         call hf1(sidsdelta_tar,SDELTA_TAR(itrk),1.)
          if (sidsp_tar.gt.0)
     $         call hf1(sidsp_tar,SP_TAR(itrk),1.)
*     
* 
        enddo                           ! end loop over hits
      endif                             ! end test on zero hits       
      RETURN
      END
