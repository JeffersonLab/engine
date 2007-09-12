      SUBROUTINE H_keep_results(ABORT,err)
*--------------------------------------------------------
*-       Prototype C analysis routine
*-
*-
*-   Purpose and Methods : Keeps statistics, etc.
*- 
*-   Output: ABORT	- success or failure
*-         : err	- reason for failure, if any
*- 
*-   Created  20-Nov-1993   Kevin B. Beard for new error standards
*
* $Log$
* Revision 1.6.24.3  2007/09/12 14:40:03  brash
* *** empty log message ***
*
* Revision 1.6.24.2  2007/09/11 19:14:17  frw
* fixed FPP related arrays and limits
*
* Revision 1.6.24.1  2007/08/22 19:09:17  frw
* added FPP
*
* Revision 1.7  frw
* added FPP call
*
* Revision 1.6  1996/09/04 14:42:14  saw
* (JRA) Make HSNUM_FPTRACK.gt.0 instead of HNTRACKS_FP .gt. 0 the
*       criteria for adding to ntuples
*
* Revision 1.5  1996/01/16 17:03:06  cdaq
* no change
*
* Revision 1.4  1995/07/27 19:40:29  cdaq
* (JRA) Only add to ntuples when we have HNTRACKS_FP > 0
*
* Revision 1.3  1995/01/27  20:14:51  cdaq
* (SAW) Add call to sieve slit ntuple keep routine
*
* Revision 1.2  1994/04/12  17:21:58  cdaq
* (KBB) Add ntuple call
*
* Revision 1.1  1994/02/04  22:17:38  cdaq
* Initial revision
*
*- All standards are from "Proposal for Hall C Analysis Software
*- Vade Mecum, Draft 1.0" by D.F.Geesamn and S.Wood, 7 May 1993
*-
*-
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      include 'gen_detectorids.par'
      include 'gen_decode_common.cmn'
      include 'hms_data_structures.cmn'
      INCLUDE 'hms_fpp_params.cmn'
      INCLUDE 'hms_fpp_event.cmn'
*
      character*50 here
      parameter (here= 'H_keep_results')
*
      logical ABORT
      character*(*) err
*
*--------------------------------------------------------
*-chance to flush any statistics, etc.
*
*
      ABORT= .FALSE.
      err= ' '
*
      if(HSNUM_FPTRACK.gt.0) call h_ntuple_keep(ABORT,err)! check for good tracks
*
      IF(ABORT) THEN
         call G_add_path(here,err)
      ELSE
         err= ' '
      ENDIF
*
*
c      write(*,*)'Calling h_fpp_nt_keep:',HFPP_eventclass,HFPP_min_event_code
      if (HFPP_eventclass.ge.HFPP_min_event_code) call h_fpp_nt_keep(ABORT,err)
*
      IF(ABORT) THEN
         call G_add_path(here,err)
      ELSE
         err= ' '
      ENDIF
*
* Need to fix up the bloody error reporting
*
      if(HSNUM_FPTRACK.gt.0)call h_sv_nt_keep(ABORT,err) ! at least one track
*
      IF(ABORT) THEN
         call G_add_path(here,err)
      ELSE
         err= ' '
      ENDIF
*
      RETURN
      END
