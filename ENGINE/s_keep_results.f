      SUBROUTINE S_keep_results(ABORT,err)
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
* $Log: s_keep_results.f,v $
* Revision 1.6  1996/09/04 15:17:33  saw
* (JRA) Make SSNUM_FPTRACK.gt.0 instead of SNTRACKS_FP .gt. 0 the
*       criteria for adding to ntuples
*
* Revision 1.5  1996/01/16 16:42:34  cdaq
* no change
*
* Revision 1.4  1995/08/11 15:43:11  cdaq
* (DD) Add sos sieve slit ntuple
*
* Revision 1.3  1995/07/27  19:43:52  cdaq
* (JRA) Only add to ntuples when we have HNTRACKS_FP > 0
*
* Revision 1.2  1994/04/12  17:29:18  cdaq
* (KBB) Add ntuple call
*
* Revision 1.1  1994/02/04  22:18:31  cdaq
* Initial revision
*
*- All standards are from "Proposal for Hall C Analysis Software
*- Vade Mecum, Draft 1.0" by D.F.Geesamn and S.Wood, 7 May 1993
*-
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      include 'sos_data_structures.cmn'
*
      character*50 here
      parameter (here= 'S_keep_results')
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
      if(SSNUM_FPTRACK.gt.0) call s_ntuple_keep(ABORT,err)! check for good tracks.
* proceed only if tracks found is greater than zero.   
*
*
      IF(ABORT) THEN
         call G_add_path(here,err)
      ELSE
         err= ' '
      ENDIF
       if(SSNUM_FPTRACK.gt.0)call s_sv_nt_keep(ABORT,err)
*
      IF(ABORT) THEN
         call G_add_path(here,err)
      ELSE
         err= ' '
      ENDIF
*
      RETURN
      END
