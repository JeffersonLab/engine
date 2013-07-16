      SUBROUTINE C_keep_results(ABORT,err)
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
* $Log: c_keep_results.f,v $
* Revision 1.5  1996/09/04 15:29:30  saw
* * (JRA) Make HSNUM_FPTRACK.gt.0 and SSNUM_FPTRACK.gt.0  instead of
*         HNTRACKS_FP .gt. 0 and HNTRACKS_FP .gt. 0 as criteria for
*         adding to ntuples
*
* Revision 1.4  1996/01/22 15:05:20  saw
* (JRA) Only fill coin ntuple if HMS and SOS both have tracks
*
* Revision 1.3  1996/01/16 21:00:40  cdaq
* no change
*
* Revision 1.2  1994/04/12 17:10:33  cdaq
* (KBB) Add ntuple call
*
* Revision 1.1  1994/02/04  21:07:07  cdaq
* Initial revision
*
*-
*- All standards are from "Proposal for Hall C Analysis Software
*- Vade Mecum, Draft 1.0" by D.F.Geesamn and S.Wood, 7 May 1993
*-
*-
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      include 'hms_data_structures.cmn'
      include 'sos_data_structures.cmn'
*
      character*14 here
      parameter (here= 'C_keep_results')
*
      logical ABORT
      character*(*) err
*
*--------------------------------------------------------
*-chance to flush any statistics, etc.
*
*
      ABORT= .FALSE.
      err = ' '
*
      if(HSNUM_FPTRACK .gt. 0 .AND. SSNUM_FPTRACK .gt. 0) ! check for tracks
     >  call c_ntuple_keep(ABORT,err)
*
      IF(ABORT) THEN
         call G_add_path(here,err)
         RETURN
      ELSE
         err= ' '
      ENDIF
*     
      RETURN
      END
