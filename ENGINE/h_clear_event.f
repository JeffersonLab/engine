      SUBROUTINE H_clear_event(ABORT,err)
*--------------------------------------------------------
*-       Prototype C analysis routine
*-
*-
*-   Purpose and Methods : clears all HMS quantities before event is processed.
*-
*- 
*-   Output: ABORT		- success or failure
*-         : err	- reason for failure, if any
*- 
*-   Created  29-Oct-1993   Kevin B. Beard
*-    $Log$
*-    Revision 1.1  1994/02/04 22:14:24  cdaq
*-    Initial revision
*-
*- 
*-
*- All standards are from "Proposal for Hall C Analysis Software
*- Vade Mecum, Draft 1.0" by D.F.Geesamn and S.Wood, 7 May 1993
*-
*-
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*13 here
      parameter (here= 'H_clear_event')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 'gen_data_structures.cmn'
*
      INTEGER hit,track,block,i,j,plane
*
*--------------------------------------------------------
*
      HDC_TOT_HITS= 0

      DO plane= 1,HNUM_DC_PLANES
         HDC_HITS_PER_PLANE(plane)= 0
      ENDDO
*
      HSCIN_TOT_HITS= 0
*
*     HMS CALORIMETER HITS
*
      HCAL_TOT_HITS= 0
*
*     HMS CERENKOV HITS
*
      HCER_TOT_HITS= 0
*
*     HMS DETECTOR TRACK QUANTITIES
*
      HNTRACKS_FP= 0
* 
*     HMS TARGET QUANTITIES
*
      HNTRACKS_TAR= 0
*
      ABORT= .FALSE.
      err= ' '
      RETURN
      END
