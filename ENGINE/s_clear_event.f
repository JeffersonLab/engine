      SUBROUTINE S_clear_event(ABORT,err)
*--------------------------------------------------------
*-       Prototype C analysis routine
*-
*-
*-   Purpose and Methods : clears all SOS quantities before event is processed.
*-
*- 
*-   Output: ABORT	- success or failure
*-         : err	- reason for failure, if any
*- 
*-   Created  2-Nov-1993   Kevin B. Beard
*-   Modified 20-Nov-1993  KBB for new errors
*-    $Log$
*-    Revision 1.3  1994/03/01 20:14:32  cdaq
*-    (SAW) Add zeroing of the raw total hits counter for the drift chambers
*-
* Revision 1.2  1994/02/22  19:04:02  cdaq
* (SAW) SNUM_DC_PLANES  --> SMAX_NUM_DC_PLANES
*
* Revision 1.1  1994/02/04  22:21:07  cdaq
* Initial revision
*
*-
*- All standards are from "Proposal for Hall C Analysis Software
*- Vade Mecum, Draft 1.0" by D.F.Geesamn and S.Wood, 7 May 1993
*-
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*     
      character*13 here
      parameter (here= 'S_clear_event')
*     
      logical ABORT
      character*(*) err
*     
      INCLUDE 'gen_data_structures.cmn'
*     
      INTEGER track,hit,block,i,j,plane
*     
*--------------------------------------------------------
*     
*     SOS DECODED DATA
*     
      SDC_RAW_TOT_HITS = 0
*
      SDC_TOT_HITS = 0
*
      DO plane= 1,SMAX_NUM_DC_PLANES
         SDC_HITS_PER_PLANE(plane)= 0
      ENDDO
*     
*     SOS SCINTILLATOR HITS
*     
      SSCIN_TOT_HITS = 0
*
      DO plane= 1,SNUM_SCIN_PLANES
         SSCIN_HITS_PER_PLANE(plane)= 0
      ENDDO
*     
*     SOS CALORIMETER HITS
*     
      SCAL_TOT_HITS= 0
*     
*     SOS CERENKOV HITS
*     
      SCER_TOT_HITS= 0
*     
*     SOS DETECTOR TRACK QUANTITIES
*     
      SNTRACKS_FP= 0
*     
*     SOS TARGET QUANTITIES
*     
      SNTRACKS_TAR= 0
*     
      ABORT= .FALSE.
      err= ' '
      RETURN
      END
