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
*-    Revision 1.5  1994/06/28 20:05:20  cdaq
*-    (SAW) Add clear of hscin_all_tot_hits
*-
* Revision 1.4  1994/06/22  20:53:21  cdaq
* (SAW) zero the miscleaneous hits counter
*
* Revision 1.3  1994/03/01  20:14:24  cdaq
* (SAW) Add zeroing of the raw total hits counter for the drift chambers
*
* Revision 1.2  1994/02/22  19:04:58  cdaq
* (SAW) HNUM_DC_PLANES -> HMAX_NUM_DC_PLANES
*
* Revision 1.1  1994/02/04  22:14:24  cdaq
* Initial revision
*
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
      HDC_RAW_TOT_HITS = 0
*
      HDC_TOT_HITS= 0
*
      DO plane= 1,HMAX_NUM_DC_PLANES
         HDC_HITS_PER_PLANE(plane)= 0
      ENDDO
*
      HSCIN_ALL_TOT_HITS = 0
      HSCIN_TOT_HITS = 0
*
      DO plane=1,HNUM_SCIN_PLANES
         HSCIN_HITS_PER_PLANE(plane) = 0
      ENDDO
*
*     HMS CALORIMETER HITS
*
      HCAL_TOT_HITS= 0
*
*     HMS CERENKOV HITS
*
      HCER_TOT_HITS= 0
*
*     HMS Miscleaneous hits
*
      HMISC_TOT_HITS = 0
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
