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
*-    Revision 1.6  1995/04/01 20:10:55  cdaq
*-    (SAW) Add missing SSCIN_ALL_TOT_HITS = 0
*-
* Revision 1.5  1994/11/22  20:14:23  cdaq
* (SPB) Bring up to date with h_clear_event
*
* Revision 1.4  1994/06/22  20:53:59  cdaq
* (SAW) zero the miscleaneous hits counter
*
* Revision 1.3  1994/03/01  20:14:32  cdaq
* (SAW) Add zeroing of the raw total hits counter for the drift chambers
*
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
      INCLUDE 'sos_tracking.cmn'
      INCLUDE 'sos_statistics.cmn'
      INCLUDE 'sos_scin_tof.cmn'
      INCLUDE 'sos_scin_parms.cmn'
      INCLUDE 'sos_calorimeter.cmn'
*     
      INTEGER plane
*     
*--------------------------------------------------------
*     
      SDC_RAW_TOT_HITS = 0
*
      SDC_TOT_HITS = 0
*
      DO plane= 1,SMAX_NUM_DC_PLANES
         SDC_HITS_PER_PLANE(plane)= 0
      ENDDO
*     
      SSCIN_ALL_TOT_HITS = 0
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
      SCAL_NUM_HITS= 0
*
*     SOS CERENKOV HITS
*     
      SCER_TOT_HITS= 0
*
*     SOS Miscleaneous hits
*
      SMISC_TOT_HITS = 0
*     
*     SOS DETECTOR TRACK QUANTITIES
*     
      SNTRACKS_FP= 0
*     
*     SOS TARGET QUANTITIES
*     
      SNTRACKS_TAR= 0
*     
      SSNUM_FPTRACK = 0
      SSNUM_TARTRACK = 0

      ABORT= .FALSE.
      err= ' '
      RETURN
      END
