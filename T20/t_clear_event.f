      SUBROUTINE T_clear_event(ABORT,err)
*--------------------------------------------------------
*-
*-   Purpose and Methods : clears all T20 quantities before event is processed.
*-
*- 
*-   Output: ABORT		- success or failure
*-         : err	- reason for failure, if any
*- 
*-   Created  22-Jan-1997   Stephen A. Wood
*
* $Log$
* Revision 1.1  1998/12/01 20:57:37  saw
* Initial revision
*
*-
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*13 here
      parameter (here= 'T_clear_event')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 't20_data_structures.cmn'
*      INCLUDE 'hms_tracking.cmn'
*      INCLUDE 'hms_statistics.cmn'
*      INCLUDE 'hms_scin_parms.cmn'
*      INCLUDE 'hms_scin_tof.cmn'
*      INCLUDE 'hms_cer_parms.cmn'
*      INCLUDE 'hms_calorimeter.cmn'

*
c      INTEGER plane,tube
*
*--------------------------------------------------------
*
      TMWPC_RAW_TOT_HITS = 0

      THODO_TOT_HITS = 0

      TMISC_TOT_HITS = 0

      TTST_RAW_TOT_HITS = 0


      ABORT= .FALSE.
      err= ' '
      RETURN
      END

