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
*
* $Log: h_clear_event.f,v $
* Revision 1.15  2002/12/20 21:55:23  jones
* Modified by Hamlet for new HMS aerogel
*
* Revision 1.15  2002/09/26
* (Hamlet) Add clear of HMS Aerogel
*
* Revision 1.14  1999/02/23 18:25:15  csa
* Add call to h_ntuple_clear
*
* Revision 1.13  1996/01/16 17:05:05  cdaq
* no change
*
* Revision 1.12  1995/10/09 18:07:59  cdaq
* (JRA) Add clear of HCER_RAW_ADC
*
* Revision 1.11  1995/09/01 13:36:45  cdaq
* (JRA) Clear some cerenkov variables
*
* Revision 1.10  1995/05/22  20:50:46  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.9  1995/03/13  18:12:46  cdaq
* (SAW) Include file ordering
*
* Revision 1.8  1994/10/11  20:27:35  cdaq
* (JRA) Include additional common blocks
*
* Revision 1.7  1994/09/20  17:29:41  cdaq
* (SAW) Add include of hms_tracking.cmn
*
* Revision 1.6  1994/07/07  21:16:57  cdaq
* (JRA) Clear additional variables
*
* Revision 1.5  1994/06/28  20:05:20  cdaq
* (SAW) Add clear of hscin_all_tot_hits
*
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
      INCLUDE 'hms_data_structures.cmn'
      INCLUDE 'hms_tracking.cmn'
      INCLUDE 'hms_statistics.cmn'
      INCLUDE 'hms_scin_parms.cmn'
      INCLUDE 'hms_scin_tof.cmn'
      INCLUDE 'hms_cer_parms.cmn'
      INCLUDE 'hms_calorimeter.cmn'

*
      INTEGER plane,tube
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
      HCAL_NUM_HITS= 0
*
*     HMS CERENKOV HITS
*
      HCER_TOT_HITS= 0
      do tube = 1, HMAX_CER_HITS
        HCER_RAW_ADC(tube) = 0
        HCER_ADC(tube) = 0
        HCER_NPE(tube) = 0.
      enddo

*
*     HMS AEROGEL HITS
*
      HAERO_TOT_HITS = 0

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
      HSNUM_FPTRACK = 0
      HSNUM_TARTRACK = 0
*

      call h_ntuple_clear

      ABORT= .FALSE.
      err= ' '
      RETURN
      END
