      SUBROUTINE S_reset_event(ABORT,err)
*--------------------------------------------------------
*-       Prototype C analysis routine
*-
*-
*-   Purpose and Methods : Resets all SOS quantities before event is processed.
*-
*- 
*-   Output: ABORT	- success or failure
*-         : err	- reason for failure, if any
*- 
*-   Created  2-Nov-1993   Kevin B. Beard
*-   Modified 20-Nov-1993   KBB for new errors
*-      $Log$
*-      Revision 1.4  1994/03/24 22:01:43  cdaq
*-      Reflect changes in gen_data_structures.cmn
*-
* Revision 1.3  1994/02/22  19:43:15  cdaq
* (SAW) SNUM_DC_PLANES  --> SMAX_NUM_DC_PLANES
*
* Revision 1.2  1994/02/11  04:12:30  cdaq
* Change var names to reflect current gen_data_structures
*
* Revision 1.1  1994/02/04  22:16:02  cdaq
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
      character*50 here
      parameter (here= 'S_reset_event')
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
      DO hit= 1,SMAX_DC_HITS
         SDC_RAW_PLANE_NUM(hit)= 0
         SDC_RAW_WIRE_NUM(hit)= 0
         SDC_RAW_TDC(hit)= 0
         SDC_DRIFT_TIME(hit)= 0.
         SDC_DRIFT_DIS(hit)= 0.
         SDC_WIRE_CENTER(hit)= 0.
         SDC_WIRE_COORD(hit)= 0.
         SDC_PLANE_NUM(hit)= 0.
         SDC_WIRE_NUM(hit)= 0.
         SDC_TDC(hit)= 0.
      ENDDO
      DO plane= 1,SMAX_NUM_DC_PLANES
         SDC_HITS_PER_PLANE(plane)= 0
      ENDDO
      SDC_TOT_HITS= 0
*     
*     SOS SCINTILLATOR HITS
*     
      DO hit= 1,SMAX_SCIN_HITS
         SSCIN_ZPOS(hit)= 0.0
         SSCIN_CENTER_COORD(hit)= 0.0
         SSCIN_COR_ADC(hit)= 0.0
         SSCIN_COR_TIME(hit)= 0.0
         SSCIN_PLANE_NUM(hit)= 0
         SSCIN_COUNTER_NUM(hit)= 0
         SSCIN_ADC_POS(hit)= 0
         SSCIN_ADC_NEG(hit)= 0
         SSCIN_TDC_POS(hit)= 0
         SSCIN_TDC_NEG(hit)= 0
      ENDDO
      DO plane= 1,SNUM_SCIN_PLANES
         SSCIN_HITS_PER_PLANE(plane)= 0
      ENDDO
      SSCIN_TOT_HITS= 0
*     
*     SOS CALORIMETER HITS
*     
      DO block= 1,SMAX_CAL_BLOCKS
         SBLOCK_XC(block) = 0.
         SBLOCK_ZC(block) = 0.
         SBLOCK_DE(block) = 0.
         SCAL_ROW(block) = 0
         SCAL_COLUMN(block) = 0
         SCAL_ADC(block) = 0
      ENDDO
      SCAL_TOT_HITS= 0
*     
*     SOS CERENKOV HITS
*     
      DO hit= 1,SMAX_CER_HITS
         SCER_TUBE_NUM(hit) = 0
         SCER_ADC(hit) = 0
         SCER_PLANE(hit) = 0
      ENDDO
      SCER_TOT_HITS= 0
*     
*     SOS DETECTOR TRACK QUANTITIES
*     
      DO track= 1,SNTRACKS_MAX
         SX_FP(track)= 0.
         SY_FP(track)= 0.
         SZ_FP(track)= 0.
         SXP_FP(track)= 0.
         SYP_FP(track)= 0.
         SCHI2_FP(track)= 0.
         SNFREE_FP(track)= 0.
         Do j= 1,4
            do i= 1,4
               SDEL_FP(i,j,track)= 0.
            enddo
         EndDo
         Do hit= 1,SNTRACKHITS_MAX
            SNTRACK_HITS(track,hit)= 0
         EndDo
      ENDDO
      SNTRACKS_FP= 0
*     
*     SOS TARGET QUANTITIES
*     
      DO track= 1,SNTRACKS_MAX
         SX_TAR(track)= 0.
         SY_TAR(track)= 0.
         SZ_TAR(track)= 0.
         SXP_TAR(track)= 0.
         SYP_TAR(track)= 0.
         SDELTA_TAR(track)= 0.
         SP_TAR(track)= 0.
         SCHI2_TAR(track)= 0.
         SNFREE_TAR(track)= 0.
         SLINK_TAR_FP(track)= 0.
         Do j= 1,5
            do i= 1,5
               SDEL_TAR(i,j,track)= 0.
            enddo
         EndDo
      ENDDO
      SNTRACKS_TAR= 0
*     
      ABORT= .FALSE.
      err= ' '
      RETURN
      END
