      SUBROUTINE H_reset_event(ABORT,err)
*--------------------------------------------------------
*-       Prototype C analysis routine
*-
*-
*-   Purpose and Methods : Resets all HMS quantities before event is processed.
*-
*- 
*-   Output: ABORT		- success or failure
*-         : err	- reason for failure, if any
*- 
*-   Created  29-Oct-1993   Kevin B. Beard
*-   Modified 20-Nov-1993   KBB for new errors
*-
*- All standards are from "Proposal for Hall C Analysis Software
*- Vade Mecum, Draft 1.0" by D.F.Geesamn and S.Wood, 7 May 1993
* $Log$
* Revision 1.10  1995/05/22 20:50:47  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.9  1995/05/11  18:58:17  cdaq
* (SAW) Change HSDEDXn vars to an array
*
* Revision 1.8  1994/09/20  17:29:33  cdaq
* (SAW) Add include of hms_tracking.cmn
*
* Revision 1.7  1994/07/07  21:16:38  cdaq
* (JRA) Clear additional variables
*
* Revision 1.6  1994/06/28  20:07:00  cdaq
* (SAW) Add clearing of HSCIN_ALL arrays
*
*-
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*50 here
      parameter (here= 'H_reset_event')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 'hms_data_structures.cmn'
      include 'hms_tracking.cmn'
*
      INTEGER hit,track,block,i,j,plane
*
*--------------------------------------------------------
*
      DO hit= 1,HMAX_DC_HITS
         HDC_RAW_PLANE_NUM(hit)= 0
         HDC_RAW_WIRE_NUM(hit)= 0
         HDC_RAW_TDC(hit)= 0
         HDC_DRIFT_TIME(hit)= 0
         HDC_DRIFT_DIS(hit)= 0
         HDC_WIRE_CENTER(hit)= 0
         HDC_WIRE_COORD(hit)= 0
         HDC_PLANE_NUM(hit)= 0
         HDC_WIRE_NUM(hit)= 0
         HDC_TDC(hit)= 0
      ENDDO
      HDC_TOT_HITS= 0
*
      DO plane= 1,HMAX_NUM_DC_PLANES
         HDC_HITS_PER_PLANE(plane)= 0
      ENDDO
*
      DO hit= 1,HMAX_SCIN_HITS
         HSCIN_ZPOS(hit)= 0.0
         HSCIN_CENTER_COORD(hit)= 0.0
         HSCIN_COR_ADC(hit)= 0
         HSCIN_COR_TIME(hit)= 0
         HSCIN_PLANE_NUM(hit)= 0
         HSCIN_COUNTER_NUM(hit)= 0
         HSCIN_ADC_POS(hit)= 0
         HSCIN_ADC_NEG(hit)= 0
         HSCIN_TDC_POS(hit)= 0
         HSCIN_TDC_NEG(hit)= 0
         HSCIN_ALL_PLANE_NUM(hit)= 0
         HSCIN_ALL_COUNTER_NUM(hit)= 0
         HSCIN_ALL_ADC_POS(hit)= 0
         HSCIN_ALL_ADC_NEG(hit)= 0
         HSCIN_ALL_TDC_POS(hit)= 0
         HSCIN_ALL_TDC_NEG(hit)= 0
      ENDDO
      DO plane= 1,HNUM_SCIN_PLANES
         HSCIN_HITS_PER_PLANE(plane)= 0
      ENDDO
      HSCIN_TOT_HITS= 0
      HSCIN_ALL_TOT_HITS= 0
*     
*     HMS CALORIMETER HITS
*
      DO block= 1,HMAX_CAL_BLOCKS
         HBLOCK_XC(block)= 0.
         HBLOCK_ZC(block)= 0
         HBLOCK_DE(block)= 0
         HCAL_ROW(block)= 0
         HCAL_COLUMN(block)= 0
         HCAL_ADC(block)= 0
      ENDDO
      HCAL_TOT_HITS= 0
*     
*     HMS CERENKOV HITS
*
      DO hit= 1,HMAX_CER_HITS
         HCER_TUBE_NUM(hit)= 0
         HCER_ADC(hit)= 0
         HCER_PLANE(hit)= 0
      ENDDO
      HCER_TOT_HITS= 0
*     
*     HMS Miscleaneous hits
*
      do hit=1,HMAX_MISC_HITS
         HMISC_RAW_ADDR1(hit) = 0
         HMISC_RAW_ADDR2(hit) = 0
         HMISC_RAW_DATA(hit) = 0
      enddo
      hmisc_tot_hits = 0
*     
*     HMS DETECTOR TRACK QUANTITIES
*     
      DO track= 1,HNTRACKS_MAX
         HX_FP(track)= 0
         HY_FP(track)= 0
         HZ_FP(track)= 0
         HXP_FP(track)= 0
         HYP_FP(track)= 0
         HCHI2_FP(track)= 0
         HDEL_FP(4,4,track)= 0
         HNFREE_FP(track)= 0
         Do hit= 1,HNTRACKHITS_MAX
	    HNTRACK_HITS(track,hit)= 0
         EndDo
      ENDDO
      HNTRACKS_FP= 0
*     
*     HMS TARGET QUANTITIES
*     
      DO track= 1,HNTRACKS_MAX
         HX_TAR(track)= 0
         HY_TAR(track)= 0
         HZ_TAR(track)= 0
         HXP_TAR(track)= 0
         HYP_TAR(track)= 0
         HDELTA_TAR(track)= 0
         HP_TAR(track)= 0
         HCHI2_TAR(track)= 0
         HDEL_TAR(5,5,track)= 0
         HNFREE_TAR(track)= 0
         HLINK_TAR_FP(track)= 0
         Do j= 1,5
            do i= 1,5
               HDEL_TAR(i,j,track)= 0.
            enddo
         EndDo
      ENDDO
      HNTRACKS_TAR= 0

      DO track= 1,HNTRACKS_MAX
         HNBLOCKS_CAL(track)= 0
         HTRACK_E1(track)= 0.
         HTRACK_E2(track)= 0.
         HTRACK_E3(track)= 0.
         HTRACK_E4(track)= 0.
         HTRACK_ET(track)= 0.
         HTRACK_PRESHOWER_E(track)= 0.
         do hit = 1 , HMAX_SCIN_HITS
           HSCIN_HIT(track,hit)= 0
         enddo
         do plane = 1 , HNUM_SCIN_PLANES
           HDEDX(track,plane) = 0.
         enddo
         HNUM_SCIN_HIT(track)=0
         HBETA(track)=0.
         HBETA_CHISQ(track)=0.
         HTIME_AT_FP(track)=0.
       ENDDO
       
       HSP=0.
       HSENERGY=0.
       HSDELTA=0.
       HSTHETA=0.
       HSPHI=0.
       HSMINV=0.
       HSZBEAM=0.
       do plane = 1 , HNUM_SCIN_PLANES
         HSDEDX(plane) = 0.
       enddo
       HSBETA=0.
       HSTRACK_ET=0.
       HSTRACK_PRESHOWER_E=0.
       HSTIME_AT_FP=0.
       HSX_FP=0.
       HSY_FP=0.
       HSXP_FP=0.
       HSYP_FP=0.
       HSCHI2PERDEG=0.
       HSX_TAR=0.
       HSY_TAR=0.
       HSXP_TAR=0.
       HSYP_TAR=0.
       HSNUM_FPTRACK=0
       HSNUM_TARTRACK=0
       HSID_LUND=0
       HSNFREE_FP=0
*     
*     
      ABORT= .FALSE.
      err= ' '
      RETURN
      END
