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
*-      $Log: s_reset_event.f,v $
*-      Revision 1.15  1999/08/20 14:52:18  saw
*-      Put in warning if Xscin_tdc_max is bigger than 4094
*-
*-      Revision 1.14  1999/02/03 21:13:04  saw
*-      Code for new Shower counter tubes
*-
*-      Revision 1.13  1996/11/05 21:43:16  saw
*-      (WH) Add lucite counter
*-
*-      Revision 1.12  1996/09/04 15:18:54  saw
*-      (JRA) Zero out some misc scalers
*-
*-      Revision 1.11  1996/04/30 12:29:55  saw
*-      (JRA) Change SAER_ADC_LEFT/RIGHT to POS/NEG
*-
*-      Revision 1.10  1995/10/09 18:09:01  cdaq
*-      (JRA) Add clear of SCER_RAW_ADC
*-
*-      Revision 1.9  1995/07/27 19:44:17  cdaq
*-      (JRA) Zero out pedestal arrays
*-
* Revision 1.8  1995/05/22  20:50:48  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.7  1995/05/11  15:08:32  cdaq
* (SAW) Change SDEDXn vars to an array.  Add reset of Aerogel structure.
*
* Revision 1.6  1994/11/22  20:15:35  cdaq
* (SPB) Bring up to date with h_reset_event
*
* Revision 1.5  1994/06/22  20:51:22  cdaq
* (SAW) Zero out the miscleaneous hits array
*
* Revision 1.4  1994/03/24  22:01:43  cdaq
* Reflect changes in gen_data_structures.cmn
*
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
      INCLUDE 'sos_data_structures.cmn'
      INCLUDE 'sos_tracking.cmn'
      INCLUDE 'sos_pedestals.cmn'
      include 'sos_scin_parms.cmn'
      include 'sos_scin_tof.cmn'
*
      INTEGER track,hit,block,i,j,plane
*
*--------------------------------------------------------
*
*     SOS DECODED DATA
*
      do i=1,snum_scin_planes
        do j=1,snum_scin_elements
          shodo_pos_ped_num(i,j) = 0
          shodo_pos_ped_sum2(i,j) = 0
          shodo_pos_ped_sum(i,j) = 0
          shodo_neg_ped_num(i,j) = 0
          shodo_neg_ped_sum2(i,j) = 0
          shodo_neg_ped_sum(i,j) = 0
        enddo
      enddo

      do i=1,smax_cal_blocks
        scal_pos_ped_num(i)=0
        scal_pos_ped_sum2(i)=0
        scal_pos_ped_sum(i)=0
        scal_neg_ped_num(i)=0
        scal_neg_ped_sum2(i)=0
        scal_neg_ped_sum(i)=0
      enddo

      do i=1,smax_cer_hits
        scer_ped_num(i)=0
        scer_ped_sum2(i)=0
        scer_ped_sum(i)=0
      enddo

      do i=1,smax_aer_hits
        saer_pos_ped_num(i)=0
        saer_pos_ped_sum2(i)=0
        saer_pos_ped_sum(i)=0
        saer_neg_ped_num(i)=0
        saer_neg_ped_sum2(i)=0
        saer_neg_ped_sum(i)=0
      enddo

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
      SDC_TOT_HITS= 0
      DO plane= 1,SMAX_NUM_DC_PLANES
         SDC_HITS_PER_PLANE(plane)= 0
      ENDDO
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
         SSCIN_ALL_PLANE_NUM(hit)= 0
         SSCIN_ALL_COUNTER_NUM(hit)= 0
         SSCIN_ALL_ADC_POS(hit)= 0
         SSCIN_ALL_ADC_NEG(hit)= 0
         SSCIN_ALL_TDC_POS(hit)= 0
         SSCIN_ALL_TDC_NEG(hit)= 0
      ENDDO
      DO plane= 1,SNUM_SCIN_PLANES
         SSCIN_HITS_PER_PLANE(plane)= 0
      ENDDO
      SSCIN_TOT_HITS= 0
      SSCIN_ALL_TOT_HITS= 0
*     
*     SOS CALORIMETER HITS
*     
      DO block= 1,SMAX_CAL_BLOCKS
         SBLOCK_XC(block) = 0.
         SBLOCK_ZC(block) = 0.
         SBLOCK_DE(block) = 0.
         SBLOCK_DE_POS(block)= 0
         SBLOCK_DE_NEG(block)= 0
         SCAL_ROW(block) = 0
         SCAL_COLUMN(block) = 0
         SCAL_ADC_POS(block)= 0
         SCAL_ADC_NEG(block)= 0
         SCAL_ADC(block) = 0
      ENDDO
      SCAL_TOT_HITS= 0
*     
*     SOS CERENKOV HITS
*     
      DO hit= 1,SMAX_CER_HITS
         SCER_TUBE_NUM(hit) = 0
         SCER_RAW_ADC(hit) = 0
         SCER_ADC(hit) = 0
         SCER_PLANE(hit) = 0
      ENDDO
      SCER_TOT_HITS= 0
*
*     SOS AEROGEL HITS
*
      DO hit= 1,SMAX_AER_HITS
         SAER_PAIR_NUM(hit) = 0
         SAER_ADC_POS(hit) = 0
         SAER_ADC_NEG(hit) = 0
         SAER_PLANE(hit) = 0
      ENDDO
      SAER_TOT_HITS = 0

*
*     SOS LUCITE HITS
*
      DO hit= 1,SMAX_LUC_HITS
         SLUC_PAIR_NUM(hit) = 0
         SLUC_ADC_POS(hit) = 0
         SLUC_ADC_NEG(hit) = 0
         SLUC_TDC_POS(hit)= 0
         SLUC_TDC_NEG(hit)= 0
         SLUC_PLANE(hit) = 0
      ENDDO
      SLUC_TOT_HITS = 0

*     
*     SOS Miscleaneous hits
*
      do hit=1,SMAX_MISC_HITS
         SMISC_RAW_ADDR1(hit) = 0
         SMISC_RAW_ADDR2(hit) = 0
         SMISC_RAW_DATA(hit) = 0
         do plane=1,snum_misc_planes
           smisc_scaler(hit,plane)=0
         enddo
      enddo
      smisc_tot_hits = 0
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
*         Do j= 1,4
*            do i= 1,4
*               SDEL_FP(i,j,track)= 0.
*            enddo
*         EndDo
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
         SDEL_TAR(5,5,track)= 0.
         SNFREE_TAR(track)= 0.
         SLINK_TAR_FP(track)= 0.
         Do j= 1,5
            do i= 1,5
               SDEL_TAR(i,j,track)= 0.
            enddo
         EndDo
      ENDDO
      SNTRACKS_TAR= 0
      DO track=1, SNTRACKS_MAX
         SNBLOCKS_CAL(track)=0
         STRACK_E1(track)=0
         STRACK_E2(track)=0
         STRACK_E3(track)=0
         STRACK_E4(track)=0
         STRACK_ET(track)=0
         STRACK_E1_POS(track)= 0.
         STRACK_E1_NEG(track)= 0.
         STRACK_E2_POS(track)= 0.
         STRACK_E2_NEG(track)= 0.
         STRACK_PRESHOWER_E(track)=0
          do hit = 1, SMAX_SCIN_HITS
            SSCIN_HIT(track,hit)= 0
          enddo
          do plane = 1, SNUM_SCIN_PLANES
             SDEDX(track,plane) = 0
          enddo
        SNUM_SCIN_HIT(track)=0
        SBETA(track)=0
        SBETA_CHISQ(track)=0
        STIME_AT_FP(track)=0
      ENDDO
      
      SSP=0
      SSENERGY=0
      SSDELTA=0
      SSTHETA=0
      SSPHI=0
      SSMINV=0
      SSZBEAM=0
      do plane = 1 , SNUM_SCIN_PLANES
        SSDEDX(plane) = 0.
      enddo
      SSBETA=0
      SSTRACK_ET=0
      SSTRACK_PRESHOWER_E=0
      SSTIME_AT_FP=0
      SSX_FP=0
      SSY_FP=0
      SSXP_FP=0
      SSYP_FP=0
      SSCHI2PERDEG=0
      SSX_TAR=0
      SSY_TAR=0
      SSXP_TAR=0
      SSYP_TAR=0
      SSNUM_FPTRACK=0
      SSNUM_TARTRACK=0
      SSID_LUND=0
      SSNFREE_FP=0
*     
       if(sscin_tdc_max.gt.4094) then
         print *,' '
         print *,'WARNING!!: sscin_tdc_max is ',sscin_tdc_max
         print *,'We usually run our high resolution TDC''s with 12 bit'
         print *,'ranges.  If sscin_tdc_max is set to a value higher than'
         print *,'the TDC''s overflow channel, then overflowed TDC channels'
         print *,'will not be rejected.  Under high rate conditions, this'
         print *,'can result in bad beta and timing calculations'
         print *,' '
       endif
*
      ABORT= .FALSE.
      err= ' '
      RETURN
      END
