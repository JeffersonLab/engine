      SUBROUTINE G_reset_event(ABORT,err)
*--------------------------------------------------------
*-       Prototype C analysis routine
*-
*-
*-   Purpose and Methods : Resets all quantities AT THE BEGINNING OF THE RUN
*-
*- 
*-   Output: ABORT	- success or failure
*-         : err	- reason for failure, if any
*- 
*-   Created  29-Oct-1993   Kevin B. Beard
*-   Modified  3-Dec-1993   Kevin B. Beard, Hampton U.
* $Log$
* Revision 1.11.24.3.2.3  2008/10/26 19:12:33  cdaq
* SEM
*
* Revision 1.11.24.3.2.2  2008/10/02 17:59:26  cdaq
* *** empty log message ***
*
* Revision 1.11.24.3.2.1  2008/05/15 18:59:22  bhovik
* 1'st version
*
* Revision 1.11.24.3  2007/09/11 19:14:17  frw
* fixed FPP related arrays and limits
*
* Revision 1.11.24.2  2007/06/04 14:56:06  puckett
* changed hit array structure for trigger related signals
*
* Revision 1.11.24.1  2007/05/15 02:55:01  jones
* Start to Bigcal code
*
* Revision 1.11  1996/01/22 15:15:01  saw
* (JRA) Put BPM/Raster data into MISC data structures
*
* Revision 1.10  1996/01/16 17:07:55  cdaq
* (JRA) Zero out ADC threshold readback array
*
* Revision 1.9  1995/10/09 18:45:20  cdaq
* (JRA) Add scaler event reset call.  Remove monte carlo stuff.
*
* Revision 1.8  1995/07/27 19:39:25  cdaq
* (SAW) Disable monte carlo (GMC)
*
* Revision 1.7  1995/04/01  19:50:55  cdaq
* (SAW) Add BPM hitlist
*
* Revision 1.6  1994/06/22  20:24:23  cdaq
* (SAW) Zero out uninstrumented channel hit data structure
*
* Revision 1.5  1994/04/12  18:42:05  cdaq
* (SAW) Remove clearing of CRAW event buffer to online compatibility
*
* Revision 1.4  1994/02/22  19:47:36  cdaq
* Change gmc_reset_event to gmc_mc_reset
*
* Revision 1.3  1994/02/17  21:49:57  cdaq
* Simplify error handling to be like g_clear_event
*
* Revision 1.2  1994/02/17  21:43:39  cdaq
* Add call to gmc_reset_event
*
* Revision 1.1  1994/02/04  22:13:26  cdaq
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
      parameter (here= 'G_reset_event')
*
      logical ABORT
      character*(*) err
*
      logical HMS_ABORT,SOS_ABORT,COIN_ABORT,SCAL_ABORT
      logical BIGCAL_ABORT,GEP_ABORT,SANE_ABORT,F1trigger_abort,sem_abort
      character*132 HMS_err,SOS_err,COIN_err,SCAL_err,BIGCAL_err
      character*132 GEP_err,SANE_err,F1trigger_err,sem_err
*
      integer hit,chan,roc,slot
*
      INCLUDE 'gen_data_structures.cmn'
      include 'gen_detectorids.par'
      INCLUDE 'gen_decode_common.cmn'
      include 'gen_run_info.cmn'
*
*--------------------------------------------------------
*
      err = ' '
      hms_err = ' '
      sos_err = ' '
      coin_err = ' '
      bigcal_err = ' '
      sane_err = ' '
      f1trigger_err = ' '
      sem_err = ' '
      gep_err = ' '
*
*     Uninstrumented hits
*
      do hit=1,GMAX_UNINST_HITS
         GUNINST_RAW_ROCSLOT(hit) = 0
         GUNINST_RAW_SUBADD(hit) = 0
         GUNINST_RAW_DATAWORD(hit) = 0
      enddo
      GUNINST_TOT_HITS = 0
*
      do hit=1,GMAX_MISC_HITS
        GMISC_RAW_ADDR1(hit) = 0
        GMISC_RAW_ADDR2(hit) = 0
        GMISC_RAW_DATA(hit) = 0
      enddo
      GMISC_TOT_HITS = 0
*
      do slot=1,gmax_slot_with_adc
        do roc=1,gmax_roc_with_adc
          do chan=1,gnum_adc_channels
            g_threshold_readback(chan,roc,slot)=0
          enddo
        enddo
      enddo
*
      call g_scaler_reset_event(SCAL_ABORT,SCAL_err)
*
      
      call H_reset_event(HMS_ABORT,HMS_err)
      
*     
      
      call S_reset_event(SOS_ABORT,SOS_err)
      
*     
      
      call C_reset_event(COIN_ABORT,COIN_err)
      
*     
      
      call B_reset_event(BIGCAL_ABORT,BIGCAL_err)
      call SANE_reset_event(SANE_ABORT,SANE_err)
      call f1trigger_reset_event(F1Trigger_ABORT,F1Trigger_err)
      call sem_reset_event(SEM_ABORT,SEM_err)
      

      
      call GEp_reset_event(GEP_ABORT,GEP_err)
      

      abort = hms_abort.or.sos_abort.or.coin_abort.or.scal_abort
     $     .or. BIGCAL_ABORT .or. GEP_ABORT
*
      IF(ABORT) then
         err= COIN_err
         call G_prepend(SOS_err,err)
         call G_prepend(HMS_err,err)
         call G_prepend(SCAL_err,err)
         call G_prepend(BIGCAL_err,err)
         call G_prepend(BIGCAL_err,err)
         call G_prepend(SANE_err,err)
         call G_prepend(F1TRIGGER_err,err)
         call G_prepend(SEM_err,err)
         call G_prepend(GEP_err,err)
         call G_add_path(here,err)
      else
         err = ' '
      endif
*     
      RETURN
      END
