       SUBROUTINE H_reconstruction(ABORT,err)
*--------------------------------------------------------
*-       Prototype C analysis routine
*-
*-
*-   Purpose and Methods : reconstruction of HMS quantities 
*-
*-   Output: ABORT              - success or failure
*-         : err             - reason for failure, if any
*- 
*-   Created  8-Nov-1993   Kevin B. Beard, HU
*-   Modified 20-Nov-1993   KBB for new errors
*-    $Log$
*-    Revision 1.10  1995/08/31 14:46:06  cdaq
*-    (JRA) Add call to h_trans_cer
*-
* Revision 1.9  1995/05/22  19:39:25  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.8  1995/05/11  19:05:48  cdaq
* (JRA) Add call to h_trans_misc
*
* Revision 1.7  1995/02/02  13:06:13  cdaq
* (SAW) Add call to h_select_best_track
*
* Revision 1.6  1994/06/06  16:49:49  cdaq
* (DFG) add h_recon_num and bypass switches
*
* Revision 1.5  1994/05/12  21:18:13  cdaq
* (DFG) Put h_prt_track_tests here. Remove from h_tof
*
* Revision 1.4  1994/04/13  16:06:00  cdaq
* (DFG) Add consolidated call to h_raw_dump_all
*       Commented out returns after ABORT's
*
* Revision 1.3  1994/02/22  15:51:33  cdaq
* (DFG) Replace with real version
* (SAW) Move to TRACKING directory
*
* Revision 1.2  1994/02/04  20:49:31  cdaq
* Print out some raw hit data
*
* Revision 1.1  1994/02/04  20:47:59  cdaq
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
      character*16 here
      parameter (here= 'H_reconstruction')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 'hms_data_structures.cmn'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      include 'hms_scin_parms.cmn'
      include 'hms_bypass_switches.cmn'
      include 'hms_statistics.cmn'
*
*     Local variables
      integer*4 istat
*--------------------------------------------------------
*
      ABORT= .TRUE.
      err= ':no events analyzed!'
*
* increment reconstructed number
      h_recon_num= h_recon_num + 1
*
*     dump
      call h_raw_dump_all(ABORT,err)
      if(ABORT) then
         call g_add_path(here,err)
         return
      endif                             ! end test on h_raw_dump_all ABORT
*
*     TRANSLATE SCINTILATORS AND CALCULATE START TIME
*     HMS_RAW_SCIN ====> HMS_DECODED_SCIN
*
      If(hbypass_trans_scin.eq.0) then
         call H_TRANS_SCIN(ABORT,err)
         if(ABORT)  then
            call G_add_path(here,err)
*     return
         endif                          ! end test on SCIN ABORT
      endif                             ! end test on hbypass_trans_scin
*
*     TRANSLATE HMISC TDC HITS.
*     H_RAW_MISC ====> HMS_DECODED_MISC
*
      If(hbypass_trans_scin.eq.0) then
         call H_TRANS_MISC(ABORT,err)
         if(ABORT)  then
            call G_add_path(here,err)
*     return
         endif                          ! end test on SCIN ABORT
      endif                             ! end test on hbypass_trans_scin
*
*     TRANSLATE CERENKOV
*     HMS_RAW_CER ====> HMS_DECODED_CER
*
      If(hbypass_trans_cer.eq.0) then
         call H_TRANS_CER(ABORT,err)
         if(ABORT)  then
            call G_add_path(here,err)
*     return
         endif                          ! end test on CER ABORT
      endif                             ! end test on hbypass_trans_cer
*
*     TRANSLATE CALORIMETER 
*     HMS_RAW_CAL ====> HMS_DECODED_CAL
*
      if(hbypass_trans_cal.eq.0) then
         call H_TRANS_CAL(ABORT,err)
         if(ABORT)   then
            call G_add_path(here,err)
*     return
         endif                          ! end test on CAL ABORT
      endif                             ! end test on hbypass_trans_cal
*     
*     TRANLATE DRIFT CHAMBERS
*     HMS_RAW_DC + HMS_DECODED_SCIN ====>  HMS_DECODED_DC
      if(hbypass_trans_dc.eq.0) then
         call H_TRANS_DC(ABORT,err)
         if(ABORT) then
            call G_add_path(here,err)
            return
         endif                          ! end test on H_TRANS_DC ABORT
      endif                             ! end test on hbypass_trans_dc
      if(hbypass_track.eq.0) then
         call H_TRACK(ABORT,err)
         if(ABORT)  then
            call G_add_path(here,err)
            return
         endif                          ! end test on H_TRACK ABORT
      endif                             ! end test on hbypass_track
*     only proceed if the number of tracks is greater than one
*     
      if(HNTRACKS_FP .lt. 1) then
         ABORT=.FALSE.
         err=":no tracks found!"
         return
      else
*     Proceed if one or more track has been found
*     
*     Project tracks back to target
*     HMS_FOCAL_PLANE  ====>  HMS_TARGET
*     
         if(hbypass_targ_trans.eq. 0) then
            call H_TARG_TRANS(ABORT,err,istat)
            if(ABORT) then
               call G_add_path(here,err)
               return
            endif                       ! end test on H_TARG_TRANS ABORT
         endif                          ! end test on hbypass_target_trans
*     
*     Now begin to process particle identification information
*     First scintillator and time of flight
*     HMS_RAW_SCIN ====> HMS_TRACK_TESTS
*
         if(hbypass_tof.eq.0) then
            call H_TOF(ABORT,err)
            if(ABORT) then
               call G_add_path(here,err)
               return
            endif                       ! end test of H_TOF ABORT
         endif                          ! end test on hbypass_tof
*     Next Calorimeter information
*     HMS_DECODED_CAL ====> HMS_TRACK_TESTS
*
         if(hbypass_cal.eq.0) then
            call H_CAL(ABORT,err)
            if(ABORT) then
               call G_add_path(here,err)
*     return
            endif                       ! end test of H_CAL ABORT
         endif                          ! end test on hbypass_cal
*     Next Cerenkov information
*     HMS_DECODED_CER ====> HMS_TRACK_TESTS
*     
         if(hbypass_cer.eq.0) then
            call H_CER(ABORT,err)
            if(ABORT) then
               call G_add_path(here,err)
*     return
            endif                       ! end test of H_CER ABORT
         endif                          ! end test on hbypass_cer
*     
         
*     Dump HMS_TRACK_TESTS if hdebugprinttracktests is set
         if( hdebugprinttracktests .ne. 0 ) then
            call h_prt_track_tests
         endif
*     
*     Combine results in HMS physics analysis
*     HMS_TARGET + HMS_TRACK_TESTS ====>  HMS_PHYSICS
*     
         if(hbypass_track.eq.0) then
           call h_select_best_track(abort,err)
           if(ABORT) then
             call G_add_path(here,err)
             return
           endif
         endif
*
         if(hbypass_physics.eq.0) then
            call h_physics(abort,err)
            if(ABORT) then
               call G_add_path(here,err)
               return
            endif                       ! end test of H_PHYSICS ABORT
         endif                          ! end test on hbypass_physics
*     
      endif                             ! end test no tracks found       
*     
*
*     Successful return
      ABORT=.FALSE.
      RETURN
      END
