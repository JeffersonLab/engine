      SUBROUTINE S_reconstruction(ABORT,err)
*--------------------------------------------------------
*-       Prototype C analysis routine
*-
*-
*-   Purpose and Methods : reconstruction of SOS quantities 
*-
*-   Output: ABORT              - success or failure
*-         : err             - reason for failure, if any
*- 
*-    $Log$
*-    Revision 1.8  1995/05/22 19:45:54  cdaq
*-    (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*-
* Revision 1.7  1995/05/11  21:07:26  cdaq
* (JRA) Add call to s_trans_misc
*
* Revision 1.6  1995/04/06  19:42:47  cdaq
* (JRA) Add call to s_select_best_track before s_physics
*
* Revision 1.5  1994/06/07  04:46:21  cdaq
* (DFG) add s_recon_num and bypass switches
*
* Revision 1.4  1994/05/13  03:34:52  cdaq
* (DFG) Put s_prt_track_tests here. Remove from s_tof
*
* Revision 1.3  1994/04/13  18:30:40  cdaq
* (DFG) add call to s_raw_dump_all and comment out some returns after ABORT's
*
* Revision 1.2  1994/02/22  15:56:17  cdaq
* (DFG) Replace with real version
* (SAW) Move to TRACKING directory
*
* Revision 1.1  1994/02/04  22:16:44  cdaq
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
      parameter (here= 'S_reconstruction')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 'sos_data_structures.cmn'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      include 'sos_scin_parms.cmn'
      include 'sos_bypass_switches.cmn'
      include 'sos_statistics.cmn'
*
*     local variables
      integer*4 istat
*--------------------------------------------------------
*
      ABORT= .TRUE.
      err= ':no events analyzed!'
* increment reconstructed number
      s_recon_num= s_recon_num + 1
*
*     dump all raw data
      call s_raw_dump_all(ABORT,err)
      if(ABORT) then
         call g_add_path(here,err)
         return
      endif
*
*     TRANSLATE SCINTILATORS AND CALCULATE START TIME
*     SOS_RAW_SCIN ====> SOS_DECODED_SCIN
*     
      If(sbypass_trans_scin.eq.0) then
        call S_TRANS_SCIN(ABORT,err)
        if(ABORT)  then
           call G_add_path(here,err)
*          return
        endif                                     ! end test on SCIN ABORT
      endif                              ! end test on sbypass_trans_scin
*
*     TRANSLATE SMISC TDC HITS.
*     S_RAW_MISC ====> SOS_DECODED_MISC
*
      If(sbypass_trans_scin.eq.0) then
         call S_TRANS_MISC(ABORT,err)
         if(ABORT)  then
            call G_add_path(here,err)
*     return
         endif                          ! end test on SCIN ABORT
      endif                             ! end test on hbypass_trans_scin
*
*
*      TRANSLATE CALORIMETER 
*      SOS_RAW_CAL ====> SOS_DECODED_CAL
*
      if(sbypass_trans_cal.eq.0) then
        call S_TRANS_CAL(ABORT,err)
        if(ABORT)   then
         call G_add_path(here,err)
*         return
        endif                                     ! end test on CAL ABORT
      endif                                    ! end test on sbypass_trans_cal
*
*      TRANLATE DRIFT CHAMBERS
*      SOS_RAW_DC + SOS_DECODED_SCIN ====>  SOS_DECODED_DC
      if(sbypass_trans_dc.eq.0) then
        call S_TRANS_DC(ABORT,err)
        if(ABORT) then
          call G_add_path(here,err)
          return
        endif                                     ! end test on S_TRANS_DC ABORT
      endif                                     ! end test on sbypass_trans_dc
*
      if(sbypass_track.eq.0) then
        call S_TRACK(ABORT,err)
        if(ABORT)  then
           call G_add_path(here,err)
           return
        endif                                     ! end test on S_TRACK ABORT
      endif                               ! end test on sbypass_track
*     only proceed if the number of tracks is greater than one
*
      if(SNTRACKS_FP .lt. 1) then
         ABORT=.FALSE.
         err=":no tracks found!"
         return
      else
*     Proceed if one or more track has been found
*
*     Project tracks back to target
*     SOS_FOCAL_PLANE  ====>  SOS_TARGET
*
       if(sbypass_targ_trans.eq. 0) then
         call S_TARG_TRANS(ABORT,err,istat)
         if(ABORT) then
            call G_add_path(here,err)
            return
         endif                                  ! end test on S_TARG_TRANS ABORT
       endif                                 ! end test on sbypass_target_trans
*
*     Now begin to process particle identification information
*     First scintillator and time of flight
*     SOS_RAW_SCIN ====> SOS_TRACK_TESTS
*
       if(sbypass_tof.eq.0) then
         call S_TOF(ABORT,err)
         if(ABORT) then
            call G_add_path(here,err)
            return
         endif                                  ! end test of S_TOF ABORT
       endif                                    ! end test on sbypass_tof
*      Next Calorimeter information
*      SOS_DECODED_CAL ====> SOS_TRACK_TESTS
*
       if(sbypass_cal.eq.0) then
         call S_CAL(ABORT,err)
         if(ABORT) then
            call G_add_path(here,err)
            return
         endif                                  ! end test of S_CAL ABORT
       endif                                    ! end test on sbypass_cal
*     Next Cerenkov information
*     SOS_DECODED_CER ====> SOS_TRACK_TESTS
*
       if(sbypass_cer.eq.0) then
         call S_CER(ABORT,err)
         if(ABORT) then
            call G_add_path(here,err)
*            return
         endif                                  ! end test of S_CER ABORT
       endif                                    ! end test on sbypass_cer
*
*     Dump SOS_TRACK_TESTS if sdebugprinttracktests is set
          if( sdebugprinttracktests .ne. 0 ) then
            call s_prt_track_tests
          endif
*     Combine results in SOS physics analysis
*     SOS_TARGET + SOS_TRACK_TESTS ====>  SOS_PHYSICS
*
         if(sbypass_track.eq.0) then
           call s_select_best_track(abort,err)
           if(ABORT) then
             call G_add_path(here,err)
             return
           endif
         endif
*
        if(sbypass_physics.eq.0) then
         call S_PHYSICS(ABORT,err)
         if(ABORT) then
            call G_add_path(here,err)
*            return
         endif                                  ! end test of S_PHYSICS ABORT
        endif                                   ! end test on sbypass_physics
*
      endif                                     ! end test no tracks found       
*      
*
*     Successful return
      ABORT=.FALSE.
      RETURN
      END
