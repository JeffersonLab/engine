      SUBROUTINE S_reconstruction(ABORT,err)
*--------------------------------------------------------
*-       Prototype C analysis routine
*-
*-
*-   Purpose and Methods : reconstruction of HMS quantities 
*-
*-   Output: ABORT              - success or failure
*-         : err             - reason for failure, if any
*- 
*-    $Log$
*-    Revision 1.3  1994/04/13 18:30:40  cdaq
*-    (DFG) add call to s_raw_dump_all and comment out some returns after ABORT's
*-
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
      INCLUDE 'gen_data_structures.cmn'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
*
*     local variables
      integer*4 istat
*--------------------------------------------------------
*
      ABORT= .TRUE.
      err= ':no events analyzed!'
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
      call S_TRANS_SCIN(ABORT,err)
      if(ABORT)  then
         call G_add_path(here,err)
*         return
      endif                                     ! end test on SCIN ABORT

*
*      TRANSLATE CALORIMETER 
*      SOS_RAW_CAL ====> SOS_DECODED_CAL
*
      call S_TRANS_CAL(ABORT,err)
      if(ABORT)   then
         call G_add_path(here,err)
*         return
      endif                                     ! end test on CAL ABORT
*
*      TRANLATE DRIFT CHAMBERS
*      SOS_RAW_DC + SOS_DECODED_SCIN ====>  SOS_DECODED_DC
      call S_TRANS_DC(ABORT,err)
      if(ABORT) then
         call G_add_path(here,err)
         return
      endif                                     ! end test on S_TRANS_DC ABORT
*
      call S_TRACK(ABORT,err)
      if(ABORT)  then
         call G_add_path(here,err)
         return
      endif                                     ! end test on S_TRACK ABORT
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
         call S_TARG_TRANS(ABORT,err,istat)
         if(ABORT) then
            call G_add_path(here,err)
            return
         endif                                  ! end test on S_TARG_TRANS ABORT
*
*     Now begin to process particle identification information
*     First scintillator and time of flight
*     SOS_RAW_SCIN ====> SOS_TRACK_TESTS
*
         call S_TOF(ABORT,err)
         if(ABORT) then
            call G_add_path(here,err)
            return
         endif                                  ! end test of S_TOF ABORT
*      Next Calorimeter information
*      SOS_DECODED_CAL ====> SOS_TRACK_TESTS
*
         call S_CAL(ABORT,err)
         if(ABORT) then
            call G_add_path(here,err)
            return
         endif                                  ! end test of S_CAL ABORT
*     Next Cerenkov information
*     SOS_DECODED_CER ====> SOS_TRACK_TESTS
*
         call S_CER(ABORT,err)
         if(ABORT) then
            call G_add_path(here,err)
*            return
         endif                                  ! end test of S_CER ABORT
*
*     Combine results in SOS physics analysis
*     SOS_TARGET + SOS_TRACK_TESTS ====>  SOS_PHYSICS
*
         call S_PHYSICS(ABORT,err)
         if(ABORT) then
            call G_add_path(here,err)
*            return
         endif                                  ! end test of S_PHYSICS ABORT
*
      endif                                     ! end test no tracks found       
*      
*
*     Successful return
      ABORT=.FALSE.
      RETURN
      END
