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
*-    Revision 1.5  1994/05/12 21:18:13  cdaq
*-    (DFG) Put h_prt_track_tests here. Remove from h_tof
*-
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
      INCLUDE 'gen_data_structures.cmn'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      include 'hms_scin_parms.cmn'
*
*     Local variables
      integer*4 istat
*--------------------------------------------------------
*
      ABORT= .TRUE.
      err= ':no events analyzed!'
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
      call H_TRANS_SCIN(ABORT,err)
      if(ABORT)  then
         call G_add_path(here,err)
*         return
      endif                             ! end test on SCIN ABORT
*
*     TRANSLATE CALORIMETER 
*     HMS_RAW_CAL ====> HMS_DECODED_CAL
*
      call H_TRANS_CAL(ABORT,err)
      if(ABORT)   then
         call G_add_path(here,err)
*         return
      endif                             ! end test on CAL ABORT
*
*     TRANLATE DRIFT CHAMBERS
*     HMS_RAW_DC + HMS_DECODED_SCIN ====>  HMS_DECODED_DC
      call H_TRANS_DC(ABORT,err)
      if(ABORT) then
         call G_add_path(here,err)
         return
      endif                             ! end test on H_TRANS_DC ABORT
      call H_TRACK(ABORT,err)
      if(ABORT)  then
         call G_add_path(here,err)
         return
      endif                             ! end test on H_TRACK ABORT
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
         call H_TARG_TRANS(ABORT,err,istat)
         if(ABORT) then
            call G_add_path(here,err)
            return
         endif                          ! end test on H_TARG_TRANS ABORT
*
*     Now begin to process particle identification information
*     First scintillator and time of flight
*     HMS_RAW_SCIN ====> HMS_TRACK_TESTS
*
         call H_TOF(ABORT,err)
         if(ABORT) then
            call G_add_path(here,err)
            return
         endif                          ! end test of H_TOF ABORT
*     Next Calorimeter information
*     HMS_DECODED_CAL ====> HMS_TRACK_TESTS
*
         call H_CAL(ABORT,err)
         if(ABORT) then
            call G_add_path(here,err)
*            return
         endif                          ! end test of H_CAL ABORT
*     Next Cerenkov information
*     HMS_DECODED_CER ====> HMS_TRACK_TESTS
*
         call H_CER(ABORT,err)
         if(ABORT) then
            call G_add_path(here,err)
*            return
         endif                          ! end test of H_CER ABORT
*

*     Dump HMS_TRACK_TESTS if hdebugprinttracktests is set
          if( hdebugprinttracktests .ne. 0 ) then
            call h_prt_track_tests
          endif
*
*     Combine results in HMS physics analysis
*     HMS_TARGET + HMS_TRACK_TESTS ====>  HMS_PHYSICS
*
         call H_PHYSICS(ABORT,err)
         if(ABORT) then
            call G_add_path(here,err)
            return
         endif                          ! end test of H_PHYSICS ABORT
*
      endif                             ! end test no tracks found       
*      
*
*     Successful return
      ABORT=.FALSE.
      RETURN
      END
