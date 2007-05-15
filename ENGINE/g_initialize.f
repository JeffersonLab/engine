      SUBROUTINE G_initialize(ABORT,err)
*----------------------------------------------------------------------
*-       Prototype hall C initialize routine
*- 
*-   Purpose and Methods : Initialization is performed and status returned
*- 
*-   Output: ABORT      - success or failure
*-         : err        - reason for failure, if any
*- 
*-   Created   9-Nov-1993   Kevin B. Beard
*-   Modified 20-Nov-1993   Kevin B. Beard
* $Log$
* Revision 1.24.6.1  2007/05/15 02:55:01  jones
* Start to Bigcal code
*
* Revision 1.23  2004/05/11 18:24:12  jones
* Initialize skip_events to false
*
* Revision 1.22  2003/09/05 15:48:36  jones
* Merge in online03 changes (mkj)
*
* Revision 1.21.2.3  2003/08/14 00:42:22  cdaq
* Modify to be able to write scaler rates for each read to a file (mkj)
*
* Revision 1.21.2.2  2003/04/10 00:41:27  cdaq
* Added gen_data_structures and included status messages when kinematics overridden
*
* Revision 1.21.2.1  2003/04/09 23:56:27  cdaq
* Check for gpbeam=0
*
* Revision 1.21  1996/11/05 21:41:36  saw
* (SAW) Use CTP routines as functions rather than subroutines for
* porting.
*
* Revision 1.20  1996/09/04 14:37:56  saw
* (JRA) Open output file for charge scalers
*
* Revision 1.19  1996/04/29 19:47:42  saw
* (JRA) Add call to engine_command_line
*
* Revision 1.18  1996/01/22 15:18:12  saw
* (JRA) Add call to g_target_initialize.  Remove call to
* g_kludge_up_kinematics
*
* Revision 1.17  1996/01/16 18:24:47  cdaq
* (JRA) Get kinematics for runinfo event, create a tcl stats screen.  Groupify
*       CTP calls
*
* Revision 1.16  1995/10/09 18:42:57  cdaq
* (SAW) Move loading of ctp_kinematics database to before CTP loading.  Take
* ntuple inialization out of spec specific init routines into a all ntuple
* init routine.
*
* Revision 1.15  1995/09/01 14:29:41  cdaq
* (JRA) Zero run time variable, read kinematics database after last book
*
* Revision 1.14  1995/07/27  19:36:41  cdaq
* (SAW) Relocate data statements for f2c compatibility, check error returns
*       on thload calls and quit if important files are missing.
*
* Revision 1.13  1995/05/22  20:41:40  cdaq
* (SAW) Split g_init_histid into h_init_histid and s_init_histid
*
* Revision 1.12  1995/04/01  19:47:22  cdaq
* (SAW) One report file for each of g, h, s, c instead of a single report file
*       Allow %d for run number in filenames
*
* Revision 1.11  1994/10/11  18:39:40  cdaq
* (SAW) Add some hacks for event display
*
* Revision 1.10  1994/09/21  19:52:57  cdaq
* (SAW) Cosmetic change
*
* Revision 1.9  1994/08/30  14:47:41  cdaq
* (SAW) Add calls to clear the test flags and scalers
*
* Revision 1.8  1994/08/18  03:45:01  cdaq
* (SAW) Correct typo in adding hack stuff
*
* Revision 1.7  1994/08/04  03:08:11  cdaq
* (SAW) Add call to Breuer's hack_initialize
*
* Revision 1.6  1994/06/22  20:55:14  cdaq
* (SAW) Load report templates
*
* Revision 1.5  1994/06/04  02:35:59  cdaq
* (KBB) Make sure CTP files are non-blank before trying to thload them
*
* Revision 1.4  1994/04/12  20:59:21  cdaq
* (SAW) Add call to calculation of histid's for hfilled histograms
*
* Revision 1.3  1994/03/24  22:02:31  cdaq
* Reorganize for online compatibility
*
* Revision 1.2  1994/02/11  18:34:49  cdaq
* Split off CTP variables registration from initialize routines
*
* Revision 1.1  1994/02/04  22:00:26  cdaq
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
      character*12 here
      parameter (here= 'G_initialize')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 'gen_filenames.cmn'               !all setup files
      INCLUDE 'hms_filenames.cmn'
      INCLUDE 'sos_filenames.cmn'
      include 'bigcal_filenames.cmn' ! add BigCal
      INCLUDE 'coin_filenames.cmn'
      include 'gep_filenames.cmn' ! add GEp
      INCLUDE 'gen_routines.dec'
      INCLUDE 'gen_pawspace.cmn'        !includes sizes of special CERNLIB space
      INCLUDE 'gen_run_info.cmn'
      include 'gen_scalers.cmn'
      include 'hms_data_structures.cmn'
      include 'sos_data_structures.cmn'
      include 'gen_data_structures.cmn'
      include 'bigcal_data_structures.cmn' ! add BigCal
*
      integer ierr
      logical HMS_ABORT,SOS_ABORT, HACK_ABORT, BIGCAL_ABORT ! add flag for BigCal
      character*132 HMS_err,SOS_err, HACK_err, BIGCAL_err ! add err. message for BigCal
*
      character*132 file
      logical*4 first_time                      ! Allows routine to be called 
      save first_time
      data first_time /.true./                  ! by online code
*
*--------------------------------------------------------
*
      ABORT= .FALSE.                            !clear any old flags
      err= ' '                                  !erase any old errors
      HMS_err= ' '
      SOS_err= ' '
      BIGCAL_err = ' '
*
* set the runtime variable to avoid divide by zero during report
*
*      g_run_time = 0.0001
*
*     Book the histograms, tests and parameters
*
      if(first_time) then
        call HLIMIT(G_sizeHBOOK)        !set in "gen_pawspace.cmn"
      endif
*     Load and book all the CTP files
*
*
      if((first_time.or.g_parm_rebook).and.g_ctp_parm_filename.ne.' ') then
        file = g_ctp_parm_filename
        call g_sub_run_number(file,gen_run_number)
        if(thload(file).ne.0) then
          ABORT = .true.
          err = file
        endif
        ierr = thbook()                 ! Assert parm values
      endif                             ! so that ctp_database can override
*
*
*     Now if there is a g_ctp_kinematics_filename set, pass the run number
*     to it to set CTP variables.  Parameters placed in this file will
*     override values defined in the CTP input files.
*
      if(.not.ABORT.and.g_ctp_kinematics_filename.ne.' ') then
        write(6,'(a,a60)') 'KINEMATICS FROM ',g_ctp_kinematics_filename(1:60)
        call g_ctp_database(ABORT, err
     $       ,gen_run_number, g_ctp_kinematics_filename)
        IF(ABORT) THEN
          call G_add_path(here,err)
        endif
      ENDIF
*
      if((first_time.or.g_test_rebook).and.g_ctp_test_filename.ne.' ') then
        file = g_ctp_test_filename
        call g_sub_run_number(file,gen_run_number)
        if(thload(file).ne.0) then
          ABORT = .true.
          if(err.ne.' ') then
            call g_append(err,' & '//file)
          else
            err = file
          endif
        endif
      endif

      write(6,'(a)') 'COMMAND LINE FLAGS'
      call engine_command_line(.true.)  ! Reset CTP vars from command line

* that was the last call to engine_command_line, the last time to input
* ctp variables.  Set some here to avoid divide by zero errors if they
* were not read in.
      if (hpcentral.le.0.001) then
         hpcentral = 1.
         write(6,*) 'hpcentral value not given: setting to 1 GeV'
      endif
      if (spcentral.le.0.001) then
         spcentral = 1.
         write(6,*) 'spcentral value not given: setting to 1 GeV'
      endif
      if (htheta_lab.le.0.001) then
         htheta_lab = 90.
         write(6,*) 'htheta_lab value not given: setting to 90 degrees'
      endif
      if (stheta_lab.le.0.001) then
         stheta_lab = 90.
         write(6,*) 'stheta_lab value not given: setting to 90 degrees'
      endif
      if (gpbeam.le.0.001) then
         gpbeam = 2.
         write(6,*) 'gpbeam value not given: setting to 2 GeV'
      endif
c     avoid divide-by-zero errors for BigCal: 
      if(BIGCAL_THETA_DEG.le.0.001) then
         BIGCAL_THETA_DEG = 90.
         write(6,*) 'bigcal_theta_deg value not given: set to 90 deg.'
      endif
      if(BIGCAL_R_TGT.le.0.001) then
         BIGCAL_R_TGT = 1000.
         write(6,*) 'bigcal_r_tgt value not given: setting to 10.0 m'
      endif

      if((first_time.or.g_hist_rebook).and.g_ctp_hist_filename.ne.' ') then
        file = g_ctp_hist_filename
        call g_sub_run_number(file,gen_run_number)
        if(thload(file).ne.0) then
          ABORT = .true.
          if(err.ne.' ') then
            call g_append(err,' & '//file)
          else
            err = file
          endif
        endif
      endif
*
      if(ABORT) then
        call g_add_path(here,err)
        return                          ! Don't try to proceed
      endif
      
*     
*     Load the report definitions
*

      if((first_time.or.g_report_rebook)
     $     .and.g_report_template_filename.ne.' ') then
        file = g_report_template_filename
        call g_sub_run_number(file,gen_run_number)
        ierr = thload(file)
      endif
*

      if((first_time.or.g_report_rebook)
     $     .and.g_stats_template_filename.ne.' ') then
        file = g_stats_template_filename
        call g_sub_run_number(file,gen_run_number)
        ierr = thload(file)
      endif
*
      if((first_time.or.g_report_rebook)
     $     .and.s_report_template_filename.ne.' ') then
        file = s_report_template_filename
        call g_sub_run_number(file,gen_run_number)
        ierr = thload(file)
      endif
*
      if((first_time.or.g_report_rebook)
     $     .and.h_report_template_filename.ne.' ') then
        file = h_report_template_filename
        call g_sub_run_number(file,gen_run_number)
        ierr = thload(file)
      endif
*
      if((first_time.or.g_report_rebook)
     $     .and.c_report_template_filename.ne.' ') then
        file = c_report_template_filename
        call g_sub_run_number(file,gen_run_number)
        ierr = thload(file)
      endif

c$$$      if((first_time.or.g_report_rebook).and. ! add BigCal
c$$$     $     b_report_template_filename.ne.' ') then
c$$$         file = b_report_template_filename
c$$$         call g_sub_run_number(file,gen_run_number)
c$$$         ierr = thload(file)
c$$$      endif
c$$$
c$$$      if((first_time.or.g_report_rebook).and. ! add GEp-coin.
c$$$     $     gep_report_template_filename.ne.' ') then
c$$$         file = gep_report_template_filename
c$$$         call g_sub_run_number(file,gen_run_number)
c$$$         ierr = thload(file)
c$$$      endif
*
*     Call thbook if any new files have been loaded
*
      if(first_time.or.g_parm_rebook.or.g_test_rebook
     $     .or.g_hist_rebook.or.g_report_rebook) then
        ierr = thbook()
*
*     Recalculate all histogram id's of user (hard wired) histograms
*
        call h_init_histid(ABORT,err)
        call s_init_histid(ABORT,err)
        call b_init_histid(ABORT,err) ! add bigcal
*        call gep_init_histid(ABORT,err) ! add GEp-coin
*
        if(g_alias_filename.ne.' ') then
          file = g_alias_filename
          call g_sub_run_number(file,gen_run_number)
          ierr = thwhalias(file)
          if (ierr.ne.0) print *,'called haliaswrite',ierr
        endif
      endif
*
      call thtstclrg("default")                     ! Clear test flags
      call thtstclsg("default")                     ! Clear test scalers
*
      call g_target_initialize(ABORT,err)

* Open output file for charge scalers.
      if (g_charge_scaler_filename.ne.' ') then
        file=g_charge_scaler_filename
        call g_sub_run_number(file,gen_run_number)
        open(unit=G_LUN_CHARGE_SCALER,file=file,status='unknown')
        write(G_LUN_CHARGE_SCALER,*) '!Charge scalers - Run #',gen_run_number
        write(G_LUN_CHARGE_SCALER,*) '!event   Unser(Hz)     BCM1(Hz)     BCM2(Hz)',
     &               '     BCM3(Hz)     Time(s)'
      endif

c
      skip_events = .false.
* Open output file to writeout scalers.
      if (g_writeout_scaler_filename.ne.' ') then
        if ( NUM_WRITEOUT_SCALERS .le. MAX_WRITEOUT_SCALERS) then
         file=g_writeout_scaler_filename
         call g_sub_run_number(file,gen_run_number)
         open(unit=G_LUN_WRITEOUT_SCALER,file=file,status='unknown')
        else
           write(*,*) ' Asking to write out ',NUM_WRITEOUT_SCALERS,' scalers'
           write(*,*) ' Maximum is  ' ,MAX_WRITEOUT_SCALERS
           WRITE(*,*) ' Modify  MAX_WRITEOUT_SCALERS in INCLUDE/gen_scalers and recompile code'
           g_writeout_scaler_filename = ' '
        endif
      endif

* Open output file for epics events.
      if (g_epics_output_filename.ne.' ') then
        file=g_epics_output_filename
        call g_sub_run_number(file,gen_run_number)
        open(unit=G_LUN_EPICS_OUTPUT,file=file,status='unknown')
      endif

      write(*,*) 'about to call h_initialize'

*-HMS initialize
c      call H_initialize(HMS_ABORT,HMS_err)
*
*-SOS initialize
c      call S_initialize(SOS_ABORT,SOS_err)

      write(*,*) 'about to call b_initialize'
*
*-BigCal initialize
      call B_initialize(BIGCAL_ABORT,BIGCAL_err)
*
      ABORT= HMS_ABORT .or. SOS_ABORT .or. BIGCAL_ABORT
      If(HMS_ABORT .and. .NOT.(SOS_ABORT.or.BIGCAL_ABORT)) Then
         err= HMS_err
      ElseIf(SOS_ABORT .and. .NOT.(HMS_ABORT.or.BIGCAL_ABORT)) Then
         err= SOS_err
      ElseIf(BIGCAL_ABORT.and. .not.(HMS_ABORT.or.SOS_ABORT)) then
         err = BIGCAL_err
      ElseIf(HMS_ABORT.and.SOS_ABORT.and.(.not.BIGCAL_ABORT)) Then
         err= '&'//SOS_err
         call G_prepend(HMS_err,err)
      ElseIf(HMS_ABORT.and.BIGCAL_ABORT.and.(.not.SOS_ABORT)) Then
         err= '&'//BIGCAL_err
         call G_prepend(HMS_err,err)
      ElseIf(BIGCAL_ABORT.and.SOS_ABORT.and.(.not.HMS_ABORT)) Then
         err= '&'//BIGCAL_err
         call G_prepend(SOS_err,err)
      ElseIf(BIGCAL_ABORT.and.SOS_ABORT.and.HMS_ABORT) Then
         err= '&'//SOS_err//'&'//BIGCAL_err
         call G_prepend(HMS_err,err)
      EndIf
*
      write(*,*) 'about to call C_initialize'
      IF(.NOT.ABORT) THEN
*     
*-COIN initialize
*     
         call C_initialize(ABORT,err)
*
      ENDIF
*
      write(*,*) 'about to call GEP_initialize'
      if(.not.ABORT) then
         call GEP_initialize(ABORT,err) ! clone of C_initialize for now
      endif
      
      write(*,*) 'about to call g_ntuple_init'
      call g_ntuple_init(HACK_ABORT,HACK_err) ! Ingore error return for now
*
      write(*,*) 'about to call hack_initialize'
      call hack_initialize(HACK_ABORT,HACK_err) ! Ignore error return for now
*
*-force reset of all space of all working arrays
*-(clear just zeros the index of each array)
      write(*,*) 'about to call g_reset_event'
      IF(.NOT.ABORT) THEN
         call G_reset_event(ABORT,err)
*
      ENDIF
*
      IF(ABORT .or. err.NE.' ') call G_add_path(here,err)
*
      first_time = .false.
*     
      RETURN
      END
