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
*-    $Log$
*-    Revision 1.13  1995/05/22 20:41:40  cdaq
*-    (SAW) Split g_init_histid into h_init_histid and s_init_histid
*-
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
      INCLUDE 'coin_filenames.cmn'
      INCLUDE 'gen_routines.dec'
      INCLUDE 'gen_pawspace.cmn'        !includes sizes of special CERNLIB space
      INCLUDE 'gen_run_info.cmn'
*HDISPLAY      include 'one_ev_io.cmn'
*HDISPLAY      include 'gen_gcbank.cmn'
*
      integer ierr
      logical HMS_ABORT,SOS_ABORT, HACK_ABORT
      character*132 HMS_err,SOS_err, HACK_err
*
      logical*4 first_time                      ! Allows routine to be called 
      data first_time /.true./                  ! by online code
      save first_time
      character*132 file
*
*--------------------------------------------------------
*
      ABORT= .FALSE.                            !clear any old flags
      err= ' '                                  !erase any old errors
      HMS_err= ' '
      SOS_err= ' '
*
*
*     Book the histograms, tests and parameters
*
      if(first_time) then
*HDISPLAY        call GZEBRA(G_size_GCBANK)      ! init GEANT memory
        call HLIMIT(G_sizeHBOOK)        !set in "gen_pawspace.cmn"
      endif
*     Load and book all the CTP files
*
*
      if((first_time.or.g_parm_rebook).and.g_ctp_parm_filename.ne.' ') then
        file = g_ctp_parm_filename
        call g_sub_run_number(file,gen_run_number)
        call thload(file)
      endif
      if((first_time.or.g_test_rebook).and.g_ctp_test_filename.ne.' ') then
        file = g_ctp_test_filename
        call g_sub_run_number(file,gen_run_number)
        call thload(file)
      endif
      if((first_time.or.g_hist_rebook).and.g_ctp_hist_filename.ne.' ') then
        file = g_ctp_hist_filename
        call g_sub_run_number(file,gen_run_number)
        call thload(file)
      endif
*     
*     Load the report definitions
*
      if((first_time.or.g_report_rebook)
     $     .and.g_report_template_filename.ne.' ') then
        file = g_report_template_filename
        call g_sub_run_number(file,gen_run_number)
        call thload(file)
      endif
*
      if((first_time.or.g_report_rebook)
     $     .and.s_report_template_filename.ne.' ') then
        file = s_report_template_filename
        call g_sub_run_number(file,gen_run_number)
        call thload(file)
      endif
*
      if((first_time.or.g_report_rebook)
     $     .and.h_report_template_filename.ne.' ') then
        file = h_report_template_filename
        call g_sub_run_number(file,gen_run_number)
        call thload(file)
      endif
*
      if((first_time.or.g_report_rebook)
     $     .and.c_report_template_filename.ne.' ') then
        file = c_report_template_filename
        call g_sub_run_number(file,gen_run_number)
        call thload(file)
      endif
*
*     Call thbook if any new files have been loaded
*
      if(first_time.or.g_parm_rebook.or.g_test_rebook
     $     .or.g_hist_rebook.or.g_report_rebook) then
        call thbook
*
*     Recalculate all histogram id's of user (hard wired) histograms
*
        call h_init_histid(ABORT,err)
        call s_init_histid(ABORT,err)
*
        if(g_alias_filename.ne.' ') then
          file = g_alias_filename
          call g_sub_run_number(file,gen_run_number)
          ierr = thwhalias(file)
          type *,'called haliaswrite',ierr
        endif
      endif
*
      call thtstclr                     ! Clear test flags
      call thtstcls                     ! Clear test scalers
*
*-HMS initialize
      call H_initialize(HMS_ABORT,HMS_err)
*
*-SOS initialize
      call S_initialize(SOS_ABORT,SOS_err)
*
      ABORT= HMS_ABORT .or. SOS_ABORT
      If(HMS_ABORT .and. .NOT.SOS_ABORT) Then
         err= HMS_err
      ElseIf(SOS_ABORT .and. .NOT.HMS_ABORT) Then
         err= SOS_err
      ElseIf(HMS_ABORT .and. SOS_ABORT) Then
         err= '&'//SOS_err
         call G_prepend(HMS_err,err)
      EndIf
*
      IF(.NOT.ABORT) THEN
*
*-COIN initialize
*
         call C_initialize(ABORT,err)
*
      ENDIF
*
      call hack_initialize(HACK_ABORT,HACK_err) ! Ignore error return for now
*
*-force reset of all space of all working arrays
*-(clear just zeros the index of each array)
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
