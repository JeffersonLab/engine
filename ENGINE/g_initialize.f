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
*-    Revision 1.2  1994/02/11 18:34:49  cdaq
*-    Split off CTP variables registration from initialize routines
*-
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
      INCLUDE 'gen_routines.dec'
      INCLUDE 'gen_pawspace.cmn'                !includes sizes of special CERNLIB space
*
      integer ierr
      logical HMS_ABORT,SOS_ABORT
      character*132 HMS_err,SOS_err
*
*--------------------------------------------------------
*
      ABORT= .FALSE.                            !clear any old flags
      err= ' '                                  !erase any old errors
      HMS_err= ' '
      SOS_err= ' '
*
*     Book the histograms, tests and parameters
*
*
*-all crucial setup information here; failure is fatal
      g_hist_rebook = .false.
      g_test_rebook = .false.
      g_parm_rebook = .false.
      g_ctp_parm_filename = ' '
      g_ctp_test_filename = ' '
      g_ctp_hist_filename = ' '
      g_data_source_filename= ' '        !undefined
      g_alias_filename = ' '
      g_histout_filename = ' '
      g_decode_map_filename = ' '
      call G_init_filenames(ABORT,err,g_config_environmental_var)
*
*-attempt to open FASTBUS-CODA file
      g_data_source_opened = .false.     !not opened yet
      g_data_source_in_hndl= 0           !none
      IF(.NOT.ABORT) call G_open_source(ABORT,err)
*
*-attempt to clear map of FASTBUS-CODA setup file
      IF(.NOT.ABORT) call G_decode_clear(ABORT,err)
*
*-attempt to read decoding of FASTBUS-CODA setup file
*     In online code we will not want to do this at the start of every run.
      IF(.NOT.ABORT) call G_decode_init(ABORT,err)
*
      IF(.NOT.ABORT) THEN
*
         call HLIMIT(G_sizeHBOOK)               !set in "gen_pawspace.cmn"

*     Load and book all the CTP files
*
*
         if(g_ctp_parm_filename.ne.' ') g_parm_rebook = .true.
         if(g_ctp_test_filename.ne.' ') g_test_rebook = .true.
         if(g_ctp_hist_filename.ne.' ') g_hist_rebook = .true.
         if(g_parm_rebook) call thload(g_ctp_parm_filename)
         if(g_test_rebook) call thload(g_ctp_test_filename)
         if(g_hist_rebook) call thload(g_ctp_hist_filename)
*
         if(g_parm_rebook .or. g_test_rebook .or. g_hist_rebook) then
            call thbook
            if(g_alias_filename.ne.' ') then
               ierr = thwhalias(g_alias_filename)
               type *,'called haliaswrite',ierr
            endif
         endif

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
      ENDIF
*
      IF(.NOT.ABORT) THEN
*
*-COIN initialize
*
         call C_initialize(ABORT,err)
*
      ENDIF
*
*-force reset of all space of all working arrays
*-(clear just zeros the index of each array)
      IF(.NOT.ABORT) THEN
*
         call G_reset_event(ABORT,err)
*
      ENDIF
*
      IF(ABORT .or. err.NE.' ') call G_add_path(here,err)
*
      RETURN
      END


