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
*-    Revision 1.5  1994/06/04 02:35:59  cdaq
*-    (KBB) Make sure CTP files are non-blank before trying to thload them
*-
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
      INCLUDE 'gen_routines.dec'
      INCLUDE 'gen_pawspace.cmn'                !includes sizes of special CERNLIB space
*
      integer ierr
      logical HMS_ABORT,SOS_ABORT
      character*132 HMS_err,SOS_err
*
      logical*4 first_time                      ! Allows routine to be called 
      data first_time /.true./                  ! by online code
      save first_time
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
      if(first_time) call HLIMIT(G_sizeHBOOK)   !set in "gen_pawspace.cmn"

*     Load and book all the CTP files
*
*
      if((first_time.or.g_parm_rebook).and.g_ctp_parm_filename.ne.' ')
     $     call thload(g_ctp_parm_filename)
      if((first_time.or.g_test_rebook).and.g_ctp_test_filename.ne.' ')
     $     call thload(g_ctp_test_filename)
      if((first_time.or.g_hist_rebook).and.g_ctp_hist_filename.ne.' ')
     $     call thload(g_ctp_hist_filename)
*
      if(first_time.or.g_parm_rebook
     $     .or.g_test_rebook.or.g_hist_rebook) then
         call thbook
*
*     Recalculate all histogram id's of user (hard wired) histograms
*
         call g_init_histid(ABORT,err)
*
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
      first_time = .false.
*     
      RETURN
      END
