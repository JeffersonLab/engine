      SUBROUTINE G_proper_shutdown(ABORT,err)
*--------------------------------------------------------
*-       Prototype C analysis routine
*-
*-
*-   Purpose and Methods : Closes files properly, flushes, etc.
*- 
*-   Output: ABORT		- success or failure
*-         : err	- reason for failure, if any
*- 
*-   Created  20-Nov-1993   Kevin B. Beard for new error standards
*-    $Log$
*-    Revision 1.5  1994/08/04 03:45:46  cdaq
*-    (SAW) Add call to Breuer's hack_shutdown
*-
* Revision 1.4  1994/06/22  19:49:31  cdaq
* (SAW) Create report file and append g_report_template to it
*
* Revision 1.3  1994/06/14  19:13:20  cdaq
* (SAW) Move histogram saving to new routine g_dump_histograms
*
* Revision 1.2  1994/04/15  20:36:49  cdaq
* (KBB) Add ntuple handling
*
* Revision 1.1  1994/02/04  22:12:15  cdaq
* Initial revision
*
*-
*- All standards are from "Proposal for Hall C Analysis Software
*- Vade Mecum, Draft 1.0" by D.F.Geesamn and S.Wood, 7 May 1993
*-
*-
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*17 here
      parameter (here= 'G_proper_shutdown')
*
      logical ABORT
      character*(*) err
*
      logical bad_report,bad_HMS,bad_SOS,bad_COIN,bad_HBK,bad_hack
      character*132 err_report,err_HMS,err_SOS,err_COIN,err_HBK,err_hack
*
      include 'gen_filenames.cmn'
      include 'gen_routines.dec'
      include 'gen_run_info.cmn'
*
      integer SPAREID
      parameter (SPAREID=67)
*     
      integer ierr
*--------------------------------------------------------
      bad_report = .TRUE.
      err_report = 'Failed to open report file'
      open(unit=SPAREID,type='UNKNOWN',access='SEQUENTIAL',
     $     file=g_report_output_filename,err=999)
      write(SPAREID,'("Run #",i6)') gen_run_number
      close(unit=SPAREID)               ! Just get the file created
*
      bad_report = .false.
      err_report = ' '
*
      ierr = threpa(g_report_blockname,g_report_output_filename)
      if(ierr.ne.0) then
         bad_report = .true.
         err = 'threpa failed to append report'
      endif
*
 999  continue
*
*-chance to flush any statistics, etc.
      call H_proper_shutdown(bad_HMS,err_HMS)
*     
      call S_proper_shutdown(bad_SOS,err_SOS)
*     
      call C_proper_shutdown(bad_COIN,err_COIN)
*
      call hack_shutdown(bad_hack,err_hack)
*
      call g_dump_histograms(bad_HBK,err_HBK)
*
      ABORT= bad_HMS .or. bad_SOS .or. bad_COIN .or. bad_HBK
     $     .or. bad_report
      err= ' '
      IF(ABORT) THEN                    !assemble error message
         if(bad_report) err = err_report
         If(bad_HBK) Then
            call G_prepend(err_HBK//' &',err)
         elseif (bad_HBK) then
            err= err_HBK
         EndIf
         If(bad_COIN .and. err.NE.' ') Then
            call G_prepend(err_COIN//' &',err)
         ElseIf(bad_COIN) Then
            err= err_COIN
         EndIf
         If(bad_SOS .and. err.NE.' ') Then
            call G_prepend(err_SOS//' &',err)
         ElseIf(bad_SOS) Then
            err= err_SOS
         EndIf
         If(bad_HMS .and. err.NE.' ') Then
            call G_prepend(err_HMS//' &',err)
         ElseIf(bad_HMS) Then
            err= err_HMS
         EndIf
         call G_add_path(here,err)
      ENDIF
      RETURN
      END
