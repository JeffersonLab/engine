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
* $Log$
* Revision 1.12.4.1  2004/10/01 18:16:01  jones
* fpi2 branch update. Add separate report file for gscaler_saved produced with syncfilter.
*
* Revision 1.14  2004/07/08 20:09:47  saw
* Flush CTP Root trees
*
* Revision 1.13  2004/02/17 17:27:10  jones
* Only dump histograms when g_histout_filename is set.
*
* Revision 1.12  1999/02/23 18:24:23  csa
* (JRA) Remove debugcalcpeds stuff, cleanup
*
* Revision 1.11  1996/09/04 14:40:05  saw
* (JRA) Get filename for "bad" report from a ctp variable
*
* Revision 1.10  1995/10/09 18:44:27  cdaq
* (JRA) Only write pedestal file if appropriate control flag(s) set.
*
* Revision 1.9  1995/09/01 15:46:41  cdaq
* (JRA) Open temp file for pedestal outputs
*
* Revision 1.8  1995/07/27  19:03:36  cdaq
* (SAW) Error return fix up
*
* Revision 1.7  1995/05/22  13:29:24  cdaq
* (JRA) Make a listing of potential detector problems
*
* Revision 1.6  1995/04/01  19:42:36  cdaq
* (SAW) One report file for each of g, h, s, c instead of a single report file
*
* Revision 1.5  1994/08/04  03:45:46  cdaq
* (SAW) Add call to Breuer's hack_shutdown
*
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

      character*17 here
      parameter (here= 'G_proper_shutdown')

      logical ABORT
      character*(*) err

      logical bad_report,bad_HMS,bad_SOS,bad_COIN,bad_HBK,bad_hack
      character*132 err_report,err_HMS,err_SOS,err_COIN,err_HBK,err_hack
      integer SPAREID
      parameter (SPAREID=67)

      include 'gen_filenames.cmn'
      include 'gen_routines.dec'
      include 'gen_run_info.cmn'
      include 'hms_data_structures.cmn'
      include 'hms_tracking.cmn'
      include 'sos_data_structures.cmn'
      include 'sos_tracking.cmn'

      integer ierr
      character*132 file
*--------------------------------------------------------
      bad_report = .TRUE.
      err_report = 'Failed to open report file'

      ierr=thtreewriteg('all') ! Flush

      if (g_bad_output_filename.ne.' ') then
        file = g_bad_output_filename
        call g_sub_run_number(file, gen_run_number)

        open(unit=SPAREID,file=file,status='unknown')
      else
        open(unit=SPAREID,file='bad.tmp',status='unknown')
      endif

*-chance to flush any statistics, etc.
      call H_proper_shutdown(SPAREID,bad_HMS,err_HMS)

      call S_proper_shutdown(SPAREID,bad_SOS,err_SOS)

      call C_proper_shutdown(SPAREID,bad_COIN,err_COIN)

      close(unit=SPAREID)

      call hack_shutdown(bad_hack,err_hack)
      
      if (g_histout_filename .ne. ' ') then 
      call g_dump_histograms(bad_HBK,err_HBK)
      endif

      bad_report = .false.
      err_report = ' '

      if(g_report_blockname.ne.' '.and.
     $     g_report_output_filename.ne.' ') then

        file = g_report_output_filename
        call g_sub_run_number(file, gen_run_number)

        ierr = threp(g_report_blockname,file)
        if(ierr.ne.0) then
          bad_report = .true.
          err_report = 'threp failed to create report in file '//file
        endif
      endif

**** TH - Add sync report file here
*
      if(g_report_blockname.ne.' '.and.
     $     g_report_output_sync_filename.ne.' ') then

        file = g_report_output_sync_filename
        call g_sub_run_number(file, gen_run_number)

        ierr = threp(g_report_blockname,file)
        if(ierr.ne.0) then
          bad_report = .true.
          err_report = 'threp failed to create report in file '//file
        endif
      endif
**

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

      bad_HBK = .false.
      err_HBK = ' '

      RETURN
      END
