      SUBROUTINE C_proper_shutdown(lunout,ABORT,err)
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
* $Log: c_proper_shutdown.f,v $
* Revision 1.8  1996/01/16 21:07:45  cdaq
* no change
*
* Revision 1.7  1995/07/27 19:02:19  cdaq
* (SAW) Move ntuple shutdown to g_ntuple_shutdown
*
* Revision 1.6  1995/05/22  13:30:11  cdaq
* (JRA) Make a listing of potential detector problems
*
* Revision 1.5  1995/04/01  19:43:57  cdaq
* (SAW) One report file for each of g, h, s, c instead of a single report file
*       Allow %d for run number in filenames
*
* Revision 1.4  1994/10/11  18:39:02  cdaq
* (SAW) Protect agains blank blocknames
*
* Revision 1.3  1994/08/30  14:45:44  cdaq
* (SAW) Add call to report generator
*
* Revision 1.2  1994/04/12  17:12:20  cdaq
* (KBB) Add ntuple call
*
* Revision 1.1  1994/02/04  21:07:58  cdaq
* Initial revision
*
*- All standards are from "Proposal for Hall C Analysis Software
*- Vade Mecum, Draft 1.0" by D.F.Geesamn and S.Wood, 7 May 1993
*-
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      include 'gen_routines.dec'
      include 'gen_filenames.cmn'
      include 'gen_run_info.cmn'
      include 'coin_filenames.cmn'
*
      character*17 here
      parameter (here= 'C_proper_shutdown')
*
      logical ABORT, report_abort
      character*(*) err
*
      integer ierr
      character*132 file
      integer lunout
*--------------------------------------------------------
*-chance to flush any statistics, etc.
*
*     
      ABORT= .FALSE.
      err = ' '
*
*      call c_ntuple_shutdown(ABORT,err)
*
      if(c_report_blockname.ne.' '.and.
     $     c_report_output_filename.ne.' ') then

        file = c_report_output_filename
        call g_sub_run_number(file, gen_run_number)

        ierr = threp(c_report_blockname, file)
        if(ierr.ne.0) then
          call g_append(err,'& threp failed to create report in file'//file)
          report_abort = .true.
        endif
      endif
*
      IF(ABORT.or.report_abort) THEN
         call G_add_path(here,err)
      ELSE
         err= ' '
      ENDIF
*     
      RETURN
      END
