      SUBROUTINE C_proper_shutdown(ABORT,err)
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
*-    Revision 1.3  1994/08/30 14:45:44  cdaq
*-    (SAW) Add call to report generator
*-
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
      include 'coin_filenames.cmn'
*
      character*50 here
      parameter (here= 'C_proper_shutdown')
*
      logical ABORT, report_abort
      character*(*) err
*
      integer ierr
*
*--------------------------------------------------------
*-chance to flush any statistics, etc.
*
*     
      ABORT= .FALSE.
      err = ' '
*
      call c_ntuple_shutdown(ABORT,err)
*
      ierr = threpa(c_report_blockname, g_report_output_filename)
      if(ierr.ne.0) then
        call g_append(err,'& threpa failed to append report')
        report_abort = .true.
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

