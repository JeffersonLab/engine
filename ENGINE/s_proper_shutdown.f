      SUBROUTINE S_proper_shutdown(ABORT,err)
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
*-    Revision 1.5  1995/03/13 18:17:49  cdaq
*-    (JRA) Add calls to s_scin_eff_shutdown and s_cal_eff_shutdown.
*-
* Revision 1.4  1994/10/11  18:40:49  cdaq
* (SAW) Protect agains blank blocknames
*
* Revision 1.3  1994/06/17  03:02:01  cdaq
* (KBB) Fix typo
*
* Revision 1.2  1994/04/12  17:28:15  cdaq
* (KBB) Add ntuple call
*
* Revision 1.1  1994/02/04  22:21:52  cdaq
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
      include 'sos_filenames.cmn'
*
      character*50 here
      parameter (here= 'S_proper_shutdown')
*     
      logical ABORT, report_abort
      character*(*) err
*
      integer ierr
*     
*--------------------------------------------------------
*-    chance to flush any statistics, etc.
*     
*     
      ABORT= .FALSE.
      err= ' '
*     
      call s_ntuple_shutdown(ABORT,err)
*
c*      call s_sv_nt_shutdown(ABORT,err)
*
      call s_scin_eff_shutdown(ABORT,err)
*
      call s_cal_eff_shutdown(ABORT,err)
*
      if(s_report_blockname .ne. ' ') then
        ierr = threpa(s_report_blockname, g_report_output_filename)
        if(ierr.ne.0) then
          call g_append(err,'& threpa failed to append report')
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

