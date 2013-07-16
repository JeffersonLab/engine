      SUBROUTINE S_proper_shutdown(lunout,ABORT,err)
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
* $Log: s_proper_shutdown.f,v $
* Revision 1.11  1995/10/09 18:56:25  cdaq
* (JRA) Add bypass switches to efficiency shutdown routine calls
*
* Revision 1.10  1995/09/01 13:40:09  cdaq
* (JRA) Add calls to more efficiency calculations and bad counter report
*
* Revision 1.9  1995/08/11  15:39:32  cdaq
* (JRA) Add sos Cerenkov efficiencies
* (DD) Add sos sieve slit ntuple
*
* Revision 1.8  1995/07/27  19:02:47  cdaq
* (SAW) Move ntuple shutdown to g_ntuple_shutdown
*
* Revision 1.7  1995/05/22  13:29:39  cdaq
* (JRA) Make a listing of potential detector problems
*
* Revision 1.6  1995/04/01  20:11:28  cdaq
* (SAW) One report file for each of g, h, s, c instead of a single report file
*       Allow %d for run number in filenames
*
* Revision 1.5  1995/03/13  18:17:49  cdaq
* (JRA) Add calls to s_scin_eff_shutdown and s_cal_eff_shutdown.
*
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
      include 'gen_run_info.cmn'
      include 'sos_filenames.cmn'
      include 'sos_bypass_switches.cmn'
*
      character*17 here
      parameter (here= 'S_proper_shutdown')
*     
      logical ABORT, report_abort
      character*(*) err
*
      integer ierr
      character*132 file
      integer lunout
*--------------------------------------------------------
*-    chance to flush any statistics, etc.
*     
*     
      ABORT= .FALSE.
      err= ' '
*     
      if (sbypass_dc_eff.eq.0) then
        call s_dc_eff_shutdown(lunout,ABORT,err)
        call s_dc_trk_eff_shutdown(lunout,ABORT,err)
      endif
*     
      if (sbypass_scin_eff.eq.0) call s_scin_eff_shutdown(lunout,ABORT,err)
*
      if (sbypass_cer_eff.eq.0) call s_cer_eff_shutdown(lunout,ABORT,err)
*
      if (sbypass_cal_eff.eq.0) call s_cal_eff_shutdown(ABORT,err)
*     
      call s_report_bad_data(lunout,ABORT,err)
*
      if(s_report_blockname.ne.' '.and.
     $     s_report_output_filename.ne.' ') then

        file = s_report_output_filename
        call g_sub_run_number(file, gen_run_number)

        ierr = threp(s_report_blockname, file)
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

