      SUBROUTINE T_proper_shutdown(lunout,ABORT,err)
*--------------------------------------------------------
*-       T20 end of run analysis
*-
*-
*-   Purpose and Methods : Closes files properly, flushes, etc.
*- 
*-   Output: ABORT		- success or failure
*-         : err	- reason for failure, if any
*- 
*-   Created  23-Jan-1997   Stephen A. Wood
* $Log$
* Revision 1.1  1998/12/01 20:57:27  saw
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
      include 't20_filenames.cmn'
      include 't20_bypass_switches.cmn'
*
      character*17 here
      parameter (here= 't_proper_shutdown')
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

      call t_polder_shutdown(ABORT,err)
*     
c     if (tbypass_dc_eff.eq.0) then
c       call t_dc_eff_shutdown(lunout,ABORT,err)
c       call t_dc_trk_eff_shutdown(lunout,ABORT,err)
c     endif
c*     
c     if (tbypass_scin_eff.eq.0) call tscin_eff_shutdown(lunout,ABORT,err)
c*
c      if (tbypass_cer_eff.eq.0) call t_eff_shutdown(lunout,ABORT,err)
c*
c      if (tbypass_cal_eff.eq.0) call t_cal_eff_shutdown(ABORT,err)
c*     
c      call t_report_bad_data(lunout,ABORT,err)
c*
      if(t_report_blockname.ne.' '.and.
     $     t_report_output_filename.ne.' ') then

        file = t_report_output_filename
        call g_sub_run_number(file, gen_run_number)

        ierr = threp(t_report_blockname, file)
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

