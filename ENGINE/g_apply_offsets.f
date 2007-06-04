      SUBROUTINE G_apply_offsets(ABORT,err)
*--------------------------------------------------------
*-       Prototype C analysis routine
*-
*-
*-   Purpose and Methods : applies offsets to spectrometer
*-   momenta and angles
*-
*- 
*-   Output: ABORT		- success or failure
*-         : err	- reason for failure, if any
*-
*-   Created  31-Aug-1999 Chris Armstrong
*- 
*-
*- All standards are from "Proposal for Hall C Analysis Software
*- Vade Mecum, Draft 1.0" by D.F.Geesamn and S.Wood, 7 May 1993
*-
*-
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*13 here
      parameter (here= 'G_apply_offsets')
*     
      logical ABORT
      character*(*) err
      logical HMS_ABORT,SOS_ABORT
      character*132 HMS_err,SOS_err

      include 'gen_run_info.cmn'
*
*--------------------------------------------------------
*
      err= ' '
      HMS_err= ' '
      SOS_err= ' '
*
      
      call H_apply_offsets(HMS_ABORT,HMS_err)
      
*
      
      call S_apply_offsets(SOS_ABORT,SOS_err)
      
*
      ABORT= HMS_ABORT .or. SOS_ABORT
*
      IF(ABORT) THEN
         err= HMS_err
         call G_prepend(SOS_err,err)
         call G_add_path(here,err)
      ELSE
         err= ' '
      ENDIF
*
      RETURN
      END
