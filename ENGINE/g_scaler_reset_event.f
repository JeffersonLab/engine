      SUBROUTINE g_scaler_reset_event(ABORT,err)
*--------------------------------------------------------
*-       Prototype C analysis routine
*-
*-
*-   Purpose and Methods : Resets all scalers at beginning of run.
*-
*- 
*-   Output: ABORT		- success or failure
*-         : err	- reason for failure, if any
*- 
*-   Created  12-Sep-1995   John Arrington
*-
*- All standards are from "Proposal for Hall C Analysis Software
*- Vade Mecum, Draft 1.0" by D.F.Geesamn and S.Wood, 7 May 1993
*
* $Log$
* Revision 1.4.2.2  2003/09/04 21:02:03  jones
* changes to run with syncfilter (mkj)
*
* Revision 1.4.2.1  2003/08/14 00:40:09  cdaq
* Modify so "beam on" scalers for both bcm1 and bcm2 (mkj)
*
* Revision 1.4  1999/02/10 18:17:57  csa
* Added beam-on variable initialization (D. McKee)
*
* Revision 1.3  1996/04/29 19:48:45  saw
* (JRA) Add gscaler, gscaler_old, gscaler_nroll, gscaler_change initialization
*
* Revision 1.2  1996/01/16 17:06:35  cdaq
* (CB) Clear out current monitor variables
*
* Revision 1.1  1995/09/19 14:58:35  cdaq
* Initial revision
*
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*20 here
      parameter (here= 'g_scaler_reset_event')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 'gen_scalers.cmn'
*
      INTEGER ind
*
*--------------------------------------------------------
*
      do ind = 1 , max_num_evscalers
        evscalers(ind) = 0.
      enddo
*     
      do ind = 1 , max_num_scalers
        gscaler(ind) = 0.
        gscaler_skipped(ind) = 0.
        gscaler_saved(ind) = 0.
        gscaler_old(ind) = 0.
        gscaler_nroll(ind) = 0
        gscaler_change(ind) = 0.
      enddo
*
      gscal_lastevnum(1)=0
      gscal_lastevnum(2)=0
*     
      gbcm1_charge = 0.
      gbcm2_charge = 0.
      gbcm3_charge = 0.
      gunser_charge = 0.
*
      g_run_time = 0.
      g_beam_on_run_time(1) = 0.    ! Have to do this, because I have to accumlate
      g_beam_on_bcm_charge(1) = 0.
      g_beam_on_run_time(2) = 0.    ! Have to do this, because I have to accumlate
      g_beam_on_bcm_charge(2) = 0.
*
      ABORT= .FALSE.
      err= ' '
      RETURN
      END
