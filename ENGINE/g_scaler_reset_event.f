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
        scalers(ind) = 0.
      enddo
*     
*     
      g_bcm1_charge = 0.
      g_bcm2_charge = 0.
      g_bcm3_charge = 0.
      g_unser_charge = 0.
*
*
      ABORT= .FALSE.
      err= ' '
      RETURN
      END
