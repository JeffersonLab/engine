      SUBROUTINE G_reset_event(ABORT,err)
*--------------------------------------------------------
*-       Prototype C analysis routine
*-
*-
*-   Purpose and Methods : Resets all quantities before an event is processed.
*-
*- 
*-   Output: ABORT	- success or failure
*-         : err	- reason for failure, if any
*- 
*-   Created  29-Oct-1993   Kevin B. Beard
*-   Modified  3-Dec-1993   Kevin B. Beard, Hampton U.
*-    $Log$
*-    Revision 1.5  1994/04/12 18:42:05  cdaq
*-    (SAW) Remove clearing of CRAW event buffer to online compatibility
*-
* Revision 1.4  1994/02/22  19:47:36  cdaq
* Change gmc_reset_event to gmc_mc_reset
*
* Revision 1.3  1994/02/17  21:49:57  cdaq
* Simplify error handling to be like g_clear_event
*
* Revision 1.2  1994/02/17  21:43:39  cdaq
* Add call to gmc_reset_event
*
* Revision 1.1  1994/02/04  22:13:26  cdaq
* Initial revision
*
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
      parameter (here= 'G_reset_event')
*
      logical ABORT
      character*(*) err
*
      logical HMS_ABORT,SOS_ABORT,COIN_ABORT,gmc_abort
      character*132 HMS_err,SOS_err,COIN_err,gmc_err
      integer i
*
      INCLUDE 'gen_data_structures.cmn'
*
*--------------------------------------------------------
*
      err = ' '
      hms_err = ' '
      sos_err = ' '
      gmc_err = ' '
*
      call H_reset_event(HMS_ABORT,HMS_err)
*     
      call S_reset_event(SOS_ABORT,SOS_err)
*     
      call C_reset_event(COIN_ABORT,COIN_err)
*     
      call gmc_mc_reset(gmc_abort, gmc_err)
*     
      abort = hms_abort.or.sos_abort.or.coin_abort.or.gmc_abort
*
      IF(ABORT) then
         err= COIN_err
         call G_prepend(SOS_err,err)
         call G_prepend(HMS_err,err)
         call g_prepend(gmc_err,err)
         call G_add_path(here,err)
      else
         err = ' '
      endif
*     
      RETURN
      END
