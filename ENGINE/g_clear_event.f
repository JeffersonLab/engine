      SUBROUTINE G_clear_event(ABORT,err)
*--------------------------------------------------------
*-       Prototype C analysis routine
*-
*-
*-   Purpose and Methods : clears all quantities before an event is processed.
*-
*- 
*-   Output: ABORT		- success or failure
*-         : err	- reason for failure, if any
*- 
*-   Created  29-Oct-1993   Kevin B. Beard, Hampton U.
*-   Modified 19-Nov-1993   Kevin B. Beard for new error standards
*-      $Log$
*-      Revision 1.2  1994/02/17 21:46:03  cdaq
*-      Add call to gmc_clear_event
*-
* Revision 1.1  1994/02/04  21:48:38  cdaq
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
*
      character*13 here
      parameter (here= 'G_clear_event')
*     
      logical ABORT
      character*(*) err
*
      INCLUDE 'gen_data_structures.cmn'
*
      logical HMS_ABORT,SOS_ABORT,COIN_ABORT,gmc_abort
      character*132 HMS_err,SOS_err,COIN_err,gmc_err
*
*--------------------------------------------------------
*
      err= ' '
      HMS_err= ' '
      SOS_err= ' '
      gmc_err= ' '
*
      CRAW(1)= 0                                !delete event header
      CRAW(2)= 0
*
      call H_clear_event(HMS_ABORT,HMS_err)
*
      call S_clear_event(SOS_ABORT,SOS_err)
*
      call C_clear_event(COIN_ABORT,COIN_err)
*
      call gmc_clear_event(gmc_abort,gmc_err)
*
      ABORT= HMS_ABORT .or. SOS_ABORT .or. COIN_ABORT .or. gmc_abort
*
      IF(ABORT) THEN
         err= COIN_err
         call G_prepend(SOS_err,err)
         call G_prepend(HMS_err,err)
         call g_prepend(gmc_err,err)
         call G_add_path(here,err)
      ELSE
         err= ' '
      ENDIF
*
      RETURN
      END
