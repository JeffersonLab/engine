      SUBROUTINE G_clear_event(ABORT,err)
*--------------------------------------------------------
*-       Prototype C analysis routine
*-
*-
*-   Purpose and Methods : clears all quantities AT THE START OF EACH EVENT
*-
*- 
*-   Output: ABORT		- success or failure
*-         : err	- reason for failure, if any
*- 
*-   Created  29-Oct-1993   Kevin B. Beard, Hampton U.
*-   Modified 19-Nov-1993   Kevin B. Beard for new error standards
*-      $Log$
*-      Revision 1.8  1996/01/16 18:41:36  cdaq
*-      (JRA) Explain that routine runs at start of each event
*-
*-      Revision 1.7  1995/07/27 19:06:40  cdaq
*-      (SAW) Disable monte carlo (GMC)
*-
* Revision 1.6  1995/04/01  19:44:31  cdaq
* (SAW) Add clear of BPM hit counter
*
* Revision 1.5  1994/06/22  20:23:47  cdaq
* (SAW) Clear the uninstrumented channel hit counter
*
* Revision 1.4  1994/04/15  20:33:43  cdaq
* (SAW) Changes for ONLINE use
*
* Revision 1.3  1994/02/22  19:47:07  cdaq
* Change gmc_clear_event to gmc_mc_clear
*
* Revision 1.2  1994/02/17  21:46:03  cdaq
* Add call to gmc_clear_event
*
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
      GUNINST_TOT_HITS = 0              ! Unistrumented hit counter
      CBPM_TOT_HITS = 0
*
      call H_clear_event(HMS_ABORT,HMS_err)
*
      call S_clear_event(SOS_ABORT,SOS_err)
*
      call C_clear_event(COIN_ABORT,COIN_err)
*
**      call gmc_mc_clear(gmc_abort,gmc_err)
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
