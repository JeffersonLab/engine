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
*-    Revision 1.2  1994/04/12 17:28:15  cdaq
*-    (KBB) Add ntuple call
*-
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
      character*50 here
      parameter (here= 'S_proper_shutdown')
*     
      logical ABORT
      character*(*) err
*     
*--------------------------------------------------------
*-    chance to flush any statistics, etc.
*     
*     
      ABORT= .FALSE.
      err= ' '
*     
      call h_ntuple_shutdown(ABORT,err)
*
      IF(ABORT) THEN
         call G_add_path(here,err)
      ELSE
         err= ' '
      ENDIF
*     
      RETURN
      END

