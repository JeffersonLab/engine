      SUBROUTINE G_proper_shutdown(ABORT,err)
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
*-    Revision 1.1  1994/02/04 22:12:15  cdaq
*-    Initial revision
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
      character*50 here
      parameter (here= 'G_proper_shutdown')
*
      logical ABORT
      character*(*) err
*
*--------------------------------------------------------
*-chance to flush any statistics, etc.
      call H_proper_shutdown(ABORT,err)
*     
      IF(.NOT.ABORT) call S_proper_shutdown(ABORT,err)
*     
      IF(.NOT.ABORT) call C_proper_shutdown(ABORT,err)
*
      IF(ABORT) THEN
         call G_add_path(here,err)
         RETURN
      ELSE
         err= ' '
      ENDIF
*     
      RETURN
      END
