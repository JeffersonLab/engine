       SUBROUTINE S_initialize(ABORT,err)
*--------------------------------------------------------
*-       Prototype C analysis routine
*-
*-
*-   Purpose and Methods : Initializes HMS quantities 
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
*-   Created  8-Nov-1993   Kevin B. Beard
*-   Modified 20-Nov-1993  KBB for new errors
*-    $Log$
*-    Revision 1.4  1994/02/11 18:36:35  cdaq
*-    Split off CTP variables registration from initialize routines
*-
* Revision 1.3  1994/02/04  20:47:40  cdaq
* Add read titles to regpar calls
*
* Revision 1.2  1994/02/03  14:28:27  cdaq
* Make clear that last arg of reg calls is a title.  Use null for now.
*
* Revision 1.1  1994/02/02  21:37:55  cdaq
* Initial revision
*
*-
*- All standards are from "Proposal for Hall C Analysis Software
*- Vade Mecum, Draft 1.0" by D.F.Geesamn and S.Wood, 7 May 1993
*-
*--------------------------------------------------------
       IMPLICIT NONE
       SAVE
*
       character*12 here
       parameter (here= 'S_initialize')
*
       logical ABORT
       character*(*) err
*
*
*--------------------------------------------------------
      err= ' '
*
      ABORT = .FALSE.
*
      return
      end
