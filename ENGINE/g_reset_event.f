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
*-    Revision 1.2  1994/02/17 21:43:39  cdaq
*-    Add call to gmc_reset_event
*-
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
      logical OK,HMS_ABORT,SOS_ABORT
      character*132 HMS_err,SOS_err
      integer i
*
      INCLUDE 'gen_data_structures.cmn'
*
*--------------------------------------------------------
*
*-zero entire event buffer
      DO i=1,LENGTH_CRAW
         CRAW(i)= 0
      ENDDO
*
*-    HMS reset
      call H_reset_event(HMS_ABORT,HMS_err)
*     
*-    SOS reset
      call S_reset_event(SOS_ABORT,SOS_err)
*     
      ABORT= HMS_ABORT.and.SOS_ABORT
*     
*-    COIN reset
      IF(.NOT.HMS_ABORT .and. .NOT.SOS_ABORT) THEN
*     
*-    try coincidence
         call C_reset_event(ABORT,err)
*     
      ELSEIF(HMS_ABORT) THEN
*     
*-    SOS only
         err= HMS_err
         call G_add_path(here,err)              !warning only
*     
      ELSEIF(SOS_ABORT) THEN
*-    HMS only
         ABORT= HMS_ABORT
         call G_add_path(here,err)              !warning only
*     
      ELSE
*-    BOTH error
         ABORT= .TRUE.
         err = '&'//SOS_err    
         call g_prepend(HMS_err,err)
*     
      ENDIF

      call gmc_reset_event(gmc_abort, gmc_err)
      if(gmc_abort) then
         if(abort) then
            err = '&'//err
            call g_prepend(gmc_err,err)
         else
            err = gmc_err
         endif
         abort = .true.
      endif
*     
      IF(ABORT) call G_add_path(here,err)
*     
      RETURN
      END
