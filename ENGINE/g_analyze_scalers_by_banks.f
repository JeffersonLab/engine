      subroutine g_analyze_scalers_by_banks(event,ABORT, err)
*-----------------------------------------------------------------------
*-     Purpose and Methods: Pull out individual scaler banks from event
*-                          for subsequent analysis
*-
*-     Find the beginning of each ROC bank and send it off to 
*-    "g_analyze_scaler_bank".
*-
*-     Inputs:
*-         event      Pointer to the first word (length) of an event data bank.
*-
*-     Outputs:
*-        ABORT       success or failure
*-        err         explanation for failure
*-
*-     Created   20-Jun-1998   Stephen Wood
*-    $Log$
*-    Revision 1.2.24.2  2007/11/09 17:17:15  cdaq
*-     added ability to read roc21 scalers
*-
*-    Revision 1.2.24.1  2007/09/10 20:33:37  pcarter
*-    Implemented changes to allow compilation on RHEL 3,4,5 and MacOSX
*-
*-    Revision 1.2  1999/11/04 20:35:15  saw
*-    Linux/G77 compatibility fixes
*-
*-    Revision 1.1  1999/02/24 15:04:41  saw
*-    Bring into CVS tree
*-
*-----------------------------------------------------------------------
      IMPLICIT NONE
      SAVE
      external jishft, jiand, jieor
*
      integer*4 event(*)
*
      character*30 here
      parameter (here= 'g_analyze_scalers_by_banks')
*
      logical ABORT
      character*(*) err
      integer*4 evlength                        ! Total length of the event
      integer*4 bankpointer                     ! Pointer to next bank
      integer*4 jiand,jishft,jieor              ! Declare to help f2c
      integer*4 roc
*
      include 'gen_data_structures.cmn'
*
      logical WARN
*
*-----------------------------------------------------------------------
*
*
*     Assume that the event is bank containing banks, the first of which is
*     an event ID bank.
*
*     Various hex constants that are used in decode routines should
*     probably be put in an include file.
*

      ABORT = jieor(jiand(event(2),'FFFF'x),'10CC'x).ne.0
      if(ABORT) then
         err = here//'Event header not standard physics event'
         return
      endif

      evlength = event(1)
      bankpointer = 3

      ABORT = jieor(event(bankpointer+1),'C0000100'x).ne.0
      if(ABORT) then
         err = here//'First bank is not an Event ID bank'
         return
      endif
      
      bankpointer = bankpointer + event(bankpointer) + 1

      WARN = (bankpointer.gt.evlength)           ! No ROC's in event
      IF(WARN) THEN
        err= ':event contained no ROC banks'
        call G_add_path(here,err)
      ENDIF

      do while(bankpointer.lt.evlength)
         roc = jiand(jishft(event(bankpointer+1),-16),'1F'X)
         if(roc.eq.5.or.roc.eq.8.or.roc.eq.20.or.roc.eq.21) then
            call g_analyze_scaler_bank(event(bankpointer),roc, ABORT, err)
         endif
         bankpointer = bankpointer + event(bankpointer) + 1

      enddo

      WARN = bankpointer.eq.(evlength + 1)
      if(WARN) THEN
         err = ':inconsistent bank and event lengths'
         call G_add_path(here,err)
      endif
*
      RETURN
      END



