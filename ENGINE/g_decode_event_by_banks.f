      subroutine g_decode_event_by_banks(ABORT, err)
*-----------------------------------------------------------------------
*-     Purpose and Methods: Pull out individual Fastbus banks from event
*-                          for subsequent decoding from commoned CRAW
*-
*-     Find the beginning of each ROC bank and send it off to 
*-    "g_decode_fb_bank".
*-
*-     Inputs:
*-         bank       Pointer to the first word (length) of a data bank.
*-
*-     Outputs:
*-        ABORT       success or failure
*-        err         explanation for failure
*-
*-     Created   3-Dec-1993   Kevin Beard, Hampton U.
*-    $Log$
*-    Revision 1.2  1994/02/02 19:59:16  cdaq
*-    Rewrite without using fbgen routines
*-
c Revision 1.1  1994/02/01  20:38:58  cdaq
c Initial revision
c
c Revision 1.1  1994/02/01  18:36:51  cdaq
c New version from Kevin Beard
c
*-----------------------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*30 here
      parameter (here= 'g_decode_event_by_banks')
*
      logical ABORT
      character*(*) err
      integer*4 evlength                        ! Total length of the event
      integer*4 bankpointer                     ! Pointer to next bank
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

      ABORT = iand(CRAW(2),'FFFF'x).ne.'10CC'x
      if(ABORT) then
         err = here//'Event header does not standard physics event'
         return
      endif

      evlength = CRAW(1)
      bankpointer = 3

      ABORT = CRAW(bankpointer+1).ne.'C0000100'x
      if(ABORT) then
         err = here//'First bank is not an Event ID bank'
         return
      endif
      
      bankpointer = bankpointer + CRAW(bankpointer) + 1

      WARN = (bankpointer.gt.evlength)           ! No ROC's in event
      IF(WARN) THEN
        err= ':event contained no ROC banks'
        call G_add_path(here,err)
      ENDIF

      do while(bankpointer.lt.evlength)
         
         call g_decode_fb_bank(ABORT, err, CRAW(bankpointer) )
         bankpointer = bankpointer + CRAW(bankpointer) + 1

      enddo

      WARN = bankpointer.eq.(evlength + 1)
      if(WARN) THEN
         err = ':inconsistent bank and event lengths'
         call G_add_path(here,err)
      endif
*
      RETURN
      END



