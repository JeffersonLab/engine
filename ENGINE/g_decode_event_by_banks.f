      subroutine g_decode_event_by_banks(event,ABORT, err)
*-----------------------------------------------------------------------
*-     Purpose and Methods: Pull out individual Fastbus banks from event
*-                          for subsequent decoding
*-
*-     Find the beginning of each ROC bank and send it off to 
*-    "g_decode_fb_bank".
*-
*-     Inputs:
*-         event      Pointer to the first word (length) of an event data bank.
*-
*-     Outputs:
*-        ABORT       success or failure
*-        err         explanation for failure
*-
*-     Created   3-Dec-1993   Kevin Beard, Hampton U.
*-    $Log: g_decode_event_by_banks.f,v $
*-    Revision 1.7  2008/09/25 00:06:33  jones
*-    Updated to run with gfortran compiler
*-
*-    Revision 1.6.24.2  2007/09/10 20:33:37  pcarter
*-    Implemented changes to allow compilation on RHEL 3,4,5 and MacOSX
*-
*-    Revision 1.6.24.1  2007/05/15 02:55:01  jones
*-    Start to Bigcal code
*-
*-    Revision 1.6  1999/11/04 20:35:15  saw
*-    Linux/G77 compatibility fixes
*-
*-    Revision 1.5  1995/07/27 19:09:10  cdaq
*-    (SAW) Use specific bit manipulation routines for f2c compatibility
*-
* Revision 1.4  1994/04/15  20:34:42  cdaq
* ???
*
* Revision 1.3  1994/02/17  21:30:37  cdaq
* Move ABORT, err args to end of g_decode_fb_bank call
*
* Revision 1.2  1994/02/02  19:59:16  cdaq
* Rewrite without using fbgen routines
*
* Revision 1.1  1994/02/01  20:38:58  cdaq
* Initial revision
*
*-----------------------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      external jiand, jieor
      integer*4 event(*)
*
      character*30 here
      parameter (here= 'g_decode_event_by_banks')
*
      logical ABORT
      character*(*) err
      integer*4 evlength                        ! Total length of the event
      integer*4 bankpointer                     ! Pointer to next bank
      integer*4 jiand, jieor
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
         !write(*,*) 'about to call g_decode_fb_bank'
         call g_decode_fb_bank(event(bankpointer), ABORT, err)

         !write(*,*) 'g_decode_fb_bank successful'
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



