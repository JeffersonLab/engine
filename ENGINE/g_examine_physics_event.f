      SUBROUTINE G_examine_physics_event(buffer,ABORT,err)
*--------------------------------------------------------
*-    
*-    Purpose and Methods : examine event and decide whether to process
*-    further
*- 
*-    Input: buffer             - raw data buffer
*-   Output: process            - worth processing
*-         : ABORT              - success or failure
*-         : err                - reason for failure, if any
*- 
*-   Created  17-May-1994   Kevin B. Beard, Hampton U.
* $Log$
* Revision 1.4.24.2.2.1  2009/02/16 00:18:13  cdaq
* *** empty log message ***
*
* Revision 1.4.24.2  2007/09/13 04:02:17  brash
* Implement some minor changes to fix Mac OS X runtime errors ... ejb
*
* Revision 1.4.24.1  2007/09/10 20:33:37  pcarter
* Implemented changes to allow compilation on RHEL 3,4,5 and MacOSX
*
* Revision 1.4  1999/11/04 20:35:16  saw
* Linux/G77 compatibility fixes
*
* Revision 1.3  1996/01/16 20:57:18  cdaq
* no change
*
* Revision 1.2  1995/07/27 19:11:15  cdaq
* (SAW) Use specific bit manipulation routines for f2c compatibility
*       Remove event number limit checking
*
* Revision 1.1  1994/06/07  18:19:03  cdaq
* Initial revision
*
*-
*- All standards are from "Proposal for Hall C Analysis Software
*- Vade Mecum, Draft 1.3" by D.F.Geesamn and S.Wood, Csoft-NOTE-94-001
*-
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
      external jishft, jiand, jieor
*
      character*23 here
      parameter (here= 'G_examine_physics_event')

      INTEGER buffer(*)
ccc      LOGICAL process
      LOGICAL ABORT
      CHARACTER*(*) err
*
      INCLUDE 'gen_run_info.cmn'
      INCLUDE 'gen_event_info.cmn'
*
      integer evtype
      logical eventidbank, nontrivial
      integer*8 EventIDbank_size,EventIDbank_desc_hex,EventIDbank_desc
*
      integer*4 jiand,jishft,jieor
*
      parameter (EventIDbank_size= 4)
      parameter (EventIDbank_desc_hex= '40000100'x)  !from CODA manual
*
      EventIDbank_desc=-1*EventIDbank_desc_hex+512
      gen_event_sequence_N= gen_event_sequence_N+1  !from beginning
*
      if(jieor(jiand(buffer(2),'FFFF'x),'10CC'x).ne.0) then
         err = 'Event is not a physics event'
         ABORT = .true.
         call g_add_path(here,err)
         return
      endif
      EvType = jISHFT(buffer(2),-16)
*
      gen_run_total_events= gen_run_total_events+1
*      gen_event_type= EvType
*
      ABORT= EvType.LT.0 .or. EvType.GT.gen_MAX_trigger_types
      If(ABORT) Then
         write(err,'(":illegal physics type #",i3," sequential #",i10)')
     $        EvType, gen_event_sequence_N
         call G_add_path(here,err)
         RETURN
      EndIf
*
ccc      process= gen_run_enable(EvType)
*
      gen_run_triggered(EvType)= gen_run_triggered(EvType)+1
*     
*-    likely that next bank is an "Event ID bank"; if so try to
*-    recover event info, if not just skip
*     
      nontrivial= buffer(1).GE.6        !non-null CODA physics event
*
      If(nontrivial) Then
         EventIDbank= buffer(1).GE.6 .and. 
     &        buffer(3).EQ.EventIDbank_size  
     &        .and. buffer(4).EQ.EventIDbank_desc
*     
c         write(*,*)'Event info: ',buffer(1),buffer(3),buffer(4)
c         write(*,*)'Event into 2:',
c     &                  EventIDbank_size,EventIDbank_desc,EventIDbank
         if(EventIDbank) then
*
            gen_event_ID_number= buffer(5)
            gen_event_class= buffer(6)
            gen_event_ROC_summary= buffer(7)
*
*-see if event_ID within limits of interest
ccc            IF(gen_run_starting_event.GT.0) THEN
ccc               process= gen_event_ID_number.GE.gen_run_starting_event
ccc            ENDIF
*
ccc            IF(gen_run_stopping_event.GE.gen_run_starting_event
ccc     &           .and. gen_run_stopping_event.GT.0) THEN
ccc               process= gen_event_ID_number.LE.gen_run_stopping_event
ccc            ENDIF
*
         else                           !1st bank NOT eventID bank-must look later
*
            gen_event_ID_number= 0
            gen_event_class= 0
            gen_event_ROC_summary= 0
*     
         endif
*     
      Else                              !trivial event- nothing inside
*
         write(err,'(":sequential event #",i10," type #",i3
     $        ," too small [",i2,"]")')
     $        gen_event_sequence_n,evtype,buffer(1)+1
         call G_add_path(here,err)      !warning only
*
      EndIf

      return
      end
