      SUBROUTINE G_examine_control_event(buffer,ABORT,err)
*--------------------------------------------------------
*-
*-    Purpose and Methods : examine a control event and gather various
*-                          information from it.
*- 
*-    Input: buffer             - raw data buffer
*-         : ABORT              - success or failure
*-         : err                - reason for failure, if any
*- 
*-   Created  17-May-1994   Kevin B. Beard, Hampton U.
* $Log$
* Revision 1.5  1994/06/28 20:04:49  cdaq
* *** empty log message ***
*
* Revision 1.4  1994/06/24  19:11:26  cdaq
* (KBB) Fill in gen_event_type with the event type
*
* Revision 1.3  1994/06/09  04:29:44  cdaq
* (SAW) Replace g_build_note calls with write(var, ... calls
*
* Revision 1.2  1994/06/07  18:18:45  cdaq
* (SAW) Split g_examine_event into g_examine_control_event
*       and g_examine_physics_event.
*
*-
*- All standards are from "Proposal for Hall C Analysis Software
*- Vade Mecum, Draft 1.3" by D.F.Geesamn and S.Wood, Csoft-NOTE-94-001
*-
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*23 here
      parameter (here= 'G_examine_control_event')
*     
      INTEGER buffer(*)
      LOGICAL ABORT
      CHARACTER*(*) err
*
      INCLUDE 'gen_run_info.cmn'
      INCLUDE 'gen_event_info.cmn'
*
      integer dy,mth,yr,hr,minute,sec,m,EvType,status,nth
      logical control,bad_sync
      character*160 msg,note
*
      integer SYNC_EvType,PRESTART_EvType,GO_EvType,END_EvType
      integer PAUSE_EvType
      parameter (SYNC_EvType     = 16)  !from CODA manual
      parameter (PRESTART_EvType = 17)  !from CODA manual
      parameter (GO_EvType       = 18)  !from CODA manual
      parameter (PAUSE_EvType    = 19)  !from CODA manual
      parameter (END_EvType      = 20)  !from CODA manual
*
*----------------------------------------------------------------------
      err= ' '
*
      gen_event_sequence_N= gen_event_sequence_N+1  !from beginning
*
      if(iand(buffer(2),'FFFF'x).ne.'01CC'x) then
         err = 'Event is not a control event'
         ABORT = .true.
         call g_add_path(here,err)
         return
      endif
      EvType = ISHFT(buffer(2),-16)
      
      gen_event_ID_number= 0
      gen_event_type= EvType
      gen_event_class= 0
*     
      If(EvType.EQ.SYNC_EvType) Then
*
         gen_run_UTC_last= buffer(3)
         gen_run_total_events= buffer(5)
         call g_UTC_date(gen_run_UTC_last,gen_run_date_last,
     &        dy,mth,yr,hr,minute,sec)
*     
         status= buffer(6)
         bad_sync= status.NE.0
         ABORT= bad_sync
         if(bad_sync) then
            err= ' '
            DO nth=0,31
               If(BTEST(status,nth)) Then
                  write(msg,'(", ROC #",i3)') nth
                  call G_append(err,msg)
               EndIf
            ENDDO
            write(msg,'("event #",i10)') gen_run_total_events
            call G_prepend(':CODA synchronization failure '//note,err)
            call G_append(err,':'//gen_run_date_last)
            call G_add_path(here,err)
            IF(ABORT) RETURN
         endif
*     
      ElseIf(EvType.EQ.PRESTART_EvType) Then
*
         gen_run_UTC_start= buffer(3)
         gen_run_number= buffer(4)
         gen_run_type= buffer(5)
         call g_UTC_date(gen_run_UTC_start,gen_run_date_start,
     &        dy,mth,yr,hr,minute,sec)
*     
         gen_event_sequence_N= 1        !start counting over
         gen_run_total_events= 1
*     
         do m=0,gen_MAX_trigger_types   !clear triggered table
            gen_run_triggered(m)= 0
         enddo
*     
         write(msg,'("INFO:PRESTART Run #",i5," type #",i4,1x,a)')
     $        gen_run_number,gen_run_type,gen_run_date_start
         call G_log_message(msg)
*     
      ElseIf(EvType.EQ.GO_EvType) Then
*     
         gen_run_UTC_start= buffer(3)
         call g_UTC_date(gen_run_UTC_start,gen_run_date_start,
     &        dy,mth,yr,hr,minute,sec)
*     
         msg= 'INFO:GO '//gen_run_date_start
         call G_log_message(msg)
*     
      ElseIf(EvType.EQ.PAUSE_EvType) Then
*     
         gen_run_UTC_last= buffer(3)
         gen_run_total_events= buffer(5)
         call g_UTC_date(gen_run_UTC_last,gen_run_date_last,
     &        dy,mth,yr,hr,minute,sec)
         msg= 'INFO:PAUSE '//gen_run_date_last
         call G_log_message(msg)
*     
      ElseIf(EvType.EQ.END_EvType) Then
*     
         gen_run_UTC_stop= buffer(3)
         gen_run_total_events= buffer(5)
         call g_UTC_date(gen_run_UTC_stop,gen_run_date_stop,
     &        dy,mth,yr,hr,minute,sec)
*     
         msg= 'INFO:END '//gen_run_date_stop
         call G_log_message(msg)
*     
      EndIf
*
      RETURN
      END
