      SUBROUTINE G_examine_event(buffer,process,ABORT,err)
*--------------------------------------------------------
*-
*-   Purpose and Methods : examine event and decide whether to process
*-                         further
*- 
*-    Input: buffer             - raw data buffer
*-   Output: process            - worth processing
*-         : ABORT              - success or failure
*-         : err                - reason for failure, if any
*- 
*-   Created  17-May-1994   Kevin B. Beard, Hampton U.
*-
*- All standards are from "Proposal for Hall C Analysis Software
*- Vade Mecum, Draft 1.3" by D.F.Geesamn and S.Wood, Csoft-NOTE-94-001
*-
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*15 here
      parameter (here= 'G_examine_event')
*     
      INTEGER buffer(*)
      LOGICAL process,ABORT
      CHARACTER*(*) err
*
      INCLUDE 'gen_run_info.cmn'
      INCLUDE 'gen_event_info.cmn'
*
      integer iv(10),dy,mth,yr,hr,min,sec,m,EvType,status,nth
      real rv(10)
      logical control,physics,EventIDbank,nontrivial,bad_sync
      character*80 msg,note
*
      integer SYNC_EvType,PRESTART_EvType,GO_EvType,END_EvType
      integer PAUSE_EvType
      parameter (SYNC_EvType     = 16)  !from CODA manual
      parameter (PRESTART_EvType = 17)  !from CODA manual
      parameter (GO_EvType       = 18)  !from CODA manual
      parameter (PAUSE_EvType    = 19)  !from CODA manual
      parameter (END_EvType      = 20)  !from CODA manual
*
      integer EventIDbank_size,EventIDbank_desc
      parameter (EventIDbank_size= 4)
      parameter (EventIDbank_desc= 'C0000100'x)  !from CODA manual
*
      integer up,down,unknown,bit,byte,test
      parameter (unknown= 0)
      parameter (bit= 1)
      parameter (byte= 8*bit)
      data up,down/2*unknown/              !initial state
      data test/1/
*
*----------------------------------------------------------------------
      err= ' '
*
*-establish platform's direction for byte operations
*
      IF(up.EQ.unknown) THEN
        IF(ISHFT(test,+1).GT.test) THEN
          up= +1
        ELSE                  !out to be zero
          up= -1
        ENDIF
        down= -up
      ENDIF
*
      gen_event_sequence_N= gen_event_sequence_N+1  !from beginning
*
      call FBgen_scan_type(EvType,physics,control,buffer,ABORT,err)
      IF(ABORT) THEN
*
        call G_add_path(here,err)
        RETURN
*
      ELSEIF(physics) THEN
*
        gen_run_total_events= gen_run_total_events+1
        gen_event_type= EvType
*
        ABORT= EvType.LT.0 .or. EvType.GT.gen_MAX_trigger_types
        If(ABORT) Then
          iv(1)= EvType
          iv(2)= gen_event_sequence_N
          call G_build_note(':illegal physics type#$ sequential#$',
     &                                        '$',iv,' ',rv,' ',err)
          call G_add_path(here,err)
          RETURN
        EndIf
*
        process= gen_run_enable(EvType)
*
        gen_run_triggered(EvType)= gen_run_triggered(EvType)+1
*
*-likely that next bank is an "Event ID bank"; if so try to
*-recover event info, if not just skip
*
        nontrivial= buffer(1).GE.6            !non-null CODA physics event
*
        If(nontrivial) Then
          EventIDbank= buffer(1).GE.6 .and. 
     &                   buffer(3).EQ.EventIDbank_size  
     &                       .and. buffer(4).EQ.EventIDbank_desc
*
          if(EventIDbank) then
*
            gen_event_ID_number= buffer(5)
            gen_event_class= buffer(6)
            gen_event_ROC_summary= buffer(7)
*
*-see if event_ID within limits of interest
            IF(gen_run_starting_event.GT.0) THEN
              process= gen_event_ID_number.GE.gen_run_starting_event
            ENDIF
*
            IF(gen_run_stopping_event.GE.gen_run_starting_event
     &                       .and. gen_run_stopping_event.GT.0) THEN
              process= gen_event_ID_number.LE.gen_run_stopping_event
            ENDIF
*
          else                   !1st bank NOT eventID bank-must look later
*
            gen_event_ID_number= 0
            gen_event_class= 0
            gen_event_ROC_summary= 0
*
          endif
*
        Else                              !trivial event- nothing inside
*
          iv(1)= gen_event_sequence_N
          iv(2)= EvType
          iv(3)= buffer(1)+1
          call G_build_note(':sequential event#$ type#$ '//
     &                       'too small [$]','$',iv,' ',rv,' ',err)
          call G_add_path(here,err)       !warning only
*
        EndIf
*
      ELSEIF(control) THEN
*
        process= .FALSE.
        gen_event_ID_number= 0
        gen_event_type= 0
        gen_event_class= 0
*
        If(EvType.EQ.SYNC_EvType) Then
*
          gen_run_UTC_last= buffer(3)
          gen_run_total_events= buffer(5)
          call g_UTC_date(gen_run_UTC_last,gen_run_date_last,
     &                                      dy,mth,yr,hr,min,sec)
*
          status= buffer(6)
          bad_sync= status.NE.0
          ABORT= bad_sync
          if(bad_sync) then
            err= ' '
            DO nth=0,31
              If(BTEST(status,nth*bit)) Then
                call G_build_note(', ROC#$','$',nth,' ',rv,' ',msg)
                call G_append(err,msg)
              EndIf
            ENDDO
            call G_build_note('event#$','$',gen_run_total_events,
     &                          ' ',rv,' ',note)
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
     &                                      dy,mth,yr,hr,min,sec)
*
          gen_event_sequence_N= 1          !start counting over
          gen_run_total_events= 1
*
          do m=0,gen_MAX_trigger_types !clear triggered table
            gen_run_triggered(m)= 0
          enddo
*
          iv(1)= gen_run_number
          iv(2)= gen_run_type
          call G_build_note('INFO:PRESTART Run#$ type#$ '//
     &                gen_run_date_start,'$',iv,' ',rv,' ',msg)
          call G_log_message(msg)
*
        ElseIf(EvType.EQ.GO_EvType) Then
*
          gen_run_UTC_start= buffer(3)
          call g_UTC_date(gen_run_UTC_start,gen_run_date_start,
     &                                      dy,mth,yr,hr,min,sec)
*
          msg= 'INFO:GO '//gen_run_date_start
          call G_log_message(msg)
*
        ElseIf(EvType.EQ.PAUSE_EvType) Then
*
          gen_run_UTC_last= buffer(3)
          gen_run_total_events= buffer(5)
          call g_UTC_date(gen_run_UTC_last,gen_run_date_last,
     &                                      dy,mth,yr,hr,min,sec)
          msg= 'INFO:PAUSE '//gen_run_date_last
          call G_log_message(msg)
*
        ElseIf(EvType.EQ.END_EvType) Then
*
          gen_run_UTC_stop= buffer(3)
          gen_run_total_events= buffer(5)
          call g_UTC_date(gen_run_UTC_stop,gen_run_date_stop,
     &                                      dy,mth,yr,hr,min,sec)
*
          msg= 'INFO:END '//gen_run_date_stop
          call G_log_message(msg)
*
        EndIf
*
      ELSE
*
        process= .FALSE.
        gen_event_ID_number= 0
        gen_event_type= 0
        gen_event_class= 0
*
      ENDIF
*
      RETURN
      END
