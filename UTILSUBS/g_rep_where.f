      SUBROUTINE G_rep_where(note)
*----------------------------------------------------------------------
*- 
*-   Purpose and Methods : report current location&time in run
*- 
*-   Outputs  : note	- current location & time
*- 
*-   Created  8Jun1994 Kevin B. Beard, Hampton U.
*-
*- form: 
*    Run#[i] Event sequence#[j] ID#[k] Type#[m] Class#[n] date time zone
*- example: 
*    Run#332 Event sequence#55 ID#51 Type#1 Class#4 8-Jun-1994 14:11:33 GMT
*
* $Log: g_rep_where.f,v $
* Revision 1.2  1995/03/21 15:35:00  cdaq
* (SAW) Replace variable min with minute
*
c Revision 1.1  1994/07/08  18:45:43  cdaq
c Initial revision
c
*----------------------------------------------------------------------
      IMPLICIT NONE
      SAVE 
*
      CHARACTER*(*) note 
*
      INCLUDE 'gen_output_info.cmn'
      INCLUDE 'gen_run_info.cmn'
      INCLUDE 'gen_event_info.cmn'
*
      character*80 time
      character*160 msg
      integer iv(10),dy,mth,yr,hr,minute,sec
      real rv(10)
*
      character*50 pttrn
      parameter (pttrn= 'Run#$ Event sequence#$ id#$ Type#$ Class#$')
*
*----------------------------------------------------------------------
*
      iv(1)= gen_run_number          !current run ID number
      iv(2)= gen_event_sequence_N    !total number of events (control&physics)
      iv(3)= gen_event_ID_number     !current, unique physics event ID
      iv(4)= gen_event_type          !current trigger type
      iv(5)= gen_event_class         !current classification
*     
      If(gen_run_UTC_last.NE.0) Then              !update date
        call g_UTC_date(gen_run_UTC_last,gen_run_date_last,
     &                                  dy,mth,yr,hr,minute,sec)
        time= gen_run_date_last
      Else                                        !ignore date
        time= ' '
      EndIf
*     
      call G_build_note(pttrn//' '//time,
     &                     '$',iv,' ',rv,' ',msg) !fill in "$"s
*     
      note= msg                                   !copy
*     
      RETURN 
      END
