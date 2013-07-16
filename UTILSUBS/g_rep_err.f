      SUBROUTINE G_rep_err(ABORT,note)
*----------------------------------------------------------------------
*- 
*-   Purpose and Methods : if status ABORT (.NOT.OK)  output the "note" 
*- 
*-   Inputs  : OK	- status 
*-           : note	- error message
*- 
*-   Created  26-MAR-1992   Kevin B. Beard 
*-   Modified for hall C 9/1/93: KBB
*-   Modified 11/19/93 for warning: KBB
*     $Log: g_rep_err.f,v $
*     Revision 1.4  1996/11/22 17:08:00  saw
*     (SAW) Cleanup
*
*     Revision 1.3  1995/03/21 15:35:48  cdaq
*     (SAW) Replace variable min with minute
*
* Revision 1.2  1994/06/06  13:30:35  cdaq
* (KBB) Add date/time info.  Return on warning's.
*
* Revision 1.1  1994/02/09  14:17:33  cdaq
* Initial revision
*
*- 
*----------------------------------------------------------------------
      IMPLICIT NONE
      SAVE 
      logical ABORT
      character*(*) note 
*
      INCLUDE 'gen_output_info.cmn'
      INCLUDE 'gen_run_info.cmn'
      INCLUDE 'gen_event_info.cmn'
      INCLUDE 'gen_routines.dec'
*
      logical warning,valid_run_info
      character*80 msg,time
      integer iv(10),dy,mth,yr,hr,minute,sec
      real rv(10)
      integer lentemp
*
*----------------------------------------------------------------------
*
      warning= note.NE.' '
*
      IF(.NOT.ABORT .and. .NOT.warning) RETURN    !do nothing
*
      valid_run_info= gen_run_number.GT.0 .or. 
     &                            gen_event_ID_number.GT.0
*
      IF(valid_run_info) THEN
*
         iv(1)= gen_run_number
         iv(2)= gen_event_ID_number
         iv(3)= gen_event_type
         iv(4)= gen_event_class
*     
         If(gen_run_UTC_last.NE.0) Then
            call g_UTC_date(gen_run_UTC_last,gen_run_date_last,
     &           dy,mth,yr,hr,minute,sec)
            time= gen_run_date_last
         Else
            time= ' '
         EndIf
*     
         call G_build_note('>>> Run#$ Event#$ Type#$ Class#$ '//
     &        time,'$',iv,' ',rv,' ',msg)
*     
         call G_wrap_note(G_OUTPUT_tty,msg)
*     
      ENDIF
*     
      lentemp = g_important_length(note)
      IF(ABORT) THEN
*     
        call g_prepend('ERROR: ',note)
        call G_wrap_note(G_OUTPUT_tty,note)
*     
      ELSE
*     
        call g_prepend('WARNING: ',note)
        call G_wrap_note(G_OUTPUT_tty,note)
*     
      ENDIF
*     
      RETURN 
      END
