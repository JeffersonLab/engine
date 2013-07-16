      SUBROUTINE G_log_message(note)
*----------------------------------------------------------------------
*- 
*-   Purpose and Methods : Output "note"; allow future steering etc.
*- 
*-   Inputs  : note	- error message
*- 
*-   Created  20-NOV-1993   Kevin B. Beard, Hampton U. 
*-   Modified  7-Dec-1993   KBB; updated for new include file
*     $Log: g_log_message.f,v $
*     Revision 1.1  1994/02/09 14:15:58  cdaq
*     Initial revision
*
*- 
*----------------------------------------------------------------------
      IMPLICIT NONE
      SAVE 
      character*(*) note 
*
      INCLUDE 'gen_output_info.cmn'
*
      DATA G_OUTPUT_OK/.FALSE./
      DATA G_OUTPUT_tty/6/
*
*----------------------------------------------------------------------
*
      IF(g_OUTPUT_OK) call G_wrap_note(g_OUTPUT_channel,note)
*
      call G_wrap_note(G_OUTPUT_tty,note)
*
      RETURN 
      END
