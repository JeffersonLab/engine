      SUBROUTINE G_wrap_note(IOchannel,note)
*----------------------------------------------------------------------
*- 
*-   Purpose and Methods : send to output a (possibly) long message
*-                         with wrapping if necc.
*- 
*-   Inputs  : IOchannel	- FORTRAN IO channel used for errors
*-           : note		- message
*- 
*-   Created  2-Dec-1992   Kevin B. Beard 
*-   Modified for hall C 9/1/93: KBB
*     $Log: g_wrap_note.f,v $
*     Revision 1.4  1996/09/05 21:07:18  saw
*     (SAW) Watch for Null's
*
*     Revision 1.3  1994/06/17 03:05:37  cdaq
*     (SAW) Get correct copy of improved line wrapping from KBB
*
* Revision 1.2  1994/06/06  02:57:10  cdaq
* (KBB) Improve line wrapping by looking for good breakpoints
*
* Revision 1.1  1994/02/09  14:18:43  cdaq
* Initial revision
*
*- 
*----------------------------------------------------------------------
      IMPLICIT NONE
      SAVE 
      integer IOchannel
      character*(*) note 
*
      integer m,max_len,pref
      character*1024 msg
      logical break,indent
      character*1 blank
      parameter (blank=' ')	!blank character
*
      INCLUDE 'gen_routines.dec'
*
      character*20 breakpt(3)   !acceptable break points in order of
                                !decreasing preference     
      data breakpt/ ' >]}),;:!&|?', '<[{(+-?=*#', '$^`"''_/.~%@'/

*----------------------------------------------------------------------
*
      msg= note				!truncate after 1024 characters
      call clear_after_null(msg)        !clear out string after a null character.
      call NO_tabs(msg)			!replace tabs w/ blanks (if any)
      call NO_leading_blanks(msg)	!remove leading blanks (if any)
      call only_one_blank(msg)		!leave only 1 consecutive blank 
      m= G_important_length(msg)	!return usefull length
      max_len= 78			!less than max char./line
      indent= .FALSE.			!don't indent 1st line
*
      DO WHILE(msg.NE.blank)            !keep cycling until message gone
*     
         break= m.LE.max_len            !look for a break (ok if short enough)
*     
         If(.not.break) Then
           m= max_len                             !start at end-of-line down
           do pref=1,3                            !three tries
             DO while(.not.break .and. m.GT.2)    !look for good break
               m= m-1                             !but quit if too short
               break= INDEX(breakpt(pref),msg(m:m)).NE.0   !seek in order
             ENDDO
           enddo
           if(.NOT.break) m= max_len      !break in the middle of a word
         EndIf
*
         If(indent) then
            write(IOchannel,'(6x,a)',err=1) msg(1:m) !indent 5 extra spaces
         Else
            write(IOchannel,'(1x,a)',err=1) msg(1:m) !do not indent
            max_len= max_len - 5        !decrease # significant char./line
            indent= .TRUE.		!indent from now on
         EndIf
*     
         msg(1:m)= blank                !erase what's been just output
         call NO_leading_blanks(msg)	!shift
         m= G_important_length(msg)	!get new length
*     
      ENDDO
 1    RETURN                            !if unable to write, just give up
      END
