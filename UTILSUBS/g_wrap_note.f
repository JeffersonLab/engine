
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
*     $Log$
*     Revision 1.1  1994/02/09 14:18:43  cdaq
*     Initial revision
*
*- 
*----------------------------------------------------------------------
      IMPLICIT NONE
      SAVE 
      integer IOchannel
      character*(*) note 
      integer m,max_len,last_m
      character*1024 msg
      logical break,indent
      character*1 blank,tab
      parameter (blank=' ')	!blank character
      parameter (tab='	')	!tab character
*
      INCLUDE 'gen_routines.dec'
*
*----------------------------------------------------------------------
*
      msg= note				!truncate after 1024 characters
      call NO_tabs(msg)			!replace tabs w/ blanks (if any)
      call NO_leading_blanks(msg)	!remove leading blanks (if any)
      call only_one_blank(msg)		!leave only 1 consecutive blank 
      m= G_important_length(msg)	!return usefull length
      max_len= 78			!one less than max char./line
      indent= .FALSE.			!don't indent 1st line
*
      DO WHILE(msg.NE.blank)	!keep cycling until message gone
*
	last_m= m
	break= m.LT.max_len	!look for a break (ok if short enough)
*
	Do while(.not.break .and. m.GT.2)	!look for good break
	  m= m-1				!but quit if too short
	  break= INDEX('>< :!&|/',msg(m:m)).NE.0	!seek any of these
	EndDo
*					!if can't find good break,
	If(.NOT.break) m= last_m	!break in the middle of a word
*
	If(indent) then
	  write(IOchannel,'(6x,a)',err=1) msg(1:m)	!indent 5 extra spaces
	Else
	  write(IOchannel,'(1x,a)',err=1) msg(1:m)	!do not indent
	  max_len= max_len - 5		!decrease # significant char./line
	  indent= .TRUE.		!indent from now on
	EndIf
*
	msg(1:m)= ' '			!erase what's been just output
	call NO_leading_blanks(msg)	!shift
	m= G_important_length(msg)	!get new length
*
      ENDDO
1     RETURN 			!if unable to write, just give up
      END
