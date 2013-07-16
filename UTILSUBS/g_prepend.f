      SUBROUTINE G_prepend(prefix,suffix)
*----------------------------------------------------------------------
*- 
*-   Purpose and Methods : Put "prefix" as a prefix on the message
*- 
*-   Inputs  : prefix	- prefix to add onto suffix
*-           : suffix   - suffix to be prepended
*-   Outputs : suffix	- prepended suffix
*- 
*-   Created  7-Dec-1993   Kevin B. Beard, Hampton U. 
*     $Log: g_prepend.f,v $
*     Revision 1.1  1994/02/09 14:17:14  cdaq
*     Initial revision
*
*-
*-note: Taken from hall B package.
*----------------------------------------------------------------------
      IMPLICIT NONE
      SAVE 
      character*(*) prefix,suffix
*
      integer m
      character*5000 msg
      character*1 last
*
      INCLUDE 'gen_routines.dec'
*
*----------------------------------------------------------------------
*
        IF(prefix.EQ.' ') THEN
          RETURN                        !no change to suffix
        ELSEIF(suffix.EQ.' ') THEN
          suffix= prefix                !just copy
          RETURN
        ENDIF
*
	msg= prefix
	call no_leading_blanks(msg)
	m= G_important_length(msg)
        last= msg(m:m)
*
	call no_leading_blanks(suffix)
	msg(m:)= last//suffix            !prevents illegal access
	suffix= msg                      !in case
*
        RETURN 
        END
