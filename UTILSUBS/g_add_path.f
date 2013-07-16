      SUBROUTINE G_add_path(where,mss)
*----------------------------------------------------------------------
*- 
*-   Purpose and Methods : Put "where" as a prefix on the message
*- 
*-   Inputs  : where	- location (subroutine name usually)
*-           : mss      - error message
*-   Outputs : mss	- prpended error message
*- 
*-   Created  20-Nov-1993   Kevin B. Beard, Hampton U. 
*-    $Log: g_add_path.f,v $
*-    Revision 1.1  1994/02/09 14:13:19  cdaq
*-    Initial revision
*-
*-
*-note: Taken from hall B package.
*----------------------------------------------------------------------
      IMPLICIT NONE
      SAVE 
      character*(*) where,mss
*
      integer m
      character*1 first,last
      character*1024 msg
      logical marker
*
      INCLUDE 'gen_routines.dec'
*
*----------------------------------------------------------------------
*
	msg= where
	call no_leading_blanks(msg)
	call no_leading_blanks(mss)
	m= G_important_length(msg)
*
	first= mss(1:1)
	last= msg(m:m)
*
        marker= last.EQ.'>' .or. last.EQ.':' .or. 
     &		last.EQ.'<' .or. first.EQ.':' .or.
     &		first.EQ.'>' .or. first.EQ.'<'
*
	If(marker) Then
	   msg(m:)= last//mss
	Else
	   msg(m:)= last//'>'//mss
	EndIf
*
	mss= msg
*
        RETURN 
        END
