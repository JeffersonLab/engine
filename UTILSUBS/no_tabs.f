	SUBROUTINE NO_tabs(line)
*
* $Log: no_tabs.f,v $
* Revision 1.1  1994/02/22 20:05:11  cdaq
* Initial revision
*
*
	character*(*) line
	character*1 tab
        integer i
	character*1 CHAR	!FUNCTION
c
c   replaces tabs with blanks
c
	tab= CHAR(9)			!ASCII nine
	i= INDEX(line,tab)
	DO WHILE (i.NE.0)
	  line(i:i)= ' '		!blank
	  i= INDEX(line,tab)
	ENDDO
	RETURN
	END
