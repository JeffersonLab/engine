	SUBROUTINE UP_case(string)
*
* $Log: up_case.f,v $
* Revision 1.1  1994/02/22 20:03:16  cdaq
* Initial revision
*
*
	IMPLICIT NONE
	character*(*) string
	integer len_string,char,m,j
	integer string_length	!FUNCTION
	character*26 lo,HI
	data lo/'abcdefghijklmnopqrstuvwxyz'/
	data HI/'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
*
*	shifts string to upper case
*
	len_string= string_length(string)
	m=1		!stop looking when only blanks remain
	DO WHILE(m.LE.len_string .and. string(m:).NE.' ')
	  j= INDEX(lo,string(m:m))
	  if(j.NE.0) string(m:m)= HI(j:j)
	  m= m+1
	ENDDO
	RETURN
	END
