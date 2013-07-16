	SUBROUTINE UP_shift(InPut)
*
* $Log: up_shift.f,v $
* Revision 1.1  1994/02/22 20:03:26  cdaq
* Initial revision
*
*
	character*(*) InPut
	integer tab
c
c      shifts strings to upper case, replaces nulls&tabs with spaces
c
	if(InPut.eq.' ') return 
	call NO_nulls(INPUT)
	call NO_tabs(INPUT)
	call UP_case(InPut)
	RETURN
	END
