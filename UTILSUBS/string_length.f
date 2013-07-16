c......................................................character operations
	integer FUNCTION string_length(string)
*
* $Log: string_length.f,v $
* Revision 1.1  1994/02/22 20:03:04  cdaq
* Initial revision
*
*
	character*(*) string
c
c			returns the declared length of the string
c
	string_length= LEN(string)
	RETURN
	end
