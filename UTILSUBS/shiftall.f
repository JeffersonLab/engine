       SUBROUTINE SHIFTall(InPut,OUTPUT) 
*
* $Log: shiftall.f,v $
* Revision 1.1  1994/02/22 20:02:34  cdaq
* Initial revision
*
*
       character*(*) InPut,OUTPUT 
c
c      shifts strings to upper case, removes all tabs,nulls & leading blanks
c
	OutPut=InPut
	call UP_shift(OutPut)
	call NO_leading_blanks(OUTPUT)
	return
       end 
