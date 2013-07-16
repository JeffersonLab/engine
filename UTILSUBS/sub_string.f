	subroutine sub_string(string,old,new)
********************* substitute new char. for old char. in string
* $Log: sub_string.f,v $
* Revision 1.1  1994/04/15 18:12:37  cdaq
* Initial revision
*
	IMPLICIT NONE
	character*(*) string,old,new
	integer i,j,len_old,len_new,len_string
	character*1024 temp
	integer string_length		!FUNCTION
	if(old.EQ.new) RETURN
	len_string= string_length(string)
	len_old= string_length(old)
	len_new= string_length(new)
	j= 0
	i= INDEX(string,old)
	do while (i.NE.0 .and. j.LE.2*len_string+1)
	  j= j+1
	  if(len_old.EQ.len_new) then
	    string(i:i+len_new-1)= new
	  else
	    temp= string(i+len_old:)
	    string(i:)= new//temp
	  endif
	  i= INDEX(string,old)
	enddo
	RETURN
	end
