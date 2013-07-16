        SUBROUTINE only_one_blank(string) 
*
* $Log: only_one_blank.f,v $
* Revision 1.1  1994/02/22 20:02:18  cdaq
* Initial revision
*
*
        IMPLICIT NONE 
        CHARACTER*(*) string
        CHARACTER*1024 line 
        INTEGER i 
C		eliminate tabs,leading blanks, multiple blanks
        CALL NO_tabs(string)
        CALL NO_leading_blanks(string)
        i= INDEX(string,'  ')                !2 blanks
        DO WHILE (i.NE.0 .AND. string(max(i,1):).ne.' ')
          line= string(i+1:)              !skip 1st blank 
          string(i:)= line                !shift left 
          i= INDEX(string,'  ')           !look again 
        ENDDO 
        RETURN
        END 
