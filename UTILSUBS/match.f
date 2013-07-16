	LOGICAL FUNCTION match(test,pattern)
*
* $Log: match.f,v $
* Revision 1.1  1994/02/22 20:01:00  cdaq
* Initial revision
*
*
	IMPLICIT NONE 
	CHARACTER*(*) test,pattern
	CHARACTER*132 a,b,b_
	INTEGER*4 i,j,k,m,n,star,B_min,A_min,j_ 
	LOGICAL*2 ok
c.................................................................
c
c  require all of "test" match "pattern", require at least up to "*" 
C  EX: test="cl" matches pattern="CL*EAR" but not pattern="CLEAR"
C      test="clx" does not match-   [case insensitive]
c
        a= test 
        b= pattern
	If(a.eq.' '.or.b.eq.' ') Then
	   match=.FALSE. 
	   RETURN
	EndIf
        i= INDEX(a,':') 
        IF(i.GT.0) a(i:)=' '
        i= INDEX(a,'=') 
        IF(i.GT.0) a(i:)=' '
        i= INDEX(a,'*') 
        IF(i.GT.0) a(i:)=' '
        CALL shiftall(a,a)
        CALL shiftall(b,b)
        CALL squeeze(a,i) 
        CALL squeeze(b,j) 
        star= INDEX(b,'*')
        IF(star.GT.0) THEN
          b_= b(1:star-1)//b(star+1:) 
          ok= a(1:star-1).EQ.b_(1:star-1) 
          if(i.ge.star) ok= ok .AND. a(1:i).EQ.b_(1:i)
        ELSEIF(star.eq.1) THEN
          ok= .TRUE.
        ELSE
          ok= a.EQ.b
        ENDIF 
        match= ok 
        RETURN
        END
