        SUBROUTINE get_values(string,n,values,ok) 
*
* $Log$
* Revision 1.1  1994/02/22 20:00:24  cdaq
* Initial revision
*
*
        IMPLICIT NONE 
        CHARACTER*(*) string
        INTEGER*4 n,values(*),v(2),divider
        INTEGER*4 i,j,k,m,value4,cycle,step
        INTEGER*2 last_binary,last_oct,last_hex,dummy2
        LOGICAL*2 ok,hex,oct,bin
        CHARACTER*132 orig,line,this
        INTEGER*4 important_length        !FUNCTION 
        INTEGER*4 INDEX                   !FUNCTION 
        CHARACTER*1 quote 
        PARAMETER (quote='''')
c................................................................ 
        n=0
	orig= string
        CALL no_tabs(orig)			!remove tabs
        DO WHILE (INDEX(orig,quote).ne.0)	!remove quote marks 
          i=INDEX(orig,quote) 
          orig(i:i)=' ' 
        ENDDO 
        DO WHILE (INDEX(orig,'::').ne.0) 	!replace sequence marks
          i=INDEX(orig,'::') 
          orig(i:i+1)='^ '
        ENDDO 
        DO WHILE (INDEX(orig,':').ne.0) 	!replace seperator marks
          i=INDEX(orig,':') 
          orig(i:i)=','
        ENDDO 
        CALL NO_blanks(orig)			!remove blanks
	CALL UP_case(ORIG)			!shift to upper case
        IF(orig.EQ.' ') THEN
          ok=.false.				!nothing to read
          RETURN
        ENDIF 
c
        line= orig
	j= INDEX(line,',')
	IF(j.gt.0) line(j:)=' '		!get first line
c
        DO WHILE (orig.NE.' ')
c
	  divider= INDEX(line,'*')			!duplicate
	  If(divider.eq.0) divider= INDEX(line,'^')	!sequence
c
	  If(divider.eq.0) Then
		cycle=1
		this= line
	  ElseIf(divider.eq.1) Then
                GOTO 2222 			!illegal
	  Else
		cycle=2
		this= line(1:divider-1)
	  EndIf
c
	  Do j=1,cycle
c
            last_binary= INDEX(this,'B')
            bin= last_binary.ne.0 
            last_hex= INDEX(this,'H') 
            If(last_hex.EQ.0) last_hex= INDEX(this,'X') 
            hex= last_hex.ne.0
            last_oct= INDEX(this,'O') 
            oct= last_oct.ne.0
c
            if(hex) then
              this(last_hex:)=' ' 
              CALL squeeze(this,i)
              IF(this.eq.' ') goto 2222
              READ(this(1:i),'(z)',err=2222) v(j)
            elseif(oct) then
              this(last_oct:)=' ' 
              CALL squeeze(this,i)
              IF(this.eq.' ') goto 2222
              READ(this(1:i),'(o)',err=2222) v(j)
            elseif(bin) then
              this(last_binary:)=' '
              CALL squeeze(this,i)
              IF(this.eq.' ') goto 2222
              value4= 0 
              DO k=1,i
                value4= 2*value4
                If(this(k:k).EQ.'1') Then
                  value4= value4+1
                ElseIf(this(k:k).NE.'0') Then
                  GOTO 2222 
                EndIf 
              ENDDO
	      v(j)= value4			!only take lowest bits
            else
              CALL squeeze(this,i)
              IF(this.eq.' ') goto 2222
              READ(this(1:i),'(i)',err=2222) v(j)
            endif
c
	    this= line(divider+1:)	 
          EndDo
c
	  ok=.true.
	  If(cycle.eq.2) Then
	    if(line(divider:divider).eq.'^') then	!sequence "^"
	      DO k=v(1),v(2),MAX(MIN(v(2)-v(1),1),-1)
		n= n+1
		values(n)= k
	      ENDDO	
	    else					!duplicate "*"
	      DO k=1,v(1)
		n=n+1
		values(n)= v(2)
	      ENDDO	
            endif
	  Else						!just single value
	    n=n+1 
            values(n)= v(1)
	  EndIf
c
          m= INDEX(orig,',')		!find next line
          If(m.EQ.0) Then 	!done
            orig=' '
          Else			!another line
            orig(1:m)=' ' 
            CALL no_leading_blanks(orig)
            line= orig 
	    m= INDEX(line,',')
	    if(m.ne.0) line(m:)=' '
	  EndIf
        ENDDO 
        RETURN
c
 2222   ok=.false.
        RETURN
        END 
