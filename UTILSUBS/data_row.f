c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c
c  Handy & general string manipulations.
c
c     $Log: data_row.f,v $
c     Revision 1.3  1994/02/02 18:45:17  cdaq
c     Add Log keyword
c
c========================================================================
       SUBROUTINE data_row(string) 
       IMPLICIT NONE
       SAVE
       character*(*) string
       character*1024 pad
       integer out,imp,i,m,LENGTH
       logical last_blank,last_comma
       integer string_length	!FUNCTION
       integer g_important_length !FUNCTION
c
c      eliminates adjacent blanks, comments, packs with ","s 
c	ex: "1   2,	3" =>  "1,2,3,,,,,,,,,,,,,,,,,,,,"
c
	LENGTH= string_length(string)       !total length
        pad= string
	call NO_nulls(pad)			!remove nulls
	call NO_tabs(pad)			!remove tabs
	call NO_comments(pad)		!remove comments
	call NO_leading_blanks(pad)		!remove leading blanks
        call only_one_blank(pad)             !remove redundant blanks
        imp= g_important_length(pad)           !only nonblank length
c
        m= INDEX(pad,', ')
	DO WHILE(m.LT.imp .and. m.GT.0)
           string= pad(m+2:)
	   pad(m+1:)= string
           m= INDEX(pad,', ')
        ENDDO
*
        m= INDEX(pad,' ,')
        DO WHILE(m.LT.imp .and. m.GT.0)
           string= pad(m+1:)
           pad(m:)= string
           m= INDEX(pad,' ,')
        ENDDO
*
        m= INDEX(pad,' ')
        DO WHILE(m.LT.imp .and. m.GT.0)
           pad(m:m)= ','
           m= INDEX(pad,' ')
        ENDDO
*
        imp= g_important_length(pad)
        string= pad(1:imp)
        DO i= imp+1,LENGTH
           string(i:i)= ','
        ENDDO
        RETURN
        END
