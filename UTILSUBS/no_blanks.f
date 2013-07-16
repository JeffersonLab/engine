       SUBROUTINE NO_blanks(string)
*
* $Log: no_blanks.f,v $
* Revision 1.1  1994/02/22 20:01:14  cdaq
* Initial revision
*
*
       character*(*) string
       integer nonblank_length
c
c      strips out blanks and tabs
c
       if(string.eq.' ') RETURN
       call squeeze(string,nonblank_length)
       return
       end
