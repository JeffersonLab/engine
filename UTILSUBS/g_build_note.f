      SUBROUTINE G_build_note(pat,wildI,Ival,wildR,Rval,fmt,note)
*----------------------------------------------------------------------
*- 
*-   Purpose and Methods : Insert integer and real values into a message.
*- 
*-   Inputs  : pat	        - string to copy before substitution
*-           : wildI            - character to replace with real values
*-           : Ival		- integer values to insert
*-           : wildR            - character to replace with real values
*-           : Rval             - real values to insert
*-           : fmt              - character string format for real values
*-           : note		- new message
*- 
*-   Created  20-Nov-1993 Kevin B. Beard, Hampton U.
*-   Modified 18-Jan-1994 K.B.Beard, HU, so wild=" " gets skipped
*     $Log$
*     Revision 1.1  1994/02/09 14:15:01  cdaq
*     Initial revision
*
*-
*-   example: 
*-         Ival(1)= 8
*-         Ival(2)= 3
*-         call G_build_note('message#@ is #@','@',Ival,' ',Rval,' ',msg)
*-         => msg= 'message#8 is #3'
*-
*----------------------------------------------------------------------
      IMPLICIT NONE
      SAVE 
*
      character*(*) pat,wildI,wildR,fmt,note 
      integer Ival(*)
      real Rval(*)
*
      integer N,i,m
      character*1024 msg,tmp
      character*1 w
      character*20 Rfmt
      character*30 fmtR,pad,new
*
      INCLUDE 'gen_routines.dec'
*
*----------------------------------------------------------------------
*
      IF(wildI.EQ.' ' .and. wildR.EQ.' ') THEN
        note= pat
        RETURN
      ENDIF
*
      msg= pat
      call only_one_blank(msg)	!leave only 1 consecutive blank 
*
      N= 0
      w= wildI
      i= INDEX(msg,w)
      DO WHILE (w.NE.' ' .and. i.GT.0)
        N= N+1
        new= '********'                 !in case of write error
        write(pad,'(I20)',err=666) Ival(N)
        new= pad
666     call squeeze(new,m)            !squeeze
        If(i.EQ.1) Then
          tmp= new(1:m)//msg(2:)
          msg= tmp
        Else
          tmp= msg(1:i-1)//new(1:m)//msg(i+1:)
          msg= tmp
        EndIf       
        call only_one_blank(msg)	!leave only 1 consecutive blank 
        i= INDEX(msg,w)
      ENDDO
*
      IF(wildR.NE.' ' .and. fmt.NE.' ') THEN
        Rfmt= fmt                       !in case fmt a literal argument
        call NO_blanks(Rfmt)
        If(Rfmt(1:1).NE.'(') Then       !add ()
          fmtR= '('//Rfmt//')'
          call NO_blanks(fmtR)
        Else                            !don't add ()
          fmtR= Rfmt
        EndIf
      ELSE
        fmtR= '(20F.5)'
      ENDIF
*
      N= 0
      w= wildR
      i= INDEX(msg,w)
      DO WHILE (w.NE.' ' .and. i.GT.0)
        N= N+1
        new= '********'                 !in case of write error
        write(pad,fmtR,err=777) Rval(N)
        new= pad
777     call squeeze(new,m)            !squeeze
        If(i.EQ.1) Then
          tmp= new(1:m)//msg(i+1:)
          msg= tmp
        ElseIf(i.GT.1) Then
          tmp= msg(1:i-1)//new(1:m)//msg(i+1:)
          msg= tmp
        EndIf       
        call only_one_blank(msg)	!leave only 1 consecutive blank 
      ENDDO
*
      note= msg
*
      RETURN
      END
