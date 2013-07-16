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
*     $Log: g_build_note.f,v $
*     Revision 1.3  1994/06/08 17:40:56  cdaq
*     (KBB) new version
*
* Revision 1.2  1994/02/17  20:56:34  cdaq
*   Fmt also for integers
*   fmt control words: "X" "Z" "HEX"       hexadecimal
*                      "B" "BIN"           binary
*   fmt decided on basis of F or E present, "()" added if needed
*
* Revision 1.1  1994/02/09  14:15:01  cdaq
* Initial revision
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
      character*20 fmtR,fmtI,FT
      character*40 pad,new
      logical wldI,wldR,binary,real_fmt,int_fmt
      character*8 dflt_fmtR,dflt_fmtI,dflt_fmtZ
      parameter (dflt_fmtR= '(f20.3)')
      parameter (dflt_fmtI= '(i20)')
      parameter (dflt_fmtZ= '(z10)')
*
*----------------------------------------------------------------------
*
      wldI= wildI.NE.' '
      wldR= wildR.NE.' '
*
      IF(.NOT.wldI .and. .NOT.wldI) THEN
        note= pat                       !do nothing
        call only_one_blank(note)       !leave only 1 consecutive blank 
        RETURN
      ENDIF
*
      msg= pat
      call only_one_blank(msg)	!leave only 1 consecutive blank 
*
      binary= .FALSE.           !assume not a binary dump
      fmtI= dflt_fmtI
      fmtR= dflt_fmtR
      call ShiftAll(fmt,FT)
      call NO_leading_blanks(FT)
      IF(FT.EQ.'Z' .or. FT.EQ.'X' .or. FT(1:3).EQ.'HEX') THEN     !hexadecimal
        fmtI= dflt_fmtZ
        fmtR= dflt_fmtR
        FT= ' '
      ELSEIF(FT.EQ.'B' .or. FT(1:3).EQ.'BIN') THEN                !binary
        fmtI= dflt_fmtZ
        fmtR= dflt_fmtR
        binary= .TRUE.
        FT= ' '
      ELSEIF(FT.NE.' ' .and. FT(1:1).NE.'(') THEN                 !add "()"
        pad= '('//FT//')'
        FT= pad
        call only_one_blank(FT)
      ENDIF
*
      real_fmt= (.NOT.wldI .and. FT.NE.' ') .or. 
     &                    INDEX(FT,'F')+INDEX(FT,'E').GT.0
      int_fmt= (.NOT.wldR .and. FT.NE.' ') .or. 
     &                    INDEX(FT,'I')+INDEX(FT,'Z').GT.0
      IF(real_fmt) THEN
        fmtR= FT
      ELSEIF(int_fmt) THEN
        fmtI= FT
      EndIf
*
      N= 0
      w= wildI
      i= INDEX(msg,w)
      DO WHILE (w.NE.' ' .and. i.GT.0)
        N= N+1
        new= '****<'//fmtI//'>****'                 !in case of write error
        call NO_blanks(new)
        write(pad,fmtI,err=666) Ival(N)
        new= pad
        If(binary) Then                      !substitue binary symbols for hex symbols
          call sub_string(new,'0','0000')
          call sub_string(new,'1','0001')
          call sub_string(new,'2','0010')
          call sub_string(new,'3','0011')
          call sub_string(new,'4','0100')
          call sub_string(new,'5','0101')
          call sub_string(new,'6','0110')
          call sub_string(new,'7','0111')
          call sub_string(new,'8','1000')
          call sub_string(new,'9','1001')
          call sub_string(new,'A','1010')
          call sub_string(new,'B','1011')
          call sub_string(new,'C','1100')
          call sub_string(new,'D','1101')
          call sub_string(new,'E','1110')
          call sub_string(new,'F','1111')
        EndIf
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
      N= 0
      w= wildR
      i= INDEX(msg,w)
      DO WHILE (w.NE.' ' .and. i.GT.0)
        N= N+1
        new= '****<'//fmtR//'>****'                 !in case of write error
        call NO_blanks(new)
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
        i= INDEX(msg,w)
      ENDDO
*
      note= msg
*
      RETURN
      END
