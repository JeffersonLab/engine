      SUBROUTINE G_IO_control(IO,command,ABORT,err)
*--------------------------------------------------------
*-
*-   Purpose : Serve as a clearing house for all FORTRAN IO channels
*- 
*-    Input: IO         - FORTRAN IO channel [command="RESERVE","FREE"]
*-           command    - "R","C","A","?"
*-   Output: IO		- FORTRAN IO channel [command="ANY","?"]
*-           ABORT	- success or failure
*-           err	- reason for failure, if any
*- 
*-   Created   8-Apr-1994   Kevin B. Beard, Hampton U.
*-   Modified 11-Apr-1994   KBB; added FREE, removed CLOSE, 
*-                               more efficient parsing
* $Log: g_io_control.f,v $
* Revision 1.4  1996/11/22 17:07:02  saw
* (SAW) Change .eq. to .eqv. for AIX compatibility
*
* Revision 1.3  1996/09/05 21:05:48  saw
* (SAW) Reduce max lun from 100 to 99 for linux compatibility
*
* Revision 1.2  1996/05/24 16:04:33  saw
* (SAW) Relocate data statements for f2c compatibility
*
* Revision 1.1  1994/04/12 16:06:00  cdaq
* Initial revision
*
*-
*-commands: FREE    -given channel IO, mark IDLE, but don't FORTRAN CLOSE it
*-          RESERVE -given channel IO, see if it's free and if so
*-                   mark it .NOT.IDLE; if already IDLE, report error
*-          ANY or ? -put the next free channel into IO and mark it .NOT.IDLE
*-
*- All standards are from "Proposal for Hall C Analysis Software
*- Vade Mecum, Draft 1.3" by D.F.Geesamn and S.Wood, Csoft-NOTE-94-001
*-
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*12 here
      parameter (here= 'G_IO_control')
*     
      integer IO
      logical ABORT
      character*(*) command,err
*
      LOGICAL MATCH     !UTILSUBS FUNCTION
*
      integer gMAX_IO_channels_allowed
      parameter (gMAX_IO_channels_allowed= 99)
      logical IDLE
      parameter (IDLE= .FALSE.)
      logical request,OK
      character*20 cmd
      character*80 pat
      real rv
      integer m
      logical LUN_channel(gMAX_IO_channels_allowed)
      data LUN_channel/gMAX_IO_channels_allowed*IDLE/
*
*--------------------------------------------------------
      err= ' '
*
      LUN_channel(5)= .NOT.IDLE                   !always in use
      LUN_channel(6)= .NOT.IDLE                   !always in use
*
      cmd= command
*-most common command forms-
      IF(cmd.EQ.'FREE' .or. cmd.EQ.'FINISHED') THEN
        cmd= 'F'
      ELSEIF(cmd.EQ.'ANY' .or. cmd.EQ.'ASK') THEN
        cmd= 'A'
      ELSEIF(cmd.EQ.'RESERVE' .or. cmd.EQ.'REQUEST') THEN
        cmd= 'R'
      ELSE                 !mixed case & VMS-like abbreviations allowed
        If(MATCH(cmd,'F*ree') .or. MATCH(cmd,'F*inished')) Then
          cmd='F'
        ElseIf(MATCH(cmd,'A*ny') .or. MATCH(cmd,'A*sk')) Then
          cmd='A'
        ElseIf(MATCH(cmd,'R*eserve') .or. MATCH(cmd,'R*equest')) Then
          cmd='R'
        Else
          call g_Shift_len(command,cmd,m)   !shift it to upper case
        EndIf
      ENDIF
*
      IF(cmd.eq.'F') Then                !FREE up a channel
*
        ABORT= 1.GT.IO .and. IO.GT.gMAX_IO_channels_allowed
        If(ABORT) Then
          pat= ':illegal FORTRAN IO channel#$ cannot be freed'
          call G_build_note(pat,'$',IO,' ',rv,' ',err)
        Else
          ABORT= IO.eq.5 .or. IO.eq.6
          if(ABORT) then
            pat= ':FORTRAN IO channel#$ special; cannot be freed'
            call G_build_note(pat,'$',IO,' ',rv,' ',err)
          else
            ABORT= LUN_channel(IO).EQV.IDLE  !already idle
            IF(ABORT) THEN
              pat= ':FORTRAN IO channel#$ already IDLE'
              call G_build_note(pat,'$',IO,' ',rv,' ',err)
            ENDIF
            LUN_channel(IO)= IDLE
          endif
        EndIf
*
      ELSEIF(cmd.EQ.'A') Then            !get ANY channel
*
        DO IO=gMAX_IO_channels_allowed,1,-1    !for max compat.; start at top
          If(LUN_channel(IO) .eqv. IDLE) Then
            LUN_channel(IO)= .NOT.IDLE
            ABORT= .FALSE.
            RETURN
          EndIf
        ENDDO
        ABORT= .TRUE.
        IO= gMAX_IO_channels_allowed
        err= ':no FORTRAN IO channels IDLE from 1-#?'
        call G_build_note(pat,'$',IO,' ',rv,' ',err)
        IO= 0
*
      ELSEIF(cmd.EQ.'R') THEN            !RESERVE a channel
*
        ABORT= 1.GT.IO .and. IO.GT.gMAX_IO_channels_allowed
        If(ABORT) Then
          pat= ':illegal FORTRAN IO channel#$ cannot be reserved'
          call G_build_note(pat,'$',IO,' ',rv,' ',err)
          call G_add_path(here,err)
        Else
          ABORT= LUN_channel(IO) .neqv. IDLE
          if(ABORT) then
            pat= ':FORTRAN IO channel#$ not IDLE-'//
     &                                   'cannot be reserved'
            call G_build_note(pat,'$',IO,' ',rv,' ',err)
          else
            LUN_channel(IO)= .NOT.IDLE
          endif  
        EndIf
*
      ELSE
*
         ABORT= .TRUE.
         err= ':command "'//cmd(1:m)//'" not supported'
*
      ENDIF
*
      IF(ABORT) call G_add_path(here,err)
      RETURN
      END
