      SUBROUTINE G_rep_err(ABORT,note)
*----------------------------------------------------------------------
*- 
*-   Purpose and Methods : if status ABORT (.NOT.OK)  output the "note" 
*- 
*-   Inputs  : OK	- status 
*-           : note	- error message
*- 
*-   Created  26-MAR-1992   Kevin B. Beard 
*-   Modified for hall C 9/1/93: KBB
*-   Modified 11/19/93 for warning: KBB
*     $Log$
*     Revision 1.1  1994/02/09 14:17:33  cdaq
*     Initial revision
*
*- 
*----------------------------------------------------------------------
      IMPLICIT NONE
      SAVE 
      logical ABORT
      character*(*) note 
*
      logical warning        !control this routine via COMMON
*
*----------------------------------------------------------------------
*
      warning= note.NE.' '
*
      IF(.NOT.ABORT .and. .NOT.warning) THEN
*
        RETURN
*
      ELSEIF(ABORT) THEN
*
        call G_wrap_note(6,'ERROR: '//note)     !replace IO=6 w/ parameter
*                                               !shortly....
      ELSE
*
        call G_wrap_note(6,'WARNING: '//note)
*
      ENDIF
*
      RETURN 
      END
