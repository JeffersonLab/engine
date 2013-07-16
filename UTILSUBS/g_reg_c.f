      SUBROUTINE G_reg_C(name,var,any_FAIL,err)
*--------------------------------------------------------
*     routine to register CTP I4 variables; nice error handling
*
*     Created 7-Jun-1994 K.B.Beard; separate HMS, SOS routines
* Input:  name          - name for CTP to give variable
*         var           - variable 
*         any_FAIL      - success or failure of previous attempts
*         err           - current error message
* Output: any_FAIL      - OR of previous failure or current
*         err           - appended error message
* $Log: g_reg_c.f,v $
* Revision 1.2  1994/06/17 02:51:45  cdaq
* (SAW) Fix typo in here
*
* Revision 1.1  1994/06/17  02:49:58  cdaq
* Initial revision
*
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*8 here
      parameter (here= 'G_reg_C')
*
      character*(*) name
      character*(*) var
      logical any_FAIL
      character*(*) err
*
      INCLUDE 'gen_routines.dec'
*
      logical FAIL
      integer ierr,m
      character*256 sanitized
*
*--------------------------------------------------------
*
      sanitized= name           !copy name, truncate at 256 characters
      call squeeze(sanitized,m) !remove blanks,tabs,&nulls, get nonblank length
*
      ierr= regparmstring(sanitized(1:m),var,0)  !only give CTP nonblank length
*
      FAIL= ierr.NE.0
      IF(FAIL) call G_append(err,','//sanitized(1:m))
*
      any_FAIL= any_FAIL .OR. FAIL
*
      RETURN
      END
