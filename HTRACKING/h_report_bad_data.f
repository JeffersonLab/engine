      SUBROUTINE H_REPORT_BAD_DATA(lunout,ABORT,errmsg)

*--------------------------------------------------------
*
*   Purpose and Methods: Output warnings for possible hardware problems
*          in file 'bad<runnum>.txt' (unit=lunout)
*
*  Required Input BANKS: 
*
*                Output: ABORT           - success or failure
*                      : err             - reason for failure, if any
* 
* author: John Arrington
* created: 8/17/95
* $Log$
* Revision 1.1  1995/08/31 14:44:52  cdaq
* Initial revision
*
*--------------------------------------------------------

      IMPLICIT NONE
*
      character*17 here
      parameter (here= 'H_REPORT_BAD_DATA')
*
      logical ABORT
      character*(*) errmsg
*
      INCLUDE 'hms_data_structures.cmn'

      integer*4 lunout

      save

!	NOTHING YET, BUT CAN ADD CHECKS/WARNINGS HERE.  NOTE, NOTHING
!	SHOULD BE WRITTEN TO THE FILE UNLESS THERE IS A WARNING TO BE OUTPUT.

      return
      end
