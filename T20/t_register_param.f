      SUBROUTINE t_register_param(ABORT,err)
*--------------------------------------------------------
*-
*-   Purpose and Methods : Initializes T20 quantities 
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
*-   Created  22-Jan-1997  Stephen A. Wood
*-
*- All standards are from "Proposal for Hall C Analysis Software
*- Vade Mecum, Draft 1.0" by D.F.Geesamn and S.Wood, 7 May 1993
* $Log$
* Revision 1.1  1998/12/01 20:57:13  saw
* Initial revision
*
*-
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*16 here
      parameter (here= 't_register_param')
*
      logical ABORT
      character*(*) err
*
*--------------------------------------------------------
      err= ' '
      ABORT = .false.
*
*     Register t20 parameters
*
      call r_t20_tracking
      call r_t20_test_histid
*
*     register bypass switches
*
      call r_t20_bypass_switches

*
*     register sos statistics
*

c      call r_sos_statistics
c      call r_sos_pedestals
*
      return
      end
