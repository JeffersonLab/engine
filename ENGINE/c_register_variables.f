      subroutine c_register_variables(ABORT,err)
*----------------------------------------------------------------------
*
*     CTP variable registration routine for coincidences
*
*     Purpose : Register all variables that are to be used by CTP, that are
*     connected with the coincidence calculations.  This includes
*     externally configured parameters/contants, event data that can be a
*     histogram source, and possible test results and scalers.
*
*     Output: ABORT      - success or failure
*           : err        - reason for failure, if any
*
*     Created: 9-Feb-1994  Stephen A. Wood
*     $Log$
*     Revision 1.1  1994/02/11 18:32:18  cdaq
*     Initial revision
*
*
*----------------------------------------------------------------------
      implicit none
      save
*
      character*20 here
      parameter (here='c_register_variables')
*
      logical ABORT
      character*(*) err
*
      ABORT = .false.
*
      return
      end
