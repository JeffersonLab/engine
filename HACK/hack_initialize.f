* ----------------------------------------------------------------------
*--  file HACK_INITIALIZE.F 
*--  Initialization for User Develpment Code
* $Log: hack_initialize.f,v $
* Revision 1.1  1994/07/25 18:03:30  cdaq
* Initial revision
*
*
      subroutine hack_initialize(ABORT,err)
*
* ----------------------------------------------------------------------
*-- first declare variables and store them in a common block (extra file)
      implicit none
      logical ABORT
      character*(*) err
      include 'gen_data_structures.cmn'
      include 'hack_.cmn'
*
*EXAMP      integer i
*
*-- >>>>>>>>>> insert additional user declarations here <<<<<<<<<
* ----------------------------------------------------------------------
      ABORT = .FALSE.
      err = ' '
* ----------------------------------------------------------------------
* ----------------------------------------------------------------------
*--  intialize any varibles that need initialization
*EXAMP      do i = 1, max_user_par
*EXAMP        hack_int(i) = 0
*EXAMP        hack_real(i) = 0.0
*EXAMP      enddo
*
*-- >>>>>>>>>> insert additional user code here <<<<<<<<<
* ----------------------------------------------------------------------
*
      return
      end
