      subroutine t_analyze_pedestal(ABORT,err)
*
*     Refer to s_analyze_pedestal or h_analyze_pedestal for examples
*     of what to put here.
*
* $Log$
* Revision 1.1  1998/12/01 20:56:24  saw
* Initial revision
*
*
      implicit none
      save
*
      character*18 here
      parameter (here='t_analyze_pedestal')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 't20_data_structures.cmn'
      INCLUDE 't20_pedestals.cmn'
 
      return
      end
