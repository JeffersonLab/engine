      subroutine t_calc_pedestal(ABORT,err)
*
*     See s_calc_pedestal or h_calc_pedestal for examples of what to put here
*
*
* $Log$
* Revision 1.1  1998/12/01 20:56:28  saw
* Initial revision
*
*
      implicit none
      save
*
      character*18 here
      parameter (here='t_calc_pedestal')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 't20_data_structures.cmn'
      INCLUDE 't20_pedestals.cmn'
      INCLUDE 't20_filenames.cmn'
      INCLUDE 'gen_run_info.cmn'
*
      integer SPAREID
      parameter (SPAREID=67)
*

      return
      end
