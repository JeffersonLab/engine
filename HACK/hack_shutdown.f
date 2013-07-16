* ----------------------------------------------------------------
*-- file: hack_shutdown.f
*-- this subroutine does User Devolpment tasks after the
*     all events have been collected.
* $Log: hack_shutdown.f,v $
* Revision 1.1  1994/07/25 18:03:54  cdaq
* Initial revision
*
*
      subroutine hack_shutdown(ABORT, err)
      implicit none                     !needed
      include 'gen_data_structures.cmn' !needed
      include 'hack_.cmn'               !needed
      logical ABORT                     !needed
      character*(*) err                 !needed
*-----------------------------------------------------------------------------
      ABORT = .FALSE.                   !needed as default
      err = ' '                         !needed as default
*-----------------------------------------------------------------------------
*--  >>>>>>>>>>>>>> insert user code here <<<<<<<<<<<
**      call hack_f_c123(ABORT, err)  !get centroids etc.
**      call hack_f_c4(ABORT, err)  !make scint. pedestal centroid file
**      call hack_f_c5(ABORT, err)  !make calorim. pedestal centroid file
*-----------------------------------------------------------------------------
      return
      end
*
