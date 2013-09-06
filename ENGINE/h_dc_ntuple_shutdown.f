      subroutine h_dc_Ntuple_shutdown(ABORT,err)
*----------------------------------------------------------------------
*
*
*
*----------------------------------------------------------------------
      implicit none
      save
*
      character*17 here
      parameter (here='h_dc_Ntuple_shutdown')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 'h_dc_ntuple.cmn'
      INCLUDE 'gen_routines.dec'
*
*
      logical FAIL
      character*80 why,directory,name
      character*1000 msg
      integer io,id,cycle,m
*
*--------------------------------------------------------
      err= ' '
      ABORT = .FALSE.
*

      IF(.NOT.h_dc_Ntuple_exists) RETURN       !nothing to do
c

      call h_dc_ntuple_close(ABORT,err)

*
      IF(h_dc_Ntuple_exists) then
         ABORT = .true.
      endif
      h_dc_Ntuple_ID= 0
      h_dc_Ntuple_name= ' '
      h_dc_Ntuple_file= ' '
      h_dc_Ntuple_title= ' '
      h_dc_Ntuple_size= 0
*
      IF(ABORT) call G_add_path(here,err)
*
      RETURN
      END      
