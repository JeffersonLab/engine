      subroutine h_fpp_nt_shutdown(ABORT,err)
*----------------------------------------------------------------------
*
*     Final shutdown of the HMS FPP Ntuple
*
*     Purpose : Flushes and closes the HMS FPP Ntuple
*
*     Output: ABORT      - success or failure
*           : err        - reason for failure, if any
*
*----------------------------------------------------------------------
      implicit none
      save
*
      character*17 here
      parameter (here='h_fpp_nt_shutdown')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 'h_fpp_ntuple.cmn'
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

      IF(.NOT.h_fpp_nt_exists) RETURN       !nothing to do
c

      call h_fpp_nt_close(ABORT,err)

*
      IF(h_fpp_nt_exists) then
         ABORT = .true.
      endif
      h_fpp_nt_ID= 0
      h_fpp_nt_name= ' '
      h_fpp_nt_file= ' '
      h_fpp_nt_title= ' '
      do m=1,HMAX_FPPntuple_size
        h_fpp_nt_tag(m)= ' '
        h_fpp_nt_contents(m)= 0.
      enddo
*
      IF(ABORT) call G_add_path(here,err)
*
      RETURN
      END      
