      subroutine sane_ntup_register(ABORT,err)

      implicit none
      save
      
      character*17 here
      parameter(here='sane_ntuple_register')

      logical ABORT
      character*(*) err

      include 'b_ntuple.cmn'
      include 'sane_ntuple.cmn'
      include 'gen_routines.dec'

      integer ierr

      err=' '
      abort=.false.
      write(*,*)'SANE REGISTERING VARIABLES'
      call G_reg_C('sane_ntuple',sane_ntuple_file,ABORT,err)

      if(abort) then
         call G_prepend(':unable to register-',err)
         call G_add_path(here,err)
      endif

      return 
      end
