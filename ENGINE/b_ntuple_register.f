      subroutine b_ntuple_register(ABORT,err)

      implicit none
      save
      
      character*17 here
      parameter(here='b_ntuple_register')

      logical ABORT
      character*(*) err

      include 'b_ntuple.cmn'
      include 'gen_routines.dec'

      integer ierr

      err=' '
      abort=.false.
      
      call G_reg_C('BigCal_Ntuple',b_ntuple_file,ABORT,err)

      if(abort) then
         call G_prepend(':unable to register-',err)
         call G_add_path(here,err)
      endif

      return 
      end
