      subroutine gep_ntuple_register(ABORT,err)

      implicit none
      save
      
      character*19 here
      parameter(here='gep_ntuple_register')

      logical abort
      character*(*) err

      include 'gep_ntuple.cmn'
      include 'gen_routines.dec'
      
      err=' '
      abort=.false.

      call G_reg_C('GEp_Ntuple',gep_ntuple_file,abort,err)

      if(abort) then
         call G_prepend(':unable to register-',err)
         call G_add_path(here,err)
      endif

      return
      end
