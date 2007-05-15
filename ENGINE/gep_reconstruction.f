      subroutine gep_reconstruction(ABORT,err)
      
      implicit none
      save

      character*18 here
      parameter(here='gep_reconstruction')

      logical abort
      character*(*) err

      include 'gep_data_structures.cmn'
      include 'gen_constants.par'
      include 'gen_units.par'
c      include 'gep_bypass_switches.cmn'

      abort=.false.
      err=' '

c      if(gepbypass_physics.eq.0) then
      call gep_physics(abort,err)
      if(abort) then 
         call G_add_path(here,err)
         return
      endif
      
c     successful return

      abort = .false.
      
      return 
      end
