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
      include 'bigcal_bypass_switches.cmn'
c      include 'gep_bypass_switches.cmn'

      abort=.false.
      err=' '

c      if(gepbypass_physics.eq.0) then

      call gep_physics(abort,err)
      if(abort) then 
         call G_add_path(here,err)
         return
      endif
      
      if(bigcal_do_calibration.ne.0) then
         call b_matrix_accum(abort,err)
         if(abort) then
            call g_add_path(here,err)
            return
         endif
      endif

c     successful return

      abort = .false.
      
      return 
      end
