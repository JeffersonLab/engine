      subroutine gep_initialize(ABORT,err)

      implicit none
      save

      character*14 here
      parameter(here='gep_initialize')

      logical ABORT
      character*(*) err

      include 'gen_data_structures.cmn'
      include 'gen_constants.par'
      
      abort=.false.

      gebeam = sqrt(gpbeam**2 + mass_electron**2)
      if(gtarg_z(gtarg_num).gt.0.)then
        call total_eloss(0,.true.,0.0,1.0,geloss)
      else
        geloss=0.
      endif
      gebeam = gebeam - geloss
      gpbeam = sqrt(gebeam**2 - mass_electron**2)
      g_beam_target_s = (gtarg_mass(gtarg_num) + gebeam)**2 - gpbeam**2

      IF(ABORT) THEN
         call G_add_path(here,err)
      ELSE
         err= ' '
      ENDIF

      return 
      end
