      subroutine gep_initialize(ABORT,err)

      implicit none
      save

      character*14 here
      parameter(here='gep_initialize')

      logical ABORT
      character*(*) err

      include 'gen_data_structures.cmn'
      include 'gen_constants.par'
      include 'gep_data_structures.cmn'
      
      abort=.false.

      gebeam = sqrt(gpbeam**2 + mass_electron**2)
      if(gtarg_z(gtarg_num).gt.0.)then
        call total_eloss(0,.true.,0.0,1.0,geloss)
      else
        geloss=0.
      endif
c      gebeam = gebeam - geloss ! for SANE no energy loss correction
      gpbeam = sqrt(gebeam**2 - mass_electron**2)
      g_beam_target_s = (gtarg_mass(gtarg_num) + gebeam)**2 - gpbeam**2

c     initialize coincidence timing window parameters if the user hasn't defined something reasonable:
      if(gep_h1time_slop.lt.10.or.gep_h1time_slop.gt.1000.) then
         gep_h1time_slop=30.
      endif
      if(gep_h2time_slop.lt.10..or.gep_h2time_slop.gt.1000.) then
         gep_h2time_slop=30.
      endif

      if(gep_btime_slop.lt.10.or.gep_btime_slop.gt.1000.) then
         gep_btime_slop=30.
      endif

      IF(ABORT) THEN
         call G_add_path(here,err)
      ELSE
         err= ' '
      ENDIF

      return 
      end
