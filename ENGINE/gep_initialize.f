      subroutine gep_initialize(ABORT,err)

      implicit none
      save

      character*14 here
      parameter(here='gep_initialize')

      logical ABORT
      character*(*) err

      include 'hms_data_structures.cmn'
      include 'bigcal_data_structures.cmn'
      include 'gen_data_structures.cmn'
      include 'gen_constants.par'
      include 'gep_data_structures.cmn'
      
      real*4 M
      parameter(M=.938272)

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

      gep_Q2_central = 2. * M * (sqrt(hpcentral**2 + M**2) - M)
      gep_Ee_central = gebeam / (1. + gebeam / M * (1. - bigcal_costheta) )

c     initialize coincidence timing window parameters if the user hasn't defined something reasonable:
      if(gep_h1time_slop.lt.10.or.gep_h1time_slop.gt.1000.) then
         gep_h1time_slop=100.
      endif
      if(gep_h2time_slop.lt.10..or.gep_h2time_slop.gt.1000.) then
         gep_h2time_slop=100.
      endif

      if(gep_btime_slop.lt.10.or.gep_btime_slop.gt.1000.) then
         gep_btime_slop=100.
      endif

      if(gep_htrig_t0(1).le.gep_htrig_cut(1).or.gep_htrig_t0(1).ge.
     $     gep_htrig_cut(2)) then
         gep_htrig_t0(1) = .5 * (gep_htrig_cut(1) + gep_htrig_cut(2))
      endif
      
      if(gep_htrig_t0(2).le.gep_htrig_cut(3).or.gep_htrig_t0(2).ge.
     $     gep_htrig_cut(4)) then
         gep_htrig_t0(2) = .5 * (gep_htrig_cut(3) + gep_htrig_cut(4))
      endif

      IF(ABORT) THEN
         call G_add_path(here,err)
      ELSE
         err= ' '
      ENDIF

      return 
      end
