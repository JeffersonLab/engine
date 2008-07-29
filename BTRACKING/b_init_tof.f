      subroutine b_init_tof(ABORT,err)

      implicit none
      save
      
      character*10 here
      parameter(here='b_init_tof')

      logical ABORT
      character*(*) err
      
      real Eprime_central,M,me,gamma,c,ebeam
      
      double precision beta

      integer*4 i

      parameter(M=.938272)
      parameter(me=.511e-3)
      parameter(c=30.)
      
      include 'bigcal_data_structures.cmn'
      include 'bigcal_tof_parms.cmn'
      include 'bigcal_bypass_switches.cmn'
      include 'gep_data_structures.cmn'
      include 'gen_data_structures.cmn'
c      include 'bigcal_geometry.cmn'
      
c     do nothing for now. Hopefully all variables are correctly read in from CTP parm
c     files
c     set sensible values for timing parameters if they have not been set yet: 
c     just make sure that bigcal_end_time is bigger than bigcal_window_center!

      if(bigcal_end_time.le.bigcal_window_center) then
         bigcal_end_time = bigcal_window_center + 500.
      endif
      
      if(gebeam .eq. 0.) then
         ebeam = gpbeam
      else
         ebeam = gebeam
      endif

      Eprime_central = gpbeam / (1. + gpbeam/M * (1. - bigcal_costheta) )

      write(*,*) 'BigCal central Eprime =',Eprime_central

      gamma = Eprime_central / me

      beta = sqrt(max(0.,1. - 1./gamma**2))

      bigcal_tof_central = bigcal_r_tgt / (beta*c) ! in ns

      if(bigcal_do_time_calib.ne.0) then ! open up timing cuts to avoid throwing away good hits
         bigcal_window_slop = 500.
         bigcal_end_time = 1000. ! always use same value for calibration
         bigcal_window_center = 185. ! always use same value for calibration purposes.
         b_timing_cut = 500. ! widen up cut
         gep_sigma_tdiff = 1000.
         gep_bcalib_cut_ctime = 1000.
      endif

      do i=1,bigcal_max_tdc
         if(bigcal_do_time_calib.ne.0) then ! zero hit time correction parameters when dumping for calibration
            bigcal_g8_time_offset(i) = 0.
            bigcal_g8_phc_coeff(i) = 0.
c            bigcal_g8_phc_p0(i) = 0.
c            bigcal_g8_phc_p1(i) = 0.
c            bigcal_g8_phc_p2(i) = 0.
c            bigcal_g8_phc_p3(i) = 0.
         endif
      enddo

      do i=1,bigcal_ttrig_maxgroups
         if(bigcal_do_time_calib.ne.0) then ! zero hit time correction parameters
            bigcal_g64_time_offset(i) = 0.
c            bigcal_g64_phc_p0(i) = 0.
c            bigcal_g64_phc_p1(i) = 0.
c            bigcal_g64_phc_p2(i) = 0.
c            bigcal_g64_phc_p3(i) = 0.
         endif
      enddo
      

      write(*,*) 'bigcal central tof = ',bigcal_tof_central

      ABORT=.false.
      err = ' '

      return 
      end
