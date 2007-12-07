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

      do i=1,bigcal_max_tdc
         bigcal_g8_time_offset(i) = bigcal_g8_time_offset(i) + 
     $        b_trig_offset 
      enddo

      do i=1,bigcal_ttrig_maxgroups
         bigcal_g64_time_offset(i) = bigcal_g64_time_offset(i) + 
     $        b_trig_offset 
      enddo
      

      write(*,*) 'bigcal central tof = ',bigcal_tof_central

      ABORT=.false.
      err = ' '

      return 
      end
