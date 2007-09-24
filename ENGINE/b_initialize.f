      subroutine b_initialize(ABORT,err)

      implicit none
      save

      character*12 here
      parameter(here='b_initialize')
      
      logical ABORT
      character*(*) err
      character*20 mss
      integer*4 istat

      logical fail
      character*1000 why
      character*80 filename
      integer*4 iochan
      integer*4 i,j
      
      include 'gen_run_info.cmn'
      include 'bigcal_data_structures.cmn'
      include 'bigcal_filenames.cmn'
      include 'bigcal_bypass_switches.cmn'

      err = ' '
      abort = .false.

c     "calculate physics singles constants"
      call b_init_physics(FAIL,why)
      if(err.ne.' '.and.why.ne.' ') then
         call G_append(err,' & '//why)
      else if(why.ne.' ') then
         err = why
      endif
      abort = abort .or. fail

      call b_generate_geometry ! initialize geometry constants

      call b_init_tof(FAIL,why) ! initialize timing constants
      if(why.ne.' '.and.err.ne.' ') then
         call G_append(err,' & '//why)
      else if(why.ne.' ') then
         err = why
      endif
      abort=abort.or.fail

      call b_init_gain(FAIL,why) ! initialize calibration constants
      if(why.ne.' '.and.err.ne.' ') then
         call G_append(err,' & '//why)
      else if(why.ne.' ') then
         err = why
      endif
      abort=abort.or.fail

      call b_init_shower(FAIL,why) ! initialize shower reconstruction parms
      if(why.ne.' '.and.err.ne.' ') then
         call G_append(err,' & '//why)
      else if(why.ne.' ') then
         err = why
      endif
      abort=abort.or.fail

c     initialize calibration stuff. Always try to read an old matrix. If 
c     b_calib_matrix_filename is given, use that file. Otherwise, use a 
c     default filename of 'outfiles/bigcal_calib'
      
      if(bigcal_do_calibration.ne.0.and. .not. abort) then 
         if(b_calib_matrix_filename.ne.' ') then
            filename = b_calib_matrix_filename
         else 
            b_calib_matrix_filename = 'outfiles/bigcal_calib'
            filename = b_calib_matrix_filename
         endif
         call g_IO_control(iochan,'ANY',ABORT,err)
         if(abort) then
            call g_add_path(here,err)
            return 
         endif
         
         bigcal_matr_iochan = iochan
         
         open(unit=iochan,file=filename,status='old',
     $        form='unformatted',err=34)
         
c     successful open: read BigCal matrix and number of events:
         read(iochan,end=35,err=35) bigcal_nmatr_event
         read(iochan,end=35,err=35) bigcal_vector
         read(iochan,end=35,err=35) bigcal_matrix
         
         write(*,*) 'read old BigCal calibration matrix from '//filename
         write(*,*) 'Number of events in matrix = ',bigcal_nmatr_event
         
c     any read problems will trigger a reset of the calibration 
c     quantities to zero. 
c     a successful read means the calib. matrix quantities are 
c     successfully initialized to their "old" values.
         
         goto 36
         
 34      write(*,*) filename//' does not exist'//
     $        ', initializing BigCal calib. matrix to zero'
         
c     if there is any trouble reading in the values, scrap and 
c     start over:
         
 35      bigcal_nmatr_event = 0
         do i=1,bigcal_all_maxhits
            bigcal_vector(i) = 0.
            do j=1,bigcal_all_maxhits
               bigcal_matrix(i,j) = 0.
            enddo
         enddo  
c     shut down the file: 
 36      call g_IO_control(iochan,'FREE',abort,err)
         if(abort) then
            call g_add_path(here,err)
            return
         endif
         close(iochan)
      endif
c     initialize debugging output file:
      if(bdebug_print_adc.ne.0 .or. bdebug_print_tdc.ne.0.or.bdebug_print_trig
     $     .ne.0) then
         if(b_debug_output_filename.ne.' ') then
            filename = b_debug_output_filename
         else
            filename = 'scalers/rawbigcal_%d.txt'
         endif

         call g_sub_run_number(filename,gen_run_number)

         open(unit=bluno,file=filename,form='formatted',status='unknown')

      endif

      if(abort .or. err.ne.' ') call g_add_path(here,err)

      return 
      end
