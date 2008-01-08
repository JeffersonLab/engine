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
      integer*4 i,j,Nred
      integer*4 irow,icol
      
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
c     count up the number of channels to calibrate for the reduced matrix:
c$$$         Nred=0
c$$$         if(bigcal_calib_iylo.ge.1.and.bigcal_calib_iylo.le.56.and.
c$$$     $        bigcal_calib_iyhi.ge.1.and.bigcal_calib_iyhi.le.56.and.
c$$$     $        bigcal_calib_iyhi.gt.bigcal_calib_iylo) then
c$$$            if(bigcal_calib_ixlo(1).ge.1.and.bigcal_calib_ixlo(1).le.32
c$$$     $           .and.bigcal_calib_ixhi(1).ge.1.and.bigcal_calib_ixhi(1)
c$$$     $           .le.32.and.bigcal_calib_ixhi(1).gt.bigcal_calib_ixlo(1)) then
c$$$               if(bigcal_calib_ixlo(2).ge.1.and.bigcal_calib_ixlo(2).le.30
c$$$     $              .and.bigcal_calib_ixhi(2).ge.1.and.bigcal_calib_ixhi(2)
c$$$     $              .le.30.and.bigcal_calib_ixhi(2).gt.bigcal_calib_ixlo(2)) then
c$$$                  Nred=0
c$$$                  
c$$$                  do i=1,bigcal_all_maxhits
c$$$                     if(i.le.1024) then
c$$$                        irow = i/32 + 1
c$$$                        icol = mod(i,32) + 1
c$$$                     else
c$$$                        j=i-1024
c$$$                        irow = j/30 + 33
c$$$                        icol = mod(j,30) + 1
c$$$                     endif
c$$$                     
c$$$                     if(irow.ge.bigcal_calib_iylo.and.irow.le.bigcal_calib_iyhi) then
c$$$                        if(irow.le.32) then
c$$$                           if(icol.ge.bigcal_calib_ixlo(1).and.icol.le.bigcal_calib_ixhi(1)) then
c$$$                              Nred = Nred + 1
c$$$                           endif
c$$$                        else
c$$$                           if(icol.ge.bigcal_calib_ixlo(2).and.icol.le.bigcal_calib_ixhi(2)) then
c$$$                              Nred = Nred + 1
c$$$                           endif
c$$$                        endif
c$$$                        
c$$$                     endif
c$$$                  enddo
c$$$               endif
c$$$            endif
c$$$         endif
c$$$         
c$$$         bigcal_Ncalib = Nred
c$$$
c$$$         write(*,*) 'Number of channels in reduced calibration matrix=',Nred
         
         if(b_calib_matrix_filename.ne.' ') then
            filename = b_calib_matrix_filename
         
            call g_IO_control(iochan,'ANY',ABORT,err)
            if(abort) then
               call g_add_path(here,err)
               return 
            endif
            
            bigcal_matr_iochan = iochan
            
            open(unit=iochan,file=filename,status='old',
     $           form='unformatted',err=34)
            
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
            
 34         write(*,*) filename//' does not exist'//
     $           ', initializing BigCal calib. matrix to zero'
            
c     if there is any trouble reading in the values, scrap and 
c     start over:
            
 35         bigcal_nmatr_event = 0
            do i=1,bigcal_all_maxhits
               bigcal_vector(i) = 0.
               do j=1,bigcal_all_maxhits
                  bigcal_matrix(i,j) = 0.
               enddo
            enddo  
c     shut down the file: 
 36         call g_IO_control(iochan,'FREE',abort,err)
            if(abort) then
               call g_add_path(here,err)
               return
            endif
            close(iochan)
         else 
            bigcal_nmatr_event = 0
            do i=1,bigcal_all_maxhits
               bigcal_vector(i) = 0.
               do j=1,bigcal_all_maxhits
                  bigcal_matrix(i,j) = 0.
               enddo
            enddo  
         endif
      endif
c     initialize debugging output file:
      if(bdebug_print_adc.ne.0 .or. bdebug_print_tdc.ne.0.or.bdebug_print_trig
     $     .ne.0.or.bdebug_print_bad.ne.0) then
         if(b_debug_output_filename.ne.' ') then
            filename = b_debug_output_filename
         else
            filename = 'scalers/rawbigcal_%d.txt'
         endif

         call g_sub_run_number(filename,gen_run_number)

         open(unit=bluno,file=filename,form='formatted',status='unknown')

      endif
c     initialize list of bad channels:
      if(b_use_bad_chan_list.ne.0 .and. b_bad_chan_list_filename
     $     .ne. ' ') then
         call b_init_bad_list(abort,err)
         if(abort) then
            call g_add_path(here,err)
            return 
         endif
      endif

      if(abort .or. err.ne.' ') call g_add_path(here,err)

      return 
      end
