      subroutine b_proper_shutdown(lunout,ABORT,err)

      implicit none
      save

      character*17 here
      parameter(here='b_proper_shutdown')

      include 'gen_routines.dec'
      include 'gen_filenames.cmn'
      include 'gen_run_info.cmn'
      include 'bigcal_data_structures.cmn'
      include 'bigcal_filenames.cmn'
      include 'bigcal_bypass_switches.cmn'
      include 'bigcal_hist_id.cmn'

      logical abort, report_abort
      character*(*) err

      integer lunout
      integer ierr
      character*132 file

      integer irow,icol,icell
      real Eavg,eff

      abort=.false.
      err=' '

      call b_report_bad_data(lunout,ABORT,err)

      if(b_report_blockname.ne.' '.and.
     $     b_report_output_filename.ne.' ') then
         file = b_report_output_filename
         call g_sub_run_number(file,gen_run_number)
         ierr = threp(b_report_blockname,file)
         if(ierr.ne.0) then
            call g_append(err,'& threp failed to create report in file'
     $           //file)
            report_abort=.true.
         endif
      endif

      if(bdebug_print_adc.ne.0 .or. bdebug_print_tdc.ne.0.or.bdebug_print_trig
     $     .ne.0.or.bdebug_print_bad.ne.0) then
         close(bluno)
      endif

      if(ABORT.or.report_abort) then
         call G_add_path(here,err)
      else
         err=' '
      endif
      
      return 
      end
