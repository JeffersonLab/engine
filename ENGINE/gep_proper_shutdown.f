      subroutine gep_proper_shutdown(lunout,ABORT,err)

      implicit none
      save

      character*19 here
      parameter(here='gep_proper_shutdown')

      include 'gen_routines.dec'
      include 'gen_filenames.cmn'
      include 'gen_run_info.cmn'
      include 'gep_data_structures.cmn'
      include 'gep_filenames.cmn'
      
      logical abort, report_abort
      character*(*) err
      
      integer lunout
      integer ierr
      character*132 file
      
      abort=.false.
      err=' '

c      call gep_report_bad_data(lunout,ABORT,err)

      if(gep_report_blockname.ne.' '.and.
     $     gep_report_output_filename.ne.' ') then
         file = gep_report_output_filename
         call g_sub_run_number(file,gen_run_number)
         ierr = threp(gep_report_blockname,file)
         if(ierr.ne.0) then
            call g_append(err,'& threp failed to create report in file'
     $           //file)
            report_abort = .true.
         endif
      endif

      if(abort.or.report_abort) then
         call G_add_path(here,err)
      else
         err=' '
      endif

      return
      end
