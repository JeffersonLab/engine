      subroutine h_tree_init(abort,err)

      implicit none
      save

      character*11 here
      parameter(here='h_tree_init')

      include 'hms_filenames.cmn'
      include 'gen_routines.dec'
      include 'hms_data_structures.cmn'
      include 'gen_run_info.cmn'

      logical abort
      character*(*) err

c     only purpose of this routine is to substitute run number in
c     tree filename! CTP will take care of the rest!!!!!!!!!!!

      call no_nulls(h_tree_filename)

      if(h_tree_filename.eq.' ') return
      
      call g_sub_run_number(h_tree_filename,gen_run_number)

      abort=.false.
      err=' '

      return
      end
