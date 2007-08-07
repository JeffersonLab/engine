      subroutine b_tree_init(abort,err)

      implicit none 
      save
      
      character*11 here
      parameter(here='b_tree_init')

      include 'bigcal_filenames.cmn'
      include 'gen_routines.dec'
      include 'bigcal_data_structures.cmn'
      include 'gen_run_info.cmn'
c      include 'b_ntuple.dte'

      logical abort
      character*(*) err

c     only purpose of this routine is to substitute run number in
c     tree filename! CTP will take care of the rest!!!!!!!!!!!

      call no_nulls(b_tree_filename)

      if(b_tree_filename.eq.' ') return
      
      call g_sub_run_number(b_tree_filename,gen_run_number)

      abort=.false.
      err=' '

      return
      end
