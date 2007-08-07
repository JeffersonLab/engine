      subroutine gep_tree_init(abort,err)

      implicit none 
      save
      
      character*13 here
      parameter(here='gep_tree_init')

      include 'gep_filenames.cmn'
      include 'gen_routines.dec'
      include 'gep_data_structures.cmn'
      include 'gen_run_info.cmn'
c      include 'b_ntuple.dte'

      logical abort
      character*(*) err

c     only purpose of this routine is to substitute run number in
c     tree filename! CTP will take care of the rest!!!!!!!!!!!

      call no_nulls(gep_tree_filename)

      if(gep_tree_filename.eq.' ') return
      
      call g_sub_run_number(gep_tree_filename,gen_run_number)

      abort=.false.
      err=' '

      return
      end
