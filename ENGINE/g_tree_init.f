      subroutine g_tree_init(abort,err)

      implicit none
      save

      character*11 here
      parameter(here='g_tree_init')

      include 'gen_filenames.cmn'
      include 'hms_filenames.cmn'
      include 'gep_filenames.cmn'
      include 'bigcal_filenames.cmn'
      include 'gen_run_info.cmn'

      logical abort
      character*(*) err

      abort=.false.
      err=' '

      call h_tree_init(abort,err)
      if(abort) then
         call g_add_path(here,err)
         return 
      endif

      call b_tree_init(abort,err)
      if(abort) then
         call g_add_path(here,err)
         return 
      endif
      
      call gep_tree_init(abort,err)
      if(abort) then
         call g_add_path(here,err)
         return 
      endif

      return 
      end
