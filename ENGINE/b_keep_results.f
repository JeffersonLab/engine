      subroutine b_keep_results(ABORT,err)

      implicit none
      save
      
      include 'bigcal_data_structures.cmn'

      character*14 here
      parameter(here='b_keep_results')

      logical abort
      character*(*) err

      abort=.false.
      err=' '

c     check whether there is a cluster. 
c     if there is a cluster, then keep the ntuple

      if(BIGCAL_PROT_NCLSTR.gt.0.or.BIGCAL_RCS_NCLSTR.gt.0.or.
     $     BIGCAL_MID_NCLSTR.gt.0) then
         call b_ntuple_keep(ABORT,err)
      endif
         
      if(abort) then
         call G_add_path(here,err)
      else
         err=' '
      endif

      return 
      end
