      subroutine b_keep_results(ABORT,err)

      implicit none
      save
      
      include 'bigcal_data_structures.cmn'
      include 'b_ntuple.cmn'

      character*14 here
      parameter(here='b_keep_results')

      logical abort
      character*(*) err

      abort=.false.
      err=' '

c     check whether there is a cluster. 
c     if there is a cluster, then keep the ntuple

c$$$      if(BIGCAL_PROT_NCLSTR.gt.0.or.BIGCAL_RCS_NCLSTR.gt.0.or.
c$$$     $     BIGCAL_MID_NCLSTR.gt.0) then
c$$$         call b_ntuple_keep(ABORT,err)
c$$$      endif

      if(bigcal_ntuple_type.le.2)then
        if(bigcal_prot_nclstr.gt.0.or.bigcal_rcs_nclstr.gt.0.or.
     $       bigcal_mid_nclstr.gt.0) then
          call b_ntuple_keep(ABORT,err)
        endif
      else if(bigcal_ntuple_type.eq.3)then 
        call b_ntuple_keep(ABORT,err)
      endif
      
      if(abort) then
         call G_add_path(here,err)
      else
         err=' '
      endif

      return 
      end
