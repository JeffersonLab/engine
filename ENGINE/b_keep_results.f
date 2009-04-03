      subroutine b_keep_results(ABORT,err)

      implicit none
      save
      
      include 'bigcal_data_structures.cmn'
      include 'b_ntuple.cmn'
      include 'gen_event_info.cmn'
      include 'sane_ntuple.cmn'
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
c$$$         call b_ntuple_keep(ABORT,err,.true.)
c$$$      endif

c      if (gen_event_type .le. 2) return

      if(bigcal_ntuple_type.eq.1)then
       if(bigcal_all_nclstr.gt.0!.or.bigcal_nmaxima.gt.0
     &        ) then
          call b_ntuple_keep(ABORT,err,.true.)
        endif
      else if(bigcal_ntuple_type.eq.2)then 
         call b_ntuple_keep(ABORT,err,.true.)
      endif

      if(abort) then
         call G_add_path(here,err)
      else
         err=' '
      endif

      return 
      end
