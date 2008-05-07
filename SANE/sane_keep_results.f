      subroutine sane_keep_results(ABORT,err)

      implicit none
      save
      
      include 'bigcal_data_structures.cmn'
      include 'sane_data_structures.cmn'
      include 'b_ntuple.cmn'
      include 'sane_ntuple.cmn'
      include 'gen_event_info.cmn'


      character*14 here
      parameter(here='sane_keep_results')

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

c      if (gen_event_type .le. 2) return
      if(sane_ntuple_type.eq.2)then
        if(bigcal_all_nclstr.gt.0.or.
     ,        LUCITE_SANE_RAW_TOT_HITS.GT.0.OR.
     ,        CERENKOV_SANE_RAW_TOT_HITS.GT.0.OR.
     ,        TRACKER_SANE_RAW_TOT_HITS_X.GT.0.OR.
     ,        TRACKER_SANE_RAW_TOT_HITS_Y.GT.0) then
          call sane_ntuple_keep(ABORT,err)
          call SANE_DUMP_NTUP_VAR()
          bigcal_all_nclstr=0
          bigcal_nmaxima=0
        endif
c      else if(sane_ntuple_type.eq.2)then 
c         call sane_ntuple_keep(ABORT,err)
      endif
      
      if(abort) then
         call G_add_path(here,err)
      else
         err=' '
      endif

      return 
      end
