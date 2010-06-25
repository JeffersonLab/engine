
      subroutine sane_keep_results(ABORT,err)

      implicit none
      save
      
      include 'bigcal_data_structures.cmn'
      include 'sane_data_structures.cmn'
      include 'b_ntuple.cmn'
      include 'sane_ntuple.cmn'
      include 'gen_event_info.cmn'
      include 'hms_data_structures.cmn'


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
c      write(*,*)nclust
      if (gen_event_type .eq. 4) return
      if(sane_ntuple_type.gt.0)then
        if((bigcal_all_nclstr.gt.0.or.gen_event_type .eq. 8
     ,        .or.HSNUM_FPTRACK.gt.0)
     ,        ) then
              call SANE_DUMP_NTUP_VAR()
              call b_ntuple_keep(ABORT,err,.false.)
              
              call sane_ntuple_keep(ABORT,err)
              call SANE_DUMP_NTUP_VAR()  
 
c          call  B_reset_event(ABORT,err)
          bigcal_all_nclstr=0
          bigcal_nmaxima=0
        endif
      endif
      
      if(abort) then
         call G_add_path(here,err)
      else
         err=' '
      endif

      return 
      end
