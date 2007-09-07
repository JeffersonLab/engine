      subroutine gep_ntuple_keep(abort,err)

      implicit none
      save
      
      character*15 here
      parameter(here='gep_ntuple_keep')

      logical abort
      character*(*) err

      include 'gep_ntuple.cmn'
      include 'gep_data_structures.cmn'
      include 'gen_data_structures.cmn'
      include 'gen_event_info.cmn'

      integer m

      logical HEXIST ! cernlib function

      err=' '
      abort=.false.
      
      if(.not.gep_ntuple_exists) return

      if(gep_ntuple_max_segmentevents.gt.0) then
         if(gep_ntuple_segmentevents.gt.gep_ntuple_max_segmentevents) 
     $        then
            call gep_ntuple_change(abort,err)
            gep_ntuple_segmentevents=0
         else
            gep_ntuple_segmentevents = gep_ntuple_segmentevents + 1
         endif
      endif         

      m=0
      
      m=m+1
      gep_ntuple_contents(m) = float(gen_event_id_number)
      m=m+1
      gep_ntuple_contents(m) = GEP_ctime_hms
      m=m+1
      gep_ntuple_contents(m) = GEP_ctime_cal
      m=m+1
      gep_ntuple_contents(m) = GEP_Q2
      m=m+1
      gep_ntuple_contents(m) = GEP_Q2_H
      m=m+1
      gep_ntuple_contents(m) = GEP_Q2_B
      m=m+1
      gep_ntuple_contents(m) = GEP_E_electron
      m=m+1
      gep_ntuple_contents(m) = GEP_P_proton
      m=m+1
      gep_ntuple_contents(m) = GEP_delta_p
      m=m+1
      gep_ntuple_contents(m) = GEP_epsilon
      m=m+1
      gep_ntuple_contents(m) = GEP_etheta_deg
      m=m+1
      gep_ntuple_contents(m) = GEP_ephi_deg
      m=m+1
      gep_ntuple_contents(m) = GEP_ptheta_deg
      m=m+1
      gep_ntuple_contents(m) = GEP_pphi_deg
      m=m+1
      gep_ntuple_contents(m) = GEP_Emiss
      m=m+1
      gep_ntuple_contents(m) = GEP_Pmiss
      m=m+1
      gep_ntuple_contents(m) = GEP_Pmissx
      m=m+1
      gep_ntuple_contents(m) = GEP_Pmissy
      m=m+1
      gep_ntuple_contents(m) = GEP_Pmissz
      m=m+1
      gep_ntuple_contents(m) = GEP_W2
      m=m+1
      gep_ntuple_contents(m) = GEP_Mmiss
      
      abort = .not. HEXIST(gep_ntuple_ID)
      if(abort) then
         call G_build_note(':Ntuple ID#$ does not exist',
     $        '$',gep_ntuple_ID,' ',0.,' ',err)
         call G_add_path(here,err)
      else
c         call HFNT(gep_ntuple_ID)
         call HFN(gep_ntuple_id,gep_ntuple_contents)
      endif
      
      return 
      end
