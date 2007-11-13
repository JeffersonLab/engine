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
      include 'gen_detectorids.par'
      include 'gen_decode_common.cmn'
      include 'hms_data_structures.cmn'
      include 'hms_fpp_event.cmn'

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
      if(gen_event_trigtype(4).eq.1.and.gen_event_trigtype(5).eq.0) then
         gep_ntuple_contents(m) = 1.
      else if(gen_event_trigtype(5).eq.1.and.gen_event_trigtype(4).eq.0) then
         gep_ntuple_contents(m) = 2.
      else 
         gep_ntuple_contents(m) = 3.
      endif
      m=m+1
      gep_ntuple_contents(m) = GEP_ctime_hms
      m=m+1
      gep_ntuple_contents(m) = GEP_ctime_cal
      m=m+1
      gep_ntuple_contents(m) = float(ntrigH1)
      m=m+1 
      gep_ntuple_contents(m) = float(ntrigH2)
      m=m+1
      gep_ntuple_contents(m) = float(ntrigB)
      m=m+1
      if(ntrigH1.gt.0.and.gep_ntuple_contents(2).ne.2.) then
         gep_ntuple_contents(m) = GEP_H1time(1) !H1 trig time in ns (first hit only)
      else
         gep_ntuple_contents(m) = 0.
      endif
      m=m+1
      if(ntrigH2.gt.0.and.gep_ntuple_contents(2).ne.1.) then
         gep_ntuple_contents(m) = GEP_H2time(1) !H2 trig time in ns (first hit only)
      else 
         gep_ntuple_contents(m) = 0.
      endif
      m=m+1 
      if(ntrigB.gt.0) then
         gep_ntuple_contents(m) = GEP_Btime(1) !Bigcal trig time in ns (first hit )
      else
         gep_ntuple_contents(m) = 0.
      endif
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
      gep_ntuple_contents(m) = GEP_Pel_htheta
      m=m+1
      gep_ntuple_contents(m) = GEP_delta_p
      m=m+1
      gep_ntuple_contents(m) = GEP_xptar_p
      m=m+1
      gep_ntuple_contents(m) = GEP_yptar_p
      m=m+1
      gep_ntuple_contents(m) = GEP_ytar_p
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
      m=m+1
      gep_ntuple_contents(m) = gbeam_helicity
      m=m+1
      gep_ntuple_contents(m) = HFPP_N_tracks(1)
      m=m+1
      gep_ntuple_contents(m) = HFPP_N_tracks(2)
      if(HFPP_N_tracks(1).eq.1) then
        m=m+1     
        gep_ntuple_contents(m) = HFPP_track_zclose(1,1) 
        m=m+1     
        gep_ntuple_contents(m) = HFPP_track_sclose(1,1) 
        m=m+1     
        gep_ntuple_contents(m) = HFPP_track_conetest(1,1) 
        m=m+1     
        gep_ntuple_contents(m) = HFPP_track_theta(1,1) 
        m=m+1     
        gep_ntuple_contents(m) = HFPP_track_phi(1,1)
      else
        m=m+1     
        gep_ntuple_contents(m) = 1.0e15 
        m=m+1     
        gep_ntuple_contents(m) = 1.0e15 
        m=m+1     
        gep_ntuple_contents(m) = 1.0e15 
        m=m+1     
        gep_ntuple_contents(m) = 1.0e15 
        m=m+1     
        gep_ntuple_contents(m) = 1.0e15
      endif
      if(HFPP_N_tracks(2).eq.1) then
        m=m+1     
        gep_ntuple_contents(m) = HFPP_track_zclose(2,1) 
        m=m+1     
        gep_ntuple_contents(m) = HFPP_track_sclose(2,1) 
        m=m+1     
        gep_ntuple_contents(m) = HFPP_track_conetest(2,1) 
        m=m+1     
        gep_ntuple_contents(m) = HFPP_track_theta(2,1) 
        m=m+1     
        gep_ntuple_contents(m) = HFPP_track_phi(2,1)
      else
        m=m+1     
        gep_ntuple_contents(m) = 1.0e15 
        m=m+1     
        gep_ntuple_contents(m) = 1.0e15 
        m=m+1     
        gep_ntuple_contents(m) = 1.0e15 
        m=m+1     
        gep_ntuple_contents(m) = 1.0e15 
        m=m+1     
        gep_ntuple_contents(m) = 1.0e15
      endif

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
