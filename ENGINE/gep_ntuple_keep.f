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
      include 'hms_geometry.cmn'

      integer m,iTrk,iSet
      real zclose_store,sclose_store
      real theta_store,phi_store,chi2_store
      integer conetest_store,track_store,nhits_store,nplanes_store
      integer ref_store
      real zanalyzer(2)
      
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
      gep_ntuple_contents(m) = GEP_Pel_btheta
      m=m+1
      gep_ntuple_contents(m) = GEP_Ebeam_two_body
      m=m+1
      gep_ntuple_contents(m) = GEP_delta_p
      m=m+1
      gep_ntuple_contents(m) = GEP_xfp_p
      m=m+1
      gep_ntuple_contents(m) = GEP_yfp_p
      m=m+1
      gep_ntuple_contents(m) = GEP_xpfp_p
      m=m+1
      gep_ntuple_contents(m) = GEP_ypfp_p
      m=m+1
      gep_ntuple_contents(m) = GEP_xptar_p
      m=m+1
      gep_ntuple_contents(m) = GEP_yptar_p
      m=m+1
      gep_ntuple_contents(m) = GEP_ytar_p
      m=m+1
      gep_ntuple_contents(m) = GEP_xbeam
      m=m+1
      gep_ntuple_contents(m) = GEP_ybeam
      m=m+1
      gep_ntuple_contents(m) = GEP_xclust
      m=m+1
      gep_ntuple_contents(m) = GEP_yclust
      m=m+1
      gep_ntuple_contents(m) = GEP_eclust
      m=m+1
      gep_ntuple_contents(m) = GEP_aclust
c     lw
      m=m+1
      gep_ntuple_contents(m) = GEP_xclust2
      m=m+1
      gep_ntuple_contents(m) = GEP_yclust2
      m=m+1
      gep_ntuple_contents(m) = GEP_eclust2
      m=m+1
      gep_ntuple_contents(m) = GEP_aclust2      
c     lw
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
      gep_ntuple_contents(m) = float(HFPP_N_tracks(1))
      m=m+1
      gep_ntuple_contents(m) = float(HFPP_N_tracks(2))
c      
c Algorithm to select "best" track for final analysis
c
      do iSet=1,2
         track_store=0
         nhits_store=0
         nplanes_store=0
         theta_store=1.0e15
         phi_store=1.0e15
         conetest_store=-1
         zclose_store=1.0e15
         sclose_store=1.0e15
         chi2_store=1.0e15

         if(hselectfpptrackprune.ne.0) then ! use best FPP track selection based on prune tests, Sitnik
            track_store = hfpp_best_track(iSet)
            if(track_store.gt.0) then
               nhits_store = hfpp_track_nhits(iSet,track_store)
               nplanes_store = hfpp_track_nlayers(iSet,track_store)
               theta_store = hfpp_track_theta(iSet,track_store)
               phi_store = hfpp_track_phi(iSet,track_store)
               conetest_store = hfpp_track_conetest(iSet,track_store)
               zclose_store = hfpp_track_zclose(iSet,track_store)
               sclose_store = hfpp_track_sclose(iSet,track_store)
               chi2_store = hfpp_track_chi2(iSet,track_store)
            endif
         else
            zanalyzer(1)=140.3
            zanalyzer(2)=237.8
c     
            do iTrk=1,HFPP_N_tracks(iSet)
               if(abs(HFPP_track_zclose(iSet,iTrk)-zanalyzer(iSet)).le.27.0) then
                  if(HFPP_track_sclose(iSet,iTrk).lt.8.0) then
                     if(HFPP_track_conetest(iSet,iTrk).eq.1) then
                        if(HFPP_track_theta(iSet,iTrk).gt.0.1/180.0*3.14159265.and.
     >                       HFPP_track_theta(iSet,iTrk).lt.theta_store) then
                           track_store=iTrk
                           theta_store=HFPP_track_theta(iSet,iTrk)
                           phi_store=HFPP_track_phi(iSet,iTrk)
                           conetest_store=HFPP_track_conetest(iSet,iTrk)
                           zclose_store=HFPP_track_zclose(iSet,iTrk)
                           sclose_store=HFPP_track_sclose(iSet,iTrk)
                           chi2_store=hfpp_track_chi2(iSet,iTrk)
                        endif
                     endif
                  endif  
               endif 
            enddo       
         endif
         m=m+1
         gep_ntuple_contents(m) = float(track_store)
         m=m+1     
         gep_ntuple_contents(m) = float(nhits_store)
         m=m+1
         gep_ntuple_contents(m) = float(nplanes_store)
         m=m+1
         gep_ntuple_contents(m) = zclose_store 
         m=m+1     
         gep_ntuple_contents(m) = sclose_store 
         m=m+1     
         gep_ntuple_contents(m) = float(conetest_store) 
         m=m+1     
         gep_ntuple_contents(m) = theta_store 
         m=m+1     
         gep_ntuple_contents(m) = phi_store
         m=m+1
         gep_ntuple_contents(m) = chi2_store
      enddo
c     initialize variables for FPP2 relative to FPP1
      track_store = 0
      ref_store = 0
      theta_store = 1.0e15
      phi_store = 1.0e15
      conetest_store = -1
      sclose_store = 1.0e15
      zclose_store = 1.0e15

      if(hfpp_best_track(1).gt.0.and.hfpp_best_track(2).gt.0) then
         track_store = hfpp_best_track(2)
         
         theta_store = hfpp_track_theta(3,track_store)
         phi_store = hfpp_track_phi(3,track_store)
         conetest_store = hfpp_track_conetest(3,track_store)
         sclose_store = hfpp_track_sclose(3,track_store)
         zclose_store = hfpp_track_zclose(3,track_store)
c     the following decision informs whether we should use theta,phi 
c     relative to HMS track or FPP1 track in final analysis: 
c     if ref_store>0 then we use FPP1 track, otherwise we use
c     HMS track as reference
         ref_store = hfpp2_best_reference(track_store)
      endif
      m=m+1
      gep_ntuple_contents(m) = float(ref_store)
      m=m+1
      gep_ntuple_contents(m) = zclose_store
      m=m+1
      gep_ntuple_contents(m) = sclose_store
      m=m+1
      gep_ntuple_contents(m) = float(conetest_store)
      m=m+1
      gep_ntuple_contents(m) = theta_store
      m=m+1
      gep_ntuple_contents(m) = phi_store


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
