      subroutine gep_fill_hist(abort,err)
      
      implicit none
      save
      
      character*13 here
      parameter(here='gep_fill_hist')
      
      logical abort
      character*(*) err
      
      include 'gen_event_info.cmn'
      include 'gep_data_structures.cmn'
      include 'gep_hist_id.cmn'
      include 'hms_data_structures.cmn'
      include 'bigcal_data_structures.cmn'

      real xdiff,ydiff,ediff
      
      abort = .false.
      err= ' '

      if(gepid_gep_coin1_times.gt.0.and.ntrigH1.gt.0.and.gen_event_trigtype(4)
     $     .eq.1.and.ntrigB.gt.0) then
         call hf2(gepid_gep_coin1_times,GEP_Btime(1),GEP_H1time(1),1.)
      endif
      
      if(gepid_gep_coin2_times.gt.0.and.ntrigH2.gt.0.and.gen_event_trigtype(5)
     $     .eq.1.and.ntrigB.gt.0) then
         call hf2(gepid_gep_coin2_times,GEP_Btime(1),GEP_H2time(1),1.)
      endif

      if(gepid_gep_ntrig_h1.gt.0) call hf1(gepid_gep_ntrig_h1,float(ntrigH1),1.)
      if(gepid_gep_ntrig_h2.gt.0) call hf1(gepid_gep_ntrig_h2,float(ntrigH2),1.)
      if(gepid_gep_ntrig_bigcal.gt.0) call hf1(gepid_gep_ntrig_bigcal,float(ntrigB),1.)
      
      if(abs(hsdelta).lt.10..and.abs(hsp-gep_pel_htheta)/hpcentral .lt..1.and.
     $     bigcal_itrack_best.gt.0) then
         if(gepid_hgep_delta.gt.0) call hf1(gepid_hgep_delta,hsdelta,1.)
         if(gepid_hgep_q2_hms.gt.0) call hf1(gepid_hgep_q2_hms,gep_q2_h,1.)
         if(gepid_hgep_q2_cal.gt.0) call hf1(gepid_hgep_q2_cal,gep_q2_b,1.)
         if(gepid_hgep_q2.gt.0) call hf1(gepid_hgep_q2,gep_q2,1.)
         if(gepid_hgep_ecal.gt.0) call hf1(gepid_hgep_ecal,bigcal_energy,1.)
         if(gepid_hgep_pp.gt.0) call hf1(gepid_hgep_pp,hsp,1.)
         if(gepid_hgep_epsilon.gt.0) call hf1(gepid_hgep_epsilon,gep_epsilon,1.)
         if(gepid_hgep_etheta.gt.0) call hf1(gepid_hgep_etheta,gep_etheta_deg,1.)
         if(gepid_hgep_ephi.gt.0) call hf1(gepid_hgep_ephi,gep_ephi_deg,1.)
         if(gepid_hgep_ptheta.gt.0) call hf1(gepid_hgep_ptheta,gep_ptheta_deg,1.)
         if(gepid_hgep_pphi.gt.0) call hf1(gepid_hgep_pphi,gep_pphi_deg,1.)
         if(gepid_hgep_emiss.gt.0) call hf1(gepid_hgep_emiss,gep_emiss,1.)
         if(gepid_hgep_pmissx.gt.0) call hf1(gepid_hgep_pmissx,gep_pmissx,1.)
         if(gepid_hgep_pmissy.gt.0) call hf1(gepid_hgep_pmissy,gep_pmissy,1.)
         if(gepid_hgep_pmissz.gt.0) call hf1(gepid_hgep_pmissz,gep_pmissz,1.)
         xdiff = bigcal_all_clstr_x(bigcal_itrack_best) - gep_bx_expect_h
         ydiff = bigcal_all_clstr_y(bigcal_itrack_best) - gep_by_expect_h
         ediff = gep_e_electron - bigcal_energy
         if(gepid_hgep_xdiff.gt.0) call hf1(gepid_hgep_xdiff,xdiff,1.)
         if(gepid_hgep_ydiff.gt.0) call hf1(gepid_hgep_ydiff,ydiff,1.)
         if(gepid_hgep_xydiff.gt.0) call hf2(gepid_hgep_xydiff,xdiff,ydiff,1.)
         if(gepid_hgep_ediff.gt.0) call hf1(gepid_hgep_ediff,ediff,1.)
         if(gepid_hgep_dpel.gt.0) call hf1(gepid_hgep_dpel,
     $        (hsp-gep_pel_htheta)/hpcentral*100.,1.)
      endif
      
      
      return 
      end

      
