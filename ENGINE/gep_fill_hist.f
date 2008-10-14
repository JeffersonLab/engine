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
      include 'bigcal_bypass_switches.cmn'

      real xdiff,ydiff,ediff
      integer i

      abort = .false.
      err= ' '

c$$$      if(gepid_gep_coin1_times.gt.0.and.ntrigH1.gt.0.and.gen_event_trigtype(4)
c$$$     $     .eq.1.and.ntrigB.gt.0) then
c$$$         call hf2(gepid_gep_coin1_times,GEP_Btime(1),GEP_H1time(1),1.)
c$$$      endif
c$$$      
c$$$      if(gepid_gep_coin2_times.gt.0.and.ntrigH2.gt.0.and.gen_event_trigtype(5)
c$$$     $     .eq.1.and.ntrigB.gt.0) then
c$$$         call hf2(gepid_gep_coin2_times,GEP_Btime(1),GEP_H2time(1),1.)
c$$$      endif

c$$$      if(gepid_gep_ntrig_h1.gt.0) call hf1(gepid_gep_ntrig_h1,float(ntrigH1),1.)
c$$$      if(gepid_gep_ntrig_h2.gt.0) call hf1(gepid_gep_ntrig_h2,float(ntrigH2),1.)
c$$$      if(gepid_gep_ntrig_bigcal.gt.0) call hf1(gepid_gep_ntrig_bigcal,float(ntrigB),1.)
      
      if(hsnum_fptrack.gt.0.and.bigcal_itrack_best.gt.0.and.
     $     (bigcal_do_calibration.eq.0.or.gep_good_calib_event)) then

         xdiff = bigcal_all_clstr_x(bigcal_itrack_best) - gep_bx_expect_h
         ydiff = bigcal_all_clstr_y(bigcal_itrack_best) - gep_by_expect_h
         ediff = gep_e_electron - bigcal_energy
         
         

         if(gen_event_trigtype(5).eq.1) then
            i=2
         else if(gen_event_trigtype(4).eq.1) then
            i=1
         else
            goto 999
         endif

         if(gepid_hgep_delta(i).gt.0) call hf1(gepid_hgep_delta(i),hsdelta,1.)
         if(gepid_hgep_q2_hms(i).gt.0) call hf1(gepid_hgep_q2_hms(i),gep_q2_h-gep_Q2_central,1.)
         if(gepid_hgep_q2_cal(i).gt.0) call hf1(gepid_hgep_q2_cal(i),gep_q2_b-gep_Q2_central,1.)
         if(gepid_hgep_q2(i).gt.0) call hf1(gepid_hgep_q2(i),gep_q2-gep_Q2_central,1.)
         if(gepid_hgep_ecal(i).gt.0) call hf1(gepid_hgep_ecal(i),bigcal_energy-gep_Ee_central,1.)
         if(gepid_hgep_pp(i).gt.0) call hf1(gepid_hgep_pp(i),hsp-hpcentral,1.)
         if(gepid_hgep_epsilon(i).gt.0) call hf1(gepid_hgep_epsilon(i),gep_epsilon,1.)
         if(gepid_hgep_etheta(i).gt.0) call hf1(gepid_hgep_etheta(i),gep_etheta_deg-bigcal_theta_deg,1.)
         if(gepid_hgep_ephi(i).gt.0) call hf1(gepid_hgep_ephi(i),gep_ephi_deg,1.)
         if(gepid_hgep_ptheta(i).gt.0) call hf1(gepid_hgep_ptheta(i),gep_ptheta_deg-htheta_lab,1.)
         if(gepid_hgep_pphi(i).gt.0) call hf1(gepid_hgep_pphi(i),gep_pphi_deg,1.)
         if(gepid_hgep_emiss(i).gt.0) call hf1(gepid_hgep_emiss(i),gep_emiss,1.)
         if(gepid_hgep_pmissx(i).gt.0) call hf1(gepid_hgep_pmissx(i),gep_pmissx,1.)
         if(gepid_hgep_pmissy(i).gt.0) call hf1(gepid_hgep_pmissy(i),gep_pmissy,1.)
         if(gepid_hgep_pmissz(i).gt.0) call hf1(gepid_hgep_pmissz(i),gep_pmissz,1.)
         
         if(gepid_hgep_xdiff(i).gt.0) call hf1(gepid_hgep_xdiff(i),xdiff,1.)
         if(gepid_hgep_ydiff(i).gt.0) call hf1(gepid_hgep_ydiff(i),ydiff,1.)
         if(gepid_hgep_xydiff(i).gt.0) call hf2(gepid_hgep_xydiff(i),xdiff,ydiff,1.)
         if(gepid_hgep_ediff(i).gt.0) call hf1(gepid_hgep_ediff(i),ediff,1.)
         if(gepid_hgep_dpel(i).gt.0) call hf1(gepid_hgep_dpel(i),
     $        (hsp-gep_pel_htheta)/hpcentral*100.,1.)
      endif

 999  continue
      
      return 
      end

      