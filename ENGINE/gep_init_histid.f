      subroutine gep_init_histid(abort,err)

      implicit none
      save

      character*15 here
      parameter(here='gep_init_histid')

      logical abort
      character*(*) err
      external thgetid
      integer*4 thgetid
     
      include 'gep_data_structures.cmn'
      include 'gep_hist_id.cmn'

c     do nothing until we set up some histograms...
c     miscellaneous hard-coded histograms for checkout

      abort=.false.
      err=' '

      gepid_gep_trigtype = thgetid('gep_trigtype')
      gepid_gep_evtype = thgetid('gep_evtype')
      gepid_gep_trigtype_vs_evtype = thgetid('gep_trigtype_vs_evtype')
      gepid_gep_ntrigs = thgetid('gep_ntrigs')
      gepid_gep_HMS1_rawtdc = thgetid('gep_HMS1_rawtdc')
      gepid_gep_HMS2_rawtdc = thgetid('gep_HMS2_rawtdc')
      gepid_gep_bigcal_rawtdc = thgetid('gep_bigcal_rawtdc')
      gepid_gep_coin1_times = thgetid('gep_coin1_times')
      gepid_gep_coin2_times = thgetid('gep_coin2_times')
      gepid_gep_ntrig_h1 = thgetid('gep_ntrig_h1')
      gepid_gep_ntrig_h2 = thgetid('gep_ntrig_h2')
      gepid_gep_ntrig_bigcal = thgetid('gep_ntrig_bigcal')
      gepid_hgep_delta = thgetid('hgep_delta')
      gepid_hgep_q2_hms = thgetid('hgep_q2_hms')
      gepid_hgep_q2_cal = thgetid('hgep_q2_cal')
      gepid_hgep_q2     = thgetid('hgep_q2')
      gepid_hgep_ecal   = thgetid('hgep_ecal')
      gepid_hgep_pp     = thgetid('hgep_pp')
      gepid_hgep_epsilon = thgetid('hgep_epsilon')
      gepid_hgep_etheta = thgetid('hgep_etheta')
      gepid_hgep_ephi   = thgetid('hgep_ephi')
      gepid_hgep_ptheta = thgetid('hgep_ptheta')
      gepid_hgep_pphi   = thgetid('hgep_pphi')
      gepid_hgep_emiss  = thgetid('hgep_emiss')
      gepid_hgep_pmissx = thgetid('hgep_pmissx')
      gepid_hgep_pmissy = thgetid('hgep_pmissy')
      gepid_hgep_pmissz = thgetid('hgep_pmissz')
      gepid_hgep_xdiff  = thgetid('hgep_xdiff')
      gepid_hgep_ydiff  = thgetid('hgep_ydiff')
      gepid_hgep_xydiff = thgetid('hgep_xydiff')
      gepid_hgep_ediff  = thgetid('hgep_ediff')
      gepid_hgep_dpel   = thgetid('hgep_dpel')
      
      return 
      end
