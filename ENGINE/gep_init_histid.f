      subroutine gep_init_histid(abort,err)

      implicit none
      save

      character*15 here
      parameter(here='gep_init_histid')

      logical abort
      character*(*) err
      external thgetid
      integer*4 thgetid
     
      integer i

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
      gepid_slowrastx = thgetid('gep_slowrastx')
      gepid_slowrasty = thgetid('gep_slowrasty')
      gepid_slowrastxy = thgetid('gep_slowrastxy')
      gepid_slowrastxy2 = thgetid('gep_slowrastxy2')
      gepid_gep_bigcal_rawtdc = thgetid('gep_bigcal_rawtdc')
      gepid_gep_coin1_times = thgetid('gep_coin1_times')
      gepid_gep_coin2_times = thgetid('gep_coin2_times')
      gepid_gep_ntrig_h1 = thgetid('gep_ntrig_h1')
      gepid_gep_ntrig_h2 = thgetid('gep_ntrig_h2')
      gepid_gep_ntrig_bigcal = thgetid('gep_ntrig_bigcal')

      do i=1,2

         gepid_hgep_delta(i) = thgetid('hgep_delta'//char(i+ichar('0')))
         gepid_hgep_q2_hms(i) = thgetid('hgep_q2_hms'//char(i+ichar('0')))
         gepid_hgep_q2_cal(i) = thgetid('hgep_q2_cal'//char(i+ichar('0')))
         gepid_hgep_q2(i)     = thgetid('hgep_q2_'//char(i+ichar('0')))
         gepid_hgep_ecal(i)   = thgetid('hgep_ecal'//char(i+ichar('0')))
         gepid_hgep_pp(i)     = thgetid('hgep_pp'//char(i+ichar('0')))
         gepid_hgep_epsilon(i) = 
     $        thgetid('hgep_epsilon'//char(i+ichar('0')))
         gepid_hgep_etheta(i) = thgetid('hgep_etheta'//char(i+ichar('0')))
         gepid_hgep_ephi(i)   = thgetid('hgep_ephi'//char(i+ichar('0')))
         gepid_hgep_ptheta(i) = thgetid('hgep_ptheta'//char(i+ichar('0')))
         gepid_hgep_pphi(i)   = thgetid('hgep_pphi'//char(i+ichar('0')))
         gepid_hgep_emiss(i)  = thgetid('hgep_emiss'//char(i+ichar('0')))
         gepid_hgep_pmissx(i) = thgetid('hgep_pmissx'//char(i+ichar('0')))
         gepid_hgep_pmissy(i) = thgetid('hgep_pmissy'//char(i+ichar('0')))
         gepid_hgep_pmissz(i) = thgetid('hgep_pmissz'//char(i+ichar('0')))
         gepid_hgep_xdiff(i)  = thgetid('hgep_xdiff'//char(i+ichar('0')))
         gepid_hgep_ydiff(i)  = thgetid('hgep_ydiff'//char(i+ichar('0')))
         gepid_hgep_xydiff(i) = thgetid('hgep_xydiff'//char(i+ichar('0')))
         gepid_hgep_ediff(i)  = thgetid('hgep_ediff'//char(i+ichar('0')))
         gepid_hgep_dpel(i)   = thgetid('hgep_dpel'//char(i+ichar('0')))
      
      enddo

      return 
      end
