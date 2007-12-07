      subroutine gep_clear_event(ABORT,err)

      implicit none
      save
      
      character*15 here
      parameter(here='gep_clear_event')
      
      logical ABORT
      character*(*) err
      
      integer i

      include 'gep_data_structures.cmn'

      ntrigh1 = 0
      ntrigh2 = 0
      ntrigb = 0

      do i=1,8
         gep_h1time(i) = 0.
         gep_h2time(i) = 0.
         gep_btime(i) = 0.
      enddo

      gep_btime_raw = 0.
      gep_btime_corr = 0.

      gep_ctime_hms = 0.
      gep_ctime_cal = 0.
      gep_Q2 = 0.
      gep_Q2_H = 0.
      gep_Q2_B = 0.
      gep_E_electron = 0.
      gep_P_proton = 0.
      gep_delta_P = 0.
      gep_epsilon = 0.
      gep_xfp_p = 0.
      gep_yfp_p = 0.
      gep_xpfp_p = 0.
      gep_ypfp_p = 0.
      gep_xptar_p = 0.
      gep_yptar_p = 0.
      gep_ytar_p = 0.

      gep_etheta_deg = 0.
      gep_ephi_deg = 0.
      gep_ptheta_deg = 0.
      gep_pphi_deg = 0.
      gep_emiss = 0.
      gep_pmissx = 0.
      gep_pmissy = 0.
      gep_pmissz = 0.
      gep_pmiss = 0.
      gep_w2 = 0.
      gep_mmiss = 0.

      gep_bx_expect_H = 0.
      gep_by_expect_H = 0.
      gep_etheta_expect_H = 0.
      gep_ephi_expect_h = 0.

      call gep_ntuple_clear

      abort=.false.
      err=' '

      return 
      end
