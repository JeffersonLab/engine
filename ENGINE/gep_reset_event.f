      subroutine gep_reset_event(ABORT,err)

      implicit none
      save

      character*15 here
      parameter(here='gep_reset_event')

      logical abort
      character*(*) err

      include 'gep_data_structures.cmn'

      abort=.false.
      err=' '

      gep_ctime_hms = 0.
      gep_ctime_cal = 0.
      gep_Q2 = 0.
      gep_Q2_H = 0.
      gep_Q2_B = 0.
      gep_E_electron = 0.
      gep_P_proton = 0.
      gep_delta_P = 0.
      gep_epsilon = 0.
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

      return
      end
