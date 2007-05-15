      subroutine gep_clear_event(ABORT,err)

      implicit none
      save
      
      character*15 here
      parameter(here='gep_clear_event')
      
      logical ABORT
      character*(*) err
      
      include 'gep_data_structures.cmn'

      GEP_ctime_hms = 0.
      GEP_ctime_cal = 0.
      GEP_ctime_cor = 0.
      GEP_Q2 = 0.
      GEP_E_electron = 0.
      GEP_P_proton = 0.
      GEP_delta_p = 0.
      GEP_epsilon = 0.
      GEP_etheta_deg = 0.
      GEP_ephi_deg = 0.
      GEP_ptheta_deg = 0.
      GEP_pphi_deg = 0.
      GEP_Emiss = 0.
      GEP_Pmissx = 0.
      GEP_Pmissy = 0.
      GEP_Pmissz = 0.
      GEP_Pmiss = 0.
      GEP_W2 = 0.
      GEP_Mmiss = 0.

      call gep_ntuple_clear

      abort=.false.
      err=' '

      return 
      end
