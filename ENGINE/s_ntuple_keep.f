      subroutine s_Ntuple_keep(ABORT,err)
*----------------------------------------------------------------------
*
*     Purpose : Add entry to the SOS Ntuple
*
*     Output: ABORT      - success or failure
*           : err        - reason for failure, if any
*
*     Created: 11-Apr-1994  K.B.Beard, Hampton U.
* $Log$
* Revision 1.6  1996/01/16 16:40:31  cdaq
* (JRA) Modify ntuple contents
*
* Revision 1.5  1995/09/01 13:38:46  cdaq
* (JRA) Add Cerenkov photoelectron count to ntuple
*
* Revision 1.4  1995/05/22  20:50:48  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.3  1995/05/11  19:00:39  cdaq
* (SAW) Change SSDEDXn vars to an array.
*
* Revision 1.2  1994/06/17  02:42:33  cdaq
* (KBB) Upgrade
*
* Revision 1.1  1994/04/12  16:16:28  cdaq
* Initial revision
*
*
*----------------------------------------------------------------------
      implicit none
      save
*
      character*13 here
      parameter (here='s_Ntuple_keep')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 's_ntuple.cmn'
      INCLUDE 'sos_data_structures.cmn'
      INCLUDE 'gen_event_info.cmn'
      INCLUDE 'sos_tracking.cmn'
      INCLUDE 'sos_physics_sing.cmn'
      INCLUDE 'sos_scin_tof.cmn'
      include "sos_track_histid.cmn"
*
      logical HEXIST    !CERNLIB function
*
      integer m
*
*--------------------------------------------------------
      err= ' '
      ABORT = .FALSE.
*
      IF(.NOT.s_Ntuple_exists) RETURN       !nothing to do
*
      m= 0
*  
***********************************************************
      m= m+1
      s_Ntuple_contents(m)= SCER_NPE_SUM ! cerenkov photoelectron spectrum
      m= m+1
      s_Ntuple_contents(m)= SSP	! Lab momentum of chosen track in GeV/c
      m= m+1
      s_Ntuple_contents(m)= SSENERGY! Lab total energy of chosen track in GeV
      m= m+1
      s_Ntuple_contents(m)= SSDELTA	! Spectrometer delta of chosen track
      m= m+1
      s_Ntuple_contents(m)= SSTHETA	! Lab Scattering angle in radians
      m= m+1
      s_Ntuple_contents(m)= SSPHI	! Lab Azymuthal angle in radians
      m= m+1
      s_Ntuple_contents(m)= SSMINV	! Invariant Mass of remaing hadronic system
      m= m+1
      s_Ntuple_contents(m)= SSZBEAM! Lab Z coordinate of intersection of beam
                                ! track with spectrometer ray
      m= m+1
      s_Ntuple_contents(m)= SSDEDX(1)	! DEDX of chosen track in 1st scin plane
c      m= m+1
c      s_Ntuple_contents(m)= SSDEDX(2)	! DEDX of chosen track in 2nd scin plane
c      m= m+1
c      s_Ntuple_contents(m)= SSDEDX(3)	! DEDX of chosen track in 3rd scin plane
c      m= m+1
c      s_Ntuple_contents(m)= SSDEDX(4)	! DEDX of chosen track in 4th scin plane
      m= m+1
      s_Ntuple_contents(m)= SSBETA	! BETA of chosen track
      m= m+1
      s_Ntuple_contents(m)= SSTRACK_ET	! Total shower energy of chosen track
      m= m+1
      s_Ntuple_contents(m)= SSTRACK_PRESHOWER_E	! preshower of chosen track
      m= m+1
      s_Ntuple_contents(m)= SSTIME_AT_FP
      m= m+1
      s_Ntuple_contents(m)= SSX_FP		! X focal plane position 
      m= m+1
      s_Ntuple_contents(m)= SSY_FP
      m= m+1
      s_Ntuple_contents(m)= SSXP_FP
      m= m+1
      s_Ntuple_contents(m)= SSYP_FP
      m= m+1
      s_Ntuple_contents(m)= SSCHI2PERDEG	! CHI2 per degree of freedom of chosen track.
      m= m+1
      s_Ntuple_contents(m)= float(SSNFREE_FP)
      m= m+1
      s_Ntuple_contents(m)= SSY_TAR
      m= m+1
      s_Ntuple_contents(m)= SSXP_TAR
      m= m+1
      s_Ntuple_contents(m)= SSYP_TAR
*
c      m= m+1
c      s_Ntuple_contents(m)= float(SSNUM_FPTRACK)! Index of focal plane track chosen
c      m= m+1
c      s_Ntuple_contents(m)= float(SSNUM_TARTRACK)! Index of target track chosen
c      m= m+1
c      s_Ntuple_contents(m)= float(SSID_LUND)! LUND particle ID code -- not yet filled
c      m= m+1
c      s_Ntuple_contents(m)= float(SSNFREE_FP)
*
      m= m+1
      s_Ntuple_contents(m)= float(gen_event_ID_number)
*
      m= m+1
      s_Ntuple_contents(m)= sscin_tot_hits
      m= m+1
      s_Ntuple_contents(m)= ssnum_scin_hit
      m= m+1
      s_Ntuple_contents(m)= sstart_time
c      m= m+1
c      s_Ntuple_contents(m)= s_fptime(1)
c      m= m+1
c      s_Ntuple_contents(m)= s_fptime(2)
c      m= m+1
c      s_Ntuple_contents(m)= s_fptime(3)
c      m= m+1
c      s_Ntuple_contents(m)= s_fptime(4)
c      m= m+1
c      s_Ntuple_contents(m)= ssscin_elem_hit(1)
c      m= m+1
c      s_Ntuple_contents(m)= ssscin_elem_hit(2)
c      m= m+1
c      s_Ntuple_contents(m)= ssscin_elem_hit(3)
c      m= m+1
c      s_Ntuple_contents(m)= ssscin_elem_hit(4)
      m= m+1
      s_Ntuple_contents(m)= sdc_raw_tot_hits
      m= m+1
      s_Ntuple_contents(m)= sdc_tot_hits
*
c      m= m+1
c      s_Ntuple_contents(m)= ssx_sp1
c      m= m+1
c      s_Ntuple_contents(m)= ssy_sp1
c      m= m+1
c      s_Ntuple_contents(m)= ssxp_sp1
c      m= m+1
c      s_Ntuple_contents(m)= ssx_sp2
c      m= m+1
c      s_Ntuple_contents(m)= ssy_sp2
c      m= m+1
c      s_Ntuple_contents(m)= ssxp_sp2
*
c      m= m+1
c      s_Ntuple_contents(m)= sdc_sing_res(1)
c      m= m+1
c      s_Ntuple_contents(m)= sdc_sing_res(2)
c      m= m+1
c      s_Ntuple_contents(m)= sdc_sing_res(3)
c      m= m+1
c      s_Ntuple_contents(m)= sdc_sing_res(4)
c      m= m+1
c      s_Ntuple_contents(m)= sdc_sing_res(5)
c      m= m+1
c      s_Ntuple_contents(m)= sdc_sing_res(6)
c      m= m+1
c      s_Ntuple_contents(m)= sdc_sing_res(7)
c      m= m+1
c      s_Ntuple_contents(m)= sdc_sing_res(8)
c      m= m+1
c      s_Ntuple_contents(m)= sdc_sing_res(9)
c      m= m+1
c      s_Ntuple_contents(m)= sdc_sing_res(10)
c      m= m+1
c      s_Ntuple_contents(m)= sdc_sing_res(11)
c      m= m+1
c      s_Ntuple_contents(m)= sdc_sing_res(12)
c      m= m+1
c      s_Ntuple_contents(m)= sdc_sing_drifttime(1)
c      m= m+1
c      s_Ntuple_contents(m)= sdc_sing_drifttime(2)
c      m= m+1
c      s_Ntuple_contents(m)= sdc_sing_drifttime(3)
c      m= m+1
c      s_Ntuple_contents(m)= sdc_sing_drifttime(4)
c      m= m+1
c      s_Ntuple_contents(m)= sdc_sing_drifttime(5)
c      m= m+1
c      s_Ntuple_contents(m)= sdc_sing_drifttime(6)
c      m= m+1
c      s_Ntuple_contents(m)= sdc_sing_drifttime(7)
c      m= m+1
c      s_Ntuple_contents(m)= sdc_sing_drifttime(8)
c      m= m+1
c      s_Ntuple_contents(m)= sdc_sing_drifttime(9)
c      m= m+1
c      s_Ntuple_contents(m)= sdc_sing_drifttime(10)
c      m= m+1
c      s_Ntuple_contents(m)= sdc_sing_drifttime(11)
c      m= m+1
c      s_Ntuple_contents(m)= sdc_sing_drifttime(12)
c      m= m+1
c      s_Ntuple_contents(m)= sdc_sing_driftdis(1)
c      m= m+1
c      s_Ntuple_contents(m)= sdc_sing_driftdis(2)
c      m= m+1
c      s_Ntuple_contents(m)= sdc_sing_driftdis(3)
c      m= m+1
c      s_Ntuple_contents(m)= sdc_sing_driftdis(4)
c      m= m+1
c      s_Ntuple_contents(m)= sdc_sing_driftdis(5)
c      m= m+1
c      s_Ntuple_contents(m)= sdc_sing_driftdis(6)
c      m= m+1
c      s_Ntuple_contents(m)= sdc_sing_driftdis(7)
c      m= m+1
c      s_Ntuple_contents(m)= sdc_sing_driftdis(8)
c      m= m+1
c      s_Ntuple_contents(m)= sdc_sing_driftdis(9)
c      m= m+1
c      s_Ntuple_contents(m)= sdc_sing_driftdis(10)
c      m= m+1
c      s_Ntuple_contents(m)= sdc_sing_driftdis(11)
c      m= m+1
c      s_Ntuple_contents(m)= sdc_sing_driftdis(12)
*
********************************************************
*
      ABORT= .NOT.HEXIST(s_Ntuple_ID)
      IF(ABORT) THEN
        call G_build_note(':Ntuple ID#$ does not exist',
     &                        '$',s_Ntuple_ID,' ',0.,' ',err)
        call G_add_path(here,err)
      ELSE
        call HFN(s_Ntuple_ID,s_Ntuple_contents)
      ENDIF
*
      RETURN
      END      
