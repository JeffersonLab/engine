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
* Revision 1.7.6.1  2003/12/17 22:55:06  jones
*  update e01004
*
* Revision 1.7  1996/09/04 15:18:21  saw
* (JRA) Modify ntuple contents
*
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
      include 'sos_track_histid.cmn'
      include 'sos_aero_parms.cmn'
*
      logical HEXIST    !CERNLIB function
*
      integer m

      real proton_mass
      parameter ( proton_mass = 0.93827247 ) ! [GeV/c^2]
*
*--------------------------------------------------------
      err= ' '
      ABORT = .FALSE.
*
      IF(.NOT.s_Ntuple_exists) RETURN       !nothing to do
*
      m= 0
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
      s_Ntuple_contents(m)= SINVMASS	! Invariant Mass of remaing hadronic system
      m= m+1
      s_Ntuple_contents(m)= SSZBEAM! Lab Z coordinate of intersection of beam
                                ! track with spectrometer ray
      m= m+1
      s_Ntuple_contents(m)= SSDEDX(1)	! DEDX of chosen track in 1st scin plane
      m= m+1
      s_Ntuple_contents(m)= SSBETA	! BETA of chosen track
      m= m+1
      s_Ntuple_contents(m)= SSTRACK_ET	! Total shower energy of chosen track
      m= m+1
      s_Ntuple_contents(m)= SSTRACK_PRESHOWER_E	! preshower of chosen track
      m= m+1
      s_Ntuple_contents(m)= SSX_FP		! X focal plane position 
      m= m+1
      s_Ntuple_contents(m)= SSY_FP
      m= m+1
      s_Ntuple_contents(m)= SSXP_FP
      m= m+1
      s_Ntuple_contents(m)= SSYP_FP
      m= m+1
      s_Ntuple_contents(m)= SSY_TAR
      m= m+1
      s_Ntuple_contents(m)= SSXP_TAR
      m= m+1
      s_Ntuple_contents(m)= SSYP_TAR
      m= m+1
      s_Ntuple_contents(m)= float(gen_event_ID_number)
      m= m+1
      s_Ntuple_contents(m)= sstart_time
      m= m+1
      s_Ntuple_contents(m)= saer_npe_sum
      m= m+1
      s_Ntuple_contents(m)= gbeam_x
      m= m+1
      s_Ntuple_contents(m)= gbeam_y
      m= m+1
      s_Ntuple_contents(m)= smisc_dec_data(2,2)
      m= m+1
      s_Ntuple_contents(m)= smisc_dec_data(3,2) 
      m= m+1
      s_Ntuple_contents(m)= smisc_dec_data(4,2) 
      m= m+1
c      s_Ntuple_contents(m)= smisc_dec_data(7,1) 
       s_Ntuple_contents(m)= scer_adc(1)
      m= m+1
c      s_Ntuple_contents(m)= smisc_dec_data(8,1) 
       s_Ntuple_contents(m)= scer_adc(2)
      m= m+1
c      s_Ntuple_contents(m)= smisc_dec_data(5,2) 
       s_Ntuple_contents(m)= scer_adc(3)
      m= m+1
c      s_Ntuple_contents(m)= smisc_dec_data(6,2) 
       s_Ntuple_contents(m)= scer_adc(4)
        do ihit=1,sscin_all_tot_hits
          if ( sscin_all_plane_num(ihit) .eq. 1 ) then
             s_Ntuple_contents(m)=sscin_all_counter_num(ihit)  
          endif
        enddo
      m=m+1
        do ihit=1,sscin_all_tot_hits
          if ( sscin_all_plane_num(ihit) .eq. 3 ) then
             s_Ntuple_contents(m)=sscin_all_counter_num(ihit)  
          endif
        enddo


* Experiment dependent entries start here.


* Fill ntuple for this event
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
