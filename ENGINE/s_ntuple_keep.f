      subroutine s_Ntuple_keep(ABORT,err)
* xucc comments begin
* in additoon to comments on s_ntuple_init.f                         
* we have three additional include files
*      include 'sos_calorimeter.cmn'
*      include 'gen_run_info.cmn'
*      include 'gen_data_structures.cmn'
* And we also notice that
* SSSHTRK and SSPRTRK actually is SSTRACK_ET and SSTRACK_PRESHOWER_E
* but since they have different names in our include files
* we changed them here
* 
*  However we need check the difference on this guessing.
* notice that we have tracked beta, and tracked deposit energy
* as well as untracked ones? How about the original 
* total information on SCAL?
* the same question exists on HCAL.
* xucc comments end



*----------------------------------------------------------------------
*
*     Purpose : Add entry to the SOS Ntuple
*
*     Output: ABORT      - success or failure
*           : err        - reason for failure, if any
*
*     Created: 11-Apr-1994  K.B.Beard, Hampton U.
* $Log$
* Revision 1.7.4.1  2003/03/05 22:53:52  xu
* new variables
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
*     xucc added begin
      include 'sos_calorimeter.cmn'
      include 'gen_run_info.cmn'
      include 'gen_data_structures.cmn'
*     xucc added end

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
* xucc added begin
      s_Ntuple_contents(m)= SBETA_NOTRK ! untracked BETA of chosen track
      m= m+1
* xucc added end 

      s_Ntuple_contents(m)= SSBETA	! tracked BETA of chosen track
      m= m+1

*  xucc added begin
      s_Ntuple_contents(m)= SSSHSUM     ! untracked Norm. Total shower energy of chosen track
      m= m+1
* xucc added end

* xucc changing begin
*      s_Ntuple_contents(m)= SSTRACK_ET	! Total shower energy of chosen track
*      m= m+1
*      s_Ntuple_contents(m)= SSTRACK_PRESHOWER_E	! preshower of chosen track
*      m= m+1

*   the following SSSHTRK and SSPRTRK actually is SSTRACK_ET and SSTRACK_PRESHOWER_E
* but since they have different names in our include files
* we changed them here
      s_Ntuple_contents(m)= SSSHTRK     ! tracked Norm. Total shower energy of chosen track
      m= m+1
      s_Ntuple_contents(m)= SSPRTRK     ! tracked normalized preshower of chosen track
      m= m+1
* xucc changing end


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


* Experiment dependent entries start here.
* xucc added begin
      m= m+1
      s_Ntuple_contents(m)= gbpm_x(2)
      m= m+1
      s_Ntuple_contents(m)= gbpm_y(2)
      m= m+1
      s_Ntuple_contents(m)= gfrx_raw_adc
      m= m+1
      s_Ntuple_contents(m)= gfry_raw_adc
* xucc added end


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
