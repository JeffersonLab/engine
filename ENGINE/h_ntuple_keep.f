      subroutine h_Ntuple_keep(ABORT,err)
*----------------------------------------------------------------------
*
*     Purpose : Add entry to the HMS Ntuple
*
*     Output: ABORT      - success or failure
*           : err        - reason for failure, if any
*
*     Created: 11-Apr-1994  K.B.Beard, Hampton U.
* $Log$
* Revision 1.9  2003/09/05 16:40:26  jones
* Merge in online03 changes (mkj)
*
* Revision 1.8.2.2  2003/09/04 21:10:33  jones
* add event type to ntuple (mkj)
*
* Revision 1.8.2.1  2003/04/04 12:55:11  cdaq
* add beam quantities to ntuple (MKJ)
*
* Revision 1.8  1996/09/04 14:43:17  saw
* (JRA) Modify ntuple contents
*
* Revision 1.7  1996/01/16 17:01:55  cdaq
* (JRA) Modify ntuple contents
*
* Revision 1.6  1995/09/01 13:38:28  cdaq
* (JRA) Add Cerenkov photoelectron count to ntuple
*
* Revision 1.5  1995/05/22  20:50:46  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.4  1995/05/11  17:37:13  cdaq
* (SAW) Change HSDEDXn vars to an array.
*
* Revision 1.3  1995/01/27  20:10:27  cdaq
* (JRA) Add Gas cerenkov to ntuple
*
* Revision 1.2  1994/06/17  02:44:38  cdaq
* (KBB) Upgrade
*
* Revision 1.1  1994/04/12  16:15:21  cdaq
* Initial revision
*
*
*----------------------------------------------------------------------
      implicit none
      save
*
      character*13 here
      parameter (here='h_Ntuple_keep')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 'h_ntuple.cmn'
      INCLUDE 'hms_data_structures.cmn'
      INCLUDE 'gen_data_structures.cmn'
      INCLUDE 'gen_event_info.cmn'
      INCLUDE 'hms_tracking.cmn'
      INCLUDE 'hms_physics_sing.cmn'
      INCLUDE 'hms_scin_tof.cmn'
      INCLUDE 'gen_scalers.cmn'
      include 'hms_track_histid.cmn'  !temp junk.
*
      logical HEXIST	!CERNLIB function
*
      integer m

      real proton_mass
      parameter ( proton_mass = 0.93827247 ) ! [GeV/c^2]
*
*--------------------------------------------------------
      err= ' '
      ABORT = .FALSE.
*
      IF(.NOT.h_Ntuple_exists) RETURN       !nothing to do
*
************************************************
      m= 0
*  
      m= m+1
      h_Ntuple_contents(m)= HCER_NPE_SUM ! cerenkov photoelectron spectrum
      m= m+1
      h_Ntuple_contents(m)= HSP	        ! Lab momentum of chosen track in GeV/c
      m= m+1
      h_Ntuple_contents(m)= HSENERGY    ! Lab total energy of chosen track in GeV
      m= m+1
      h_Ntuple_contents(m)= gbcm1_charge ! Charge of last scaler event
      m= m+1
      h_Ntuple_contents(m)= HSDELTA	! Spectrometer delta of chosen track
      m= m+1
      h_Ntuple_contents(m)= HSTHETA	! Lab Scattering angle in radians
      m= m+1
      h_Ntuple_contents(m)= HSPHI	! Lab Azymuthal angle in radians
      m= m+1
      h_Ntuple_contents(m)= HINVMASS	! Invariant Mass of remaing hadronic system
      m= m+1
      h_Ntuple_contents(m)= HSZBEAM! Lab Z coordinate of intersection of beam
c                                ! track with spectrometer ray
      m= m+1
      h_Ntuple_contents(m)= HSDEDX(1)	! DEDX of chosen track in 1st scin plane
      m= m+1
      h_Ntuple_contents(m)= HSBETA	! BETA of chosen track
      m= m+1
      h_Ntuple_contents(m)= HSTRACK_ET	! Total shower energy of chosen track
      m= m+1
      h_Ntuple_contents(m)= HSTRACK_PRESHOWER_E	! preshower of chosen track
      m= m+1
      h_Ntuple_contents(m)= HSX_FP		! X focal plane position 
      m= m+1
      h_Ntuple_contents(m)= HSY_FP
      m= m+1
      h_Ntuple_contents(m)= HSXP_FP
      m= m+1
      h_Ntuple_contents(m)= HSYP_FP
      m= m+1
      h_Ntuple_contents(m)= HSY_TAR
      m= m+1
      h_Ntuple_contents(m)= HSXP_TAR
      m= m+1
      h_Ntuple_contents(m)= HSYP_TAR
      m= m+1
      h_Ntuple_contents(m)= hstart_time
      m= m+1
      h_Ntuple_contents(m)= float(gen_event_ID_number)
      m= m+1
      h_Ntuple_contents(m)= float(gen_event_type)
c
      m= m+1
      h_Ntuple_contents(m)= gfrx_raw_adc
      m= m+1
      h_Ntuple_contents(m)= gfry_raw_adc
      m= m+1
      h_Ntuple_contents(m)= gbeam_x
      m= m+1
      h_Ntuple_contents(m)= gbeam_y
      m= m+1
      h_Ntuple_contents(m)= gbpm_x(1)
      m= m+1
      h_Ntuple_contents(m)= gbpm_y(1)
      m= m+1
      h_Ntuple_contents(m)= gbpm_x(2)
      m= m+1
      h_Ntuple_contents(m)= gbpm_y(2)
      m= m+1
      h_Ntuple_contents(m)= gbpm_x(3)
      m= m+1
      h_Ntuple_contents(m)= gbpm_y(3)

* Experiment dependent entries start here.


* Fill ntuple for this event
      ABORT= .NOT.HEXIST(h_Ntuple_ID)
      IF(ABORT) THEN
        call G_build_note(':Ntuple ID#$ does not exist',
     &                        '$',h_Ntuple_ID,' ',0.,' ',err)
        call G_add_path(here,err)
      ELSE
        call HFN(h_Ntuple_ID,h_Ntuple_contents)
      ENDIF
*
      RETURN
      END
