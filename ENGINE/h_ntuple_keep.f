      subroutine h_Ntuple_keep(ABORT,err)
*----------------------------------------------------------------------
*
*     Purpose : Add entry to the HMS Ntuple
*
*     Output: ABORT      - success or failure
*           : err        - reason for failure, if any
*
*     Created: 11-Apr-1994  K.B.Beard, Hampton U.
* $Log: h_ntuple_keep.f,v $
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
      INCLUDE 'hms_scin_parms.cmn'
      INCLUDE 'gen_scalers.cmn'
      include 'hms_track_histid.cmn'  !temp junk.
      INCLUDE 'hms_calorimeter.cmn'
      INCLUDE 'hms_aero_parms.cmn'
*
      logical HEXIST	!CERNLIB function
*
      integer m

	real*8 save_threescin1,save_hnclust1 ! TH - Add for scin hit checks
	real*8 save_threescin2,save_hnclust2 ! TH - Add for scin hit checks
	real*8 save_threescin3,save_hnclust3 ! TH - Add for scin hit checks
	real*8 save_threescin4,save_hnclust4 ! TH - Add for scin hit checks
	real*8 save_htotscin1,save_htotscin2 ! TH - Add for scin hit checks
	real*8 save_htotscin3,save_htotscin4 ! TH - Add for scin hit checks

	common /save_track/ save_threescin1,save_hnclust1,
     >                      save_threescin2,save_hnclust2,
     >                      save_threescin3,save_hnclust3,
     >                      save_threescin4,save_hnclust4,
     >                      save_htotscin1,save_htotscin2,
     >                      save_htotscin3,save_htotscin4

      real proton_mass
      parameter ( proton_mass = 0.93827247 ) ! [GeV/c^2]
*
*--------------------------------------------------------
      err= ' '
      ABORT = .FALSE.
*
      IF(.NOT.h_Ntuple_exists) RETURN       !nothing to do
c
      if (h_Ntuple_max_segmentevents .gt. 0) then
       if (h_Ntuple_segmentevents .gt. h_Ntuple_max_segmentevents) then
        call h_ntuple_change(ABORT,err)
        h_Ntuple_segmentevents = 0
       else
        h_Ntuple_segmentevents = h_Ntuple_segmentevents +1
       endif
      endif
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
      h_Ntuple_contents(m)= gbcm2_charge ! Charge of last scaler event
      m= m+1
      h_Ntuple_contents(m)= g_beam_on_bcm_charge(2) ! Charge of last scaler event
      m= m+1
      h_Ntuple_contents(m)= HSDELTA	! Spectrometer delta of chosen track
      m= m+1
      h_Ntuple_contents(m)= HSTHETA	! Lab Scattering angle in radians
      m= m+1
      h_Ntuple_contents(m)= HSPHI	! Lab Azymuthal angle in radians
      m= m+1
      h_Ntuple_contents(m)= HINVMASS	! Invariant Mass of remaing hadronic system
      m= m+1
      h_Ntuple_contents(m)= HSBIGQ2     ! Four momentum transfer magnitud
      m= m+1
      h_Ntuple_contents(m)= HSZBEAM! Lab Z coordinate of intersection of beam
c                                ! track with spectrometer ray
      m= m+1
      h_Ntuple_contents(m)= HSDEDX(1)	! DEDX of chosen track in 1st scin plane
      m= m+1
      h_Ntuple_contents(m)= HSBETA	! BETA of chosen track
      m= m+1
      h_Ntuple_contents(m)= HBETA_NOTRK ! untracked BETA 
      m= m+1
      h_Ntuple_contents(m)= HSSHSUM	! Untracked Total shower energy of chosen track
      m= m+1
      h_Ntuple_contents(m)= HSSHTRK	! Tracked Total shower energy of chosen track
      m= m+1
      h_Ntuple_contents(m)= HSPRTRK	! Tracked preshower of chosen track
      m= m+1
      h_Ntuple_contents(m)= HAERO_NPE_SUM
      m= m+1
      h_Ntuple_contents(m)= HAERO_POS_NPE_SUM
      m= m+1
      h_Ntuple_contents(m)= HAERO_NEG_NPE_SUM
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
      m= m+1
      h_Ntuple_contents(m)= hcal_et
      m= m+1
      h_Ntuple_contents(m)= hntracks_fp
      m= m+1
      h_Ntuple_contents(m)= hgoodscinhits
c
      m= m+1
      h_Ntuple_contents(m)= gfrx_raw_adc
      m= m+1
      h_Ntuple_contents(m)= gfry_raw_adc
      m= m+1
      h_Ntuple_contents(m)= gbeam_x
      m= m+1
      h_Ntuple_contents(m)= gbeam_y
*      m= m+1
*      h_Ntuple_contents(m)= HSX_S1	
*      m= m+1
*      h_Ntuple_contents(m)= HSX_S2	
*      m= m+1
*      h_Ntuple_contents(m)= HSY_S1	
*      m= m+1
*      h_Ntuple_contents(m)= HSY_S2	
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
      m= m+1
      h_Ntuple_contents(m)= hmisc_dec_data(34,1)
      m= m+1
      h_Ntuple_contents(m)= hcer_adc(1)
      m= m+1
      h_Ntuple_contents(m)= hcer_adc(2)
      m= m+1
      h_Ntuple_contents(m)= hmisc_dec_data(35,1)
      m= m+1
      h_Ntuple_contents(m)= hmisc_dec_data(10,1)
      m= m+1
      h_Ntuple_contents(m)= hmisc_dec_data(9,1)
      m= m+1
      h_Ntuple_contents(m)= hmisc_dec_data(31,1)
      m= m+1
      h_Ntuple_contents(m)= hmisc_dec_data(41,1)
      m= m+1
      h_Ntuple_contents(m)= hmisc_dec_data(42,1)
      m= m+1
      h_Ntuple_contents(m)= hmisc_dec_data(43,1)
      m= m+1
      h_Ntuple_contents(m)= hmisc_dec_data(44,1)
      m= m+1
      h_Ntuple_contents(m)= hmisc_dec_data(45,1)
      m= m+1
      h_Ntuple_contents(m)= hmisc_dec_data(27,1)
      m= m+1
      h_Ntuple_contents(m)= hmisc_dec_data(30,1)
      m= m+1
      h_Ntuple_contents(m)= hmisc_dec_data(28,1)
      m= m+1
      h_Ntuple_contents(m)= gscaler(185)
      m= m+1
      h_Ntuple_contents(m)= gscaler(189)
      m= m+1
      h_Ntuple_contents(m)= gscaler(345)
      m= m+1
      h_Ntuple_contents(m)= gscaler(349)
      m= m+1
      h_Ntuple_contents(m)= gscaler(160)
      m= m+1
      h_Ntuple_contents(m)= hnumscins1
      m= m+1
      h_Ntuple_contents(m)= hnumscins2
      m= m+1
      h_Ntuple_contents(m)= hnumscins3
      m= m+1
      h_Ntuple_contents(m)= hnumscins4
      m= m+1
      h_Ntuple_contents(m)= hscal_suma
      m= m+1
      h_Ntuple_contents(m)= hscal_sumb
      m= m+1
      h_Ntuple_contents(m)= hscal_sumc
      m= m+1
      h_Ntuple_contents(m)= hscal_sumd

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
