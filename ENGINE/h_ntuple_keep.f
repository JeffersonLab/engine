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
      INCLUDE 'gen_event_info.cmn'
      INCLUDE 'hms_tracking.cmn'
      INCLUDE 'hms_physics_sing.cmn'
      INCLUDE 'hms_scin_tof.cmn'
      include 'hms_track_histid.cmn'  !temp junk.
*
      logical HEXIST	!CERNLIB function
*
      integer m
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
c      m= m+1
c      h_Ntuple_contents(m)= HCER_ADC(1) ! cerenkov adc #1
c      m= m+1
c      h_Ntuple_contents(m)= HCER_ADC(2) ! cerenkov adc #2
      m= m+1
      h_Ntuple_contents(m)= HCER_NPE_SUM ! cerenkov photoelectron spectrum
      m= m+1
      h_Ntuple_contents(m)= HSP	        ! Lab momentum of chosen track in GeV/c
      m= m+1
      h_Ntuple_contents(m)= HSENERGY    ! Lab total energy of chosen track in GeV
      m= m+1
      h_Ntuple_contents(m)= HSDELTA	! Spectrometer delta of chosen track
      m= m+1
      h_Ntuple_contents(m)= HSTHETA	! Lab Scattering angle in radians
      m= m+1
      h_Ntuple_contents(m)= HSPHI	! Lab Azymuthal angle in radians
      m= m+1
      h_Ntuple_contents(m)= HSMINV	! Invariant Mass of remaing hadronic system
      m= m+1
      h_Ntuple_contents(m)= HSZBEAM! Lab Z coordinate of intersection of beam
c                                ! track with spectrometer ray
      m= m+1
      h_Ntuple_contents(m)= HSDEDX(1)	! DEDX of chosen track in 1st scin plane
c      m= m+1
c      h_Ntuple_contents(m)= HSDEDX(2)	! DEDX of chosen track in 2nd scin plane
c      m= m+1
c      h_Ntuple_contents(m)= HSDEDX(3)	! DEDX of chosen track in 3rd scin plane
c      m= m+1
c      h_Ntuple_contents(m)= HSDEDX(4)	! DEDX of chosen track in 4th scin plane
      m= m+1
      h_Ntuple_contents(m)= HSBETA	! BETA of chosen track
      m= m+1
      h_Ntuple_contents(m)= HSTRACK_ET	! Total shower energy of chosen track
      m= m+1
      h_Ntuple_contents(m)= HSTRACK_PRESHOWER_E	! preshower of chosen track
      m= m+1
      h_Ntuple_contents(m)= HSTIME_AT_FP
      m= m+1
      h_Ntuple_contents(m)= HSX_FP		! X focal plane position 
      m= m+1
      h_Ntuple_contents(m)= HSY_FP
      m= m+1
      h_Ntuple_contents(m)= HSXP_FP
      m= m+1
      h_Ntuple_contents(m)= HSYP_FP
      m= m+1
      h_Ntuple_contents(m)= HSCHI2PERDEG	! CHI2 per degree of freedom of chosen track.
      m= m+1
      h_Ntuple_contents(m)= float(HSNFREE_FP)
      m= m+1
      h_Ntuple_contents(m)= HSY_TAR
      m= m+1
      h_Ntuple_contents(m)= HSXP_TAR
      m= m+1
      h_Ntuple_contents(m)= HSYP_TAR
*
c      m= m+1
c      h_Ntuple_contents(m)= float(HSNUM_FPTRACK)! Index of focal plane track chosen
c      m= m+1
c      h_Ntuple_contents(m)= float(HSID_LUND)! LUND particle ID code -- not yet filled
*
      m= m+1
      h_Ntuple_contents(m)= float(gen_event_ID_number)
*
      m= m+1
      h_Ntuple_contents(m)= hscin_tot_hits
      m= m+1
      h_Ntuple_contents(m)= hsnum_scin_hit
      m= m+1
      h_Ntuple_contents(m)= hstart_time
c      m= m+1
c      h_Ntuple_contents(m)= h_fptime(1)
c      m= m+1
c      h_Ntuple_contents(m)= h_fptime(2)
c      m= m+1
c      h_Ntuple_contents(m)= h_fptime(3)
c      m= m+1
c      h_Ntuple_contents(m)= h_fptime(4)
c      m= m+1
c      h_Ntuple_contents(m)= hsscin_elem_hit(1)
c      m= m+1
c      h_Ntuple_contents(m)= hsscin_elem_hit(2)
c      m= m+1
c      h_Ntuple_contents(m)= hsscin_elem_hit(3)
c      m= m+1
c      h_Ntuple_contents(m)= hsscin_elem_hit(4)
      m= m+1
      h_Ntuple_contents(m)= hdc_raw_tot_hits
      m= m+1
      h_Ntuple_contents(m)= hdc_tot_hits
*
c      m= m+1
c      h_Ntuple_contents(m)= hsx_sp1
c      m= m+1
c      h_Ntuple_contents(m)= hsy_sp1
c      m= m+1
c      h_Ntuple_contents(m)= hsxp_sp1
c      m= m+1
c      h_Ntuple_contents(m)= hsx_sp2
c      m= m+1
c      h_Ntuple_contents(m)= hsy_sp2
c      m= m+1
c      h_Ntuple_contents(m)= hsxp_sp2
*
c      m= m+1
c      h_Ntuple_contents(m)= hdc_sing_res(1)
c      m= m+1
c      h_Ntuple_contents(m)= hdc_sing_res(2)
c      m= m+1
c      h_Ntuple_contents(m)= hdc_sing_res(3)
c      m= m+1
c      h_Ntuple_contents(m)= hdc_sing_res(4)
c      m= m+1
c      h_Ntuple_contents(m)= hdc_sing_res(5)
c      m= m+1
c      h_Ntuple_contents(m)= hdc_sing_res(6)
c      m= m+1
c      h_Ntuple_contents(m)= hdc_sing_res(7)
c      m= m+1
c      h_Ntuple_contents(m)= hdc_sing_res(8)
c      m= m+1
c      h_Ntuple_contents(m)= hdc_sing_res(9)
c      m= m+1
c     h_Ntuple_contents(m)= hdc_sing_res(10)
c     m= m+1
c      h_Ntuple_contents(m)= hdc_sing_res(11)
c      m= m+1
c      h_Ntuple_contents(m)= hdc_sing_res(12)
c      m= m+1
c      h_Ntuple_contents(m)= hdc_sing_driftdis(1)
c      m= m+1
c      h_Ntuple_contents(m)= hdc_sing_driftdis(2)
c      m= m+1
c      h_Ntuple_contents(m)= hdc_sing_driftdis(3)
c      m= m+1
c      h_Ntuple_contents(m)= hdc_sing_driftdis(4)
c      m= m+1
c      h_Ntuple_contents(m)= hdc_sing_driftdis(5)
c      m= m+1
c      h_Ntuple_contents(m)= hdc_sing_driftdis(6)
c      m= m+1
c      h_Ntuple_contents(m)= hdc_sing_driftdis(7)
c      m= m+1
c      h_Ntuple_contents(m)= hdc_sing_driftdis(8)
c      m= m+1
c      h_Ntuple_contents(m)= hdc_sing_driftdis(9)
c      m= m+1
c      h_Ntuple_contents(m)= hdc_sing_driftdis(10)
c      m= m+1
c      h_Ntuple_contents(m)= hdc_sing_driftdis(11)
c      m= m+1
c      h_Ntuple_contents(m)= hdc_sing_driftdis(12)
*
************************************************
*
*
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
