      subroutine c_Ntuple_keep(ABORT,err)
*----------------------------------------------------------------------
*
*     Purpose : Add entry to the COIN Ntuple
*
*     Output: ABORT      - success or failure
*           : err        - reason for failure, if any
*
*     Created: 11-Apr-1994  K.B.Beard, Hampton U.
* $Log$
* Revision 1.8  1996/09/04 15:30:17  saw
* (JRA) Modify ntuple contents
*
* Revision 1.7  1996/04/29 18:44:04  saw
* (JRA) Add aerogel photon count
*
* Revision 1.6  1996/01/22 15:06:41  saw
* (JRA) Change ntuple contents
*
* Revision 1.5  1996/01/16 21:01:33  cdaq
* (JRA) Add HSDELTA and SSDELTA
*
* Revision 1.4  1995/09/01 15:45:21  cdaq
* (JRA) Add spectrometer kinematic vars to ntuple
*
* Revision 1.3  1995/05/22  20:50:43  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.2  1994/06/17  02:41:25  cdaq
* (KBB) Upgrade
*
* Revision 1.1  1994/04/12  16:12:33  cdaq
* Initial revision
*
*
*----------------------------------------------------------------------
      implicit none
      save
*
      character*13 here
      parameter (here='c_Ntuple_keep')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 'c_ntuple.cmn'
      INCLUDE 'gen_data_structures.cmn'
      INCLUDE 'coin_data_structures.cmn'
      INCLUDE 'hms_data_structures.cmn'
      INCLUDE 'sos_data_structures.cmn'
      INCLUDE 'hms_scin_parms.cmn'
      INCLUDE 'sos_scin_parms.cmn'
      INCLUDE 'sos_track_histid.cmn'
      INCLUDE 'sos_aero_parms.cmn'
      INCLUDE 's_ntuple.cmn'
      INCLUDE 'sos_tracking.cmn'
      INCLUDE 'gen_event_info.cmn'
      INCLUDE 'gen_scalers.cmn'
*
      logical HEXIST    !CERNLIB function
*
      integer m
*
*--------------------------------------------------------
      err= ' '
      ABORT = .FALSE.
*
      IF(.NOT.c_Ntuple_exists) RETURN       !nothing to do
*
**********begin insert description of contents of COIN tuple ******

      m= 0
      m= m+1
      c_Ntuple_contents(m)= ccointime_hms ! Corrected Coincidence time
      m= m+1
      c_Ntuple_contents(m)= gbpm_x(3)   ! Beam X Position
      m= m+1
      c_Ntuple_contents(m)= gbpm_y(3)   ! Beam Y Position
      m= m+1
      c_Ntuple_contents(m)= gbpm_xprime(3) ! Beam X Position
      m= m+1
      c_Ntuple_contents(m)= gbpm_yprime(3) ! Beam Y Position
      m= m+1
      c_Ntuple_contents(m)= HSX_FP      ! HMS Focal Plane
      m= m+1
      c_Ntuple_contents(m)= HSY_FP      ! 
      m= m+1
      c_Ntuple_contents(m)= HSXP_FP     ! 
      m= m+1
      c_Ntuple_contents(m)= HSYP_FP     ! 
      m= m+1
      c_Ntuple_contents(m)= SSX_FP      ! SOS Focal Plane
      m= m+1
      c_Ntuple_contents(m)= SSY_FP      ! 
      m= m+1
      c_Ntuple_contents(m)= SSXP_FP     ! 
      m= m+1
      c_Ntuple_contents(m)= SSYP_FP     ! 
      m= m+1
      c_Ntuple_contents(m)= HSY_TAR     ! HMS Target
      m= m+1
      c_Ntuple_contents(m)= HSXP_TAR    ! 
      m= m+1
      c_Ntuple_contents(m)= HSYP_TAR    ! 
      m= m+1
      c_Ntuple_contents(m)= HSDELTA     !
      m= m+1
      c_Ntuple_contents(m)= SSY_TAR     ! SOS Target
      m= m+1
      c_Ntuple_contents(m)= SSXP_TAR    ! 
      m= m+1
      c_Ntuple_contents(m)= SSYP_TAR    ! 
      m= m+1
      c_Ntuple_contents(m)= SSDELTA     !
      m= m+1
      c_Ntuple_contents(m)= HCER_NPE_SUM ! HMS Particle Id.
      m= m+1
      c_Ntuple_contents(m)= HSTRACK_ET  !
      m= m+1
      c_Ntuple_contents(m)= HSTRACK_PRESHOWER_E !
      m= m+1
      c_Ntuple_contents(m)= HSBETA      !
      m= m+1
      c_Ntuple_contents(m)= HSDEDX(1)   !
      m= m+1
      c_Ntuple_contents(m)= SCER_NPE_SUM ! SOS Particle Id.
      m= m+1
      c_Ntuple_contents(m)= SAER_NPE_SUM ! SAER_NPE_SUM
      m= m+1
      c_Ntuple_contents(m)= SSTRACK_ET  !
      m= m+1
      c_Ntuple_contents(m)= SSTRACK_PRESHOWER_E !
      m= m+1
      c_Ntuple_contents(m)= SSBETA      !
      m= m+1
      c_Ntuple_contents(m)= SSDEDX(1)   !
      m= m+1
      c_Ntuple_contents(m)= gbcm1_charge ! Charge of last scaler event
      m= m+1
      c_Ntuple_contents(m)= cmissing_e  ! missing energy
      m= m+1
      c_Ntuple_contents(m)= cmissing_mom ! Missing Momentum
      m= m+1
      c_Ntuple_contents(m)= FLOAT(gen_event_ID_number)
      m= m+1
      c_Ntuple_contents(m)= cmissing_mom_par ! pm parallel to q
      m= m+1
      c_Ntuple_contents(m)= cmissing_mom_perp ! pm perp tp q
      m= m+1
      c_Ntuple_contents(m)= cmissing_mom_oop ! pm out of plane
      m= m+1
      c_Ntuple_contents(m)= P_HMS_CORR  ! Corrected hms singles
      m= m+1
      c_Ntuple_contents(m)= P_SOS_CORR  ! Corrected sos singles
***********end insert description of contents of COIN tuple********
*
      ABORT= .NOT.HEXIST(c_Ntuple_ID)
      IF(ABORT) THEN
        call G_build_note(':Ntuple ID#$ does not exist',
     &                        '$',c_Ntuple_ID,' ',0.,' ',err)
        call G_add_path(here,err)
      ELSE
        call HFN(c_Ntuple_ID,c_Ntuple_contents)
      ENDIF
*
      RETURN
      END      
