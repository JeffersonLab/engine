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
* Revision 1.10  2004/02/17 17:26:34  jones
* Changes to enable possiblity of segmenting rzdat files
*
* Revision 1.9  1999/02/23 16:41:08  csa
* Variable changes
*
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
      INCLUDE 'gen_run_info.cmn'
      INCLUDE 'coin_data_structures.cmn'
      INCLUDE 'hms_data_structures.cmn'
      INCLUDE 'hms_physics_sing.cmn'
      INCLUDE 'sos_data_structures.cmn'
      INCLUDE 'hms_scin_parms.cmn'
      INCLUDE 'sos_scin_parms.cmn'
      INCLUDE 'sos_track_histid.cmn'
      INCLUDE 'sos_aero_parms.cmn'
      INCLUDE 's_ntuple.cmn'
      INCLUDE 'sos_tracking.cmn'
      INCLUDE 'gen_event_info.cmn'
      INCLUDE 'gen_scalers.cmn'
      INCLUDE 'hms_calorimeter.cmn'
      INCLUDE 'sos_calorimeter.cmn'
      INCLUDE 'hms_scin_tof.cmn'
      INCLUDE 'sos_scin_tof.cmn'
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
c
      if (c_Ntuple_max_segmentevents .gt. 0) then
       if (c_Ntuple_segmentevents .gt. c_Ntuple_max_segmentevents) then
        call c_ntuple_change(ABORT,err)
        c_Ntuple_segmentevents = 0
       else
        c_Ntuple_segmentevents = c_Ntuple_segmentevents +1
       endif
      endif
*
**********begin insert description of contents of COIN tuple ******

      m= 0
      m= m+1
      c_Ntuple_contents(m)= ccointime_hms ! Corrected Coincidence time
      m= m+1
      c_Ntuple_contents(m)= gbpm_beam_x   ! Mean Beam X Position
      m= m+1
      c_Ntuple_contents(m)= gbpm_beam_y   ! Mean Beam Y Position
      m= m+1
      c_Ntuple_contents(m)= gbpm_x(2)   ! Beam X Position bei BPM 2 (gut=1.8)
      m= m+1
      c_Ntuple_contents(m)= gbpm_y(2)   ! Beam Y Position bei BPM 2 (gut=0.0)
      m= m+1
      c_Ntuple_contents(m)= gfrx_raw_adc ! Fast Raster X
      m= m+1
      c_Ntuple_contents(m)= gfry_raw_adc ! Fast Raster Y
      m= m+1
      c_Ntuple_contents(m)= gbeam_x ! Berechnete StrahlpositionX
      m= m+1
      c_Ntuple_contents(m)= gbeam_y ! Berechnete StrahlpositionY
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
      c_Ntuple_contents(m)= HSSHSUM  !
      m= m+1
      c_Ntuple_contents(m)= HSSHTRK  !
      m= m+1
      c_Ntuple_contents(m)= HSPRTRK !
      m= m+1
      c_Ntuple_contents(m)= HBETA_NOTRK !
      m= m+1
      c_Ntuple_contents(m)= HSBETA      !
      m= m+1
      c_Ntuple_contents(m)= HSDEDX(1)   !
      m= m+1
      c_Ntuple_contents(m)= SCER_NPE_SUM ! SOS Particle Id.
      m= m+1
      c_Ntuple_contents(m)= SSSHSUM  !
      m= m+1
      c_Ntuple_contents(m)= SSSHTRK  !
      m= m+1
      c_Ntuple_contents(m)= SSPRTRK !
      m= m+1
      c_Ntuple_contents(m)= SBETA_NOTRK      !
      m= m+1
      c_Ntuple_contents(m)= SSBETA      !
      m= m+1
      c_Ntuple_contents(m)= SSDEDX(1)   !
      m= m+1
      c_Ntuple_contents(m)= gbcm1_charge ! Charge of last scaler event
      m= m+1
      c_Ntuple_contents(m)= FLOAT(gen_event_ID_number)
      m= m+1
      c_Ntuple_contents(m)= cmissing_e  ! missing energy
      m= m+1
      c_Ntuple_contents(m)= cmissing_mass ! missing mass
      m= m+1
      c_Ntuple_contents(m)= cmissing_mom ! Missing Momentum
      m= m+1
      c_Ntuple_contents(m)= cmissing_mom_par ! pm parallel to q
      m= m+1
      c_Ntuple_contents(m)= cmissing_mom_perp ! pm perp tp q
      m= m+1
      c_Ntuple_contents(m)= cmissing_mom_oop ! pm out of plane
      m= m+1
      c_Ntuple_contents(m)= cthetapq
      m= m+1
      c_Ntuple_contents(m)= cphipq

*      m= m+1
*      c_Ntuple_contents(m)= P_HMS_CORR  ! Corrected hms singles
*      m= m+1
*      c_Ntuple_contents(m)= P_SOS_CORR  ! Corrected sos singles
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
