      subroutine c_Ntuple_keep(ABORT,err)

* xucc comments see c_Ntuple_init.f

*----------------------------------------------------------------------
*
*     Purpose : Add entry to the COIN Ntuple
*
*     Output: ABORT      - success or failure
*           : err        - reason for failure, if any
*
*     Created: 11-Apr-1994  K.B.Beard, Hampton U.
* $Log$
* Revision 1.9.2.2  2003/07/03 14:06:08  cdaq
* update for fpi-2 (xu)
*
* Revision 1.9.4.1  2003/03/05 22:51:44  xu
* new variables
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
      INCLUDE 'sos_physics_sing.cmn'  ! for online purpose xucc june 20,2003
      INCLUDE 'hms_aero_parms.cmn'    ! for online purpose xucc june 21,2003


*
      logical HEXIST    !CERNLIB function
*
      integer m
*     xucc added begin 
      real*4 ztar_dummy,ztard1,ztard2,ddegrad
      parameter (ddegrad=0.0174533)  ! Z target information
*     xucc added end

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
*    xucc added begin
      ztard1=abs(htheta_lab)*ddegrad
      ztard2=ztard1-HSYP_TAR
      ztar_dummy=HSY_TAR*(sin(ztard1)+cos(ztard1)/tan(ztard2))
      c_Ntuple_contents(m)= ztar_dummy   
      m= m+1
*     xucc added end

      c_Ntuple_contents(m)= HSXP_TAR    ! 
      m= m+1
      c_Ntuple_contents(m)= HSYP_TAR    ! 
      m= m+1
      c_Ntuple_contents(m)= HSDELTA     !
      m= m+1
      c_Ntuple_contents(m)= SSY_TAR     ! SOS Target
      m= m+1

*     xucc added begin
      ztard1=abs(stheta_lab)*ddegrad
      ztard2=ztard1+SSYP_TAR
      ztar_dummy=SSY_TAR*(sin(ztard1)+cos(ztard1)/tan(ztard2))
      c_Ntuple_contents(m)= ztar_dummy   
      m= m+1
*     xucc added end 


      c_Ntuple_contents(m)= SSXP_TAR    ! 
      m= m+1
      c_Ntuple_contents(m)= SSYP_TAR    ! 
      m= m+1
      c_Ntuple_contents(m)= SSDELTA     !
      m= m+1
      c_Ntuple_contents(m)= HCER_NPE_SUM ! HMS Particle Id.
      m= m+1
      c_Ntuple_contents(m)= HSSHSUM  !  xucc what's wrong with this?
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
      c_Ntuple_contents(m)= SSSHSUM  ! xucc what's wrong with this again?
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
*    xucc added begin
      c_Ntuple_contents(m)= ce_exc ! Excitation Energy
      m= m+1
      c_Ntuple_contents(m)= cmex  ! missing energy
      m= m+1
      c_Ntuple_contents(m)= cmmx ! missing mass
      m= m+1
      c_Ntuple_contents(m)= ce_excx ! Excitation Energy
      m= m+1
*     xucc added end
 
      c_Ntuple_contents(m)= cmissing_mom ! Missing Momentum
      m= m+1
      c_Ntuple_contents(m)= cmissing_mom_par ! pm parallel to q
      m= m+1
      c_Ntuple_contents(m)= cmissing_mom_perp ! pm perp tp q
      m= m+1
      c_Ntuple_contents(m)= cmissing_mom_oop ! pm out of plane
      m= m+1
*    xucc added begin
      c_Ntuple_contents(m)= c_bigq2  ! q2?
      m= m+1
      c_Ntuple_contents(m)= c_invmass
      m= m+1
      c_Ntuple_contents(m)= cmin_t
      m= m+1
*    xucc added end

      c_Ntuple_contents(m)= cthetapq 
      m= m+1
*     xucc added begin
      c_Ntuple_contents(m)= cphipi ! originally cphipq 

*    ! cphipq=asin(p_rot_y/p_rot_mag_check)/deg_rad
*    ! Azimuthal angle of hadron about q, but in original code it never calculate it 
*    ! anywhere so I change it to be cphipi
*    !         cphipi = acos(p_new_x/sqrt(p_new_x**2+p_new_y**2))
*    !         It is an angel about pi particle
*    !         why is it p_new_x? p_new_y? not yet know. 

      m= m+1
      c_Ntuple_contents(m)= c_epsilon
      m= m+1
      c_Ntuple_contents(m)= c_gamma_v
*    xucc added end

* on June 21,2003, xucc added following for online purpose
      m=m+1
      c_Ntuple_contents(m)=  ssx_cal
      m=m+1
      c_ntuple_contents(m)=  ssy_cal
      m=m+1
      c_ntuple_contents(m)=  hsmass2
      m=m+1
      c_ntuple_contents(m)=  haero_pos_npe_sum
      m=m+1
      c_ntuple_contents(m)=  haero_neg_npe_sum
      m=m+1
      c_ntuple_contents(m)=  hcer_adc(1)
      m=m+1
      c_ntuple_contents(m)=  hcer_adc(2)
      m=m+1
      c_ntuple_contents(m)=  hsx_cer
      m=m+1
      c_ntuple_contents(m)=  (sspath_cor - hspath_cor)
      m=m+1
      c_ntuple_contents(m)=   sszbeam
      m=m+1
      c_ntuple_contents(m)=   hszbeam      
      m=m+1
      c_ntuple_contents(m)=   ctphix
      m=m+1
      c_ntuple_contents(m)=   ctphiy

c       write(6,*)'ssz_beam =',sszbeam
c       write(6,*)'hsmass2 =',hsmass2
* end of xucc adding on June 21,2003



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







