      SUBROUTINE C_PHYSICS(ABORT,err)
*--------------------------------------------------------
*-
*-   Purpose and Methods : Compute coincident quantities
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
* $Log$
* Revision 1.4  1996/01/16 21:06:55  cdaq
* (RE?) Change some definitions
*
* Revision 1.3  1995/05/22 20:50:44  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.2  1995/05/13  02:44:07  cdaq
* (SAW) Remove some debugging
*
* Revision 1.1  1995/05/11  15:17:36  cdaq
* Initial revision
*
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*9 here
      parameter (here= 'C_PHYSICS')
*
      logical ABORT
      character*(*) err
*
      include 'gen_data_structures.cmn'
      include 'hms_data_structures.cmn'
      include 'sos_data_structures.cmn'
      include 'coin_data_structures.cmn'
      include 'gen_constants.par'
*
*     local variables
*
      real*4 coshq,sinpq,cosphipq, sinphipq, costhm
      real*4 qe,qp,qtheta,qphi
      real*4 he,hp,htheta,hphi
      real*8 temp
      real*4 cqx,cqy,cqz,cqabs
      real*4 ekinrec,m_rec
      real*4 tar_amin1

*
      ABORT = .FALSE.
      err = ' '
*
c      RETURN  !*********kill this routine, to avoid numerical crashed
!                       caused by bad angle reconstruction, and errors
!                       due to having wrong particle in spectrometer
!                       (i.e. this gives errors and few crashes if
!                       have wrong particle masses in the param files.
*
      if(HSNUM_FPTRACK.le.0.or.SSNUM_FPTRACK.le.0) then
        return
      endif
c      type *,hsenergy,hsp,hstheta,hsphi
c      type *,ssenergy,hsp,sstheta,ssphi
*
c      cs = (HSENERGY+SSENERGY)**2 - hsp**2 - ssp**2 -
c     $     hsp*ssp*(cos(hstheta)*cos(sstheta) +
c     $     sin(hstheta)*sin(sstheta)*cos(hsphi-ssphi))

*
*     Need to select which arm is the hadron
*

        tar_amin1= ta_target-1.0
c        p_beam = sqrt(cebeam**2-mass_electron**2)
        m_rec = tar_amin1*m_amu
c        write(*,*)hstheta,sstheta
      if(hpartmass .lt. 2*mass_electron) then ! Less than 1 MeV, HMS is elec
        cqx = -HSP*cos(HSxp_tar)*sin(HSTHETA)
        cqy = -HSP*sin(Hsxp_tar)
        cqz = cpbeam - HSP*cos(HSxp_tar)*cos(HSTHETA)
        cqabs= sqrt(cqx**2+cqy**2+cqz**2)
        cmissing_momx = cqx + SSP*cos(SSxp_tar)*sin(SSTHETA)
        cmissing_momy = cqy - SSP*sin(SSxp_tar)
        cmissing_momz = cqz - SSP*cos(SSxp_tar)*cos(SSTHETA)
        cmissing_mom    = sqrt(cmissing_momx**2 + cmissing_momy**2 
     >                         + cmissing_momz**2)
        cmissing_mom_par = (cmissing_momx*cqx+cmissing_momz*cqz)/cqabs
        cmissing_mom_perp = (-cmissing_momz*cqx+cmissing_momx*cqz)/cqabs
        cmissing_mom_oop = cmissing_momy
        if(cmissing_mom_perp.lt.0)then
         cmissing_mom = -cmissing_mom
        endif
        OMEGA = cebeam-HSENERGY
        if(tar_amin1.ge.999. .or.tar_amin1.lt.0.001)then
         ekinrec = 0
        else
         ekinrec = sqrt(cmissing_mom**2 + m_rec**2)-m_rec
        endif      
        CMISSING_E= OMEGA -(SSENERGY - mass_nucleon) - ekinrec
        P_HMS_CORR = HSENERGY - cebeam/(1+cebeam/0.938272*(1-cos(HSTHETA)))
        P_SOS_CORR = SSP -2.*mass_nucleon*cebeam*cos(SSTHETA)/
     >     (cebeam+mass_nucleon)/(1-(cebeam*cos(SSTHETA)/
     >     (cebeam+mass_nucleon))**2) 
C        cqe = hseloss
C        cqp = hscq3
C        cqtheta = hsthetacq 
C        cqphi = hsphicq
c        he = ssenergy
c        hp = ssp                        ! Hadron momentum
c        htheta = sstheta                ! Hadron polar angle
c        hphi = ssphi                    ! Hadron Azimutal angle
c       W2 = mass_nucleon**2 +2.*mass_nucleon*(cpbeam-hsp) - cqabs**2       
      else                              ! SOS is the electron
        cqx = -SSP*cos(SSxp_tar)*sin(SSTHETA)
        cqy = -SSP*sin(SSxp_tar)
        cqz = cpbeam - SSP*cos(SSxp_tar)*cos(SSTHETA)
        cqabs= sqrt(cqx**2+cqy**2+cqz**2)
        cmissing_momx = cqx + HSP*cos(HSxp_tar)*sin(HSTHETA)
        cmissing_momy = cqy - HSP*sin(HSxp_tar)
        cmissing_momz = cqz - HSP*cos(HSxp_tar)*cos(HSTHETA)
        cmissing_mom    = sqrt(cmissing_momx**2 + cmissing_momy**2 
     >                         + cmissing_momz**2)
        cmissing_mom_par = (cmissing_momx*cqx+cmissing_momz*cqz)/cqabs
        cmissing_mom_perp = (-cmissing_momz*cqx+cmissing_momx*cqz)/cqabs
        cmissing_mom_oop = cmissing_momy
        if(cmissing_mom_perp.lt.0)then
         cmissing_mom = -cmissing_mom
        endif
        OMEGA = cebeam-SSENERGY
        if(tar_amin1.ge.999. .or.tar_amin1.lt.0.001)then
         ekinrec = 0
        else
         ekinrec = sqrt(cmissing_mom**2 + m_rec**2)-m_rec
        endif      
        CMISSING_E= OMEGA -(HSENERGY - mass_nucleon) - ekinrec
        P_SOS_CORR = SSENERGY - cebeam/(1+cebeam/0.938272*(1-cos(SSTHETA))) 
        P_HMS_CORR = HSP -2.*mass_nucleon*cebeam*cos(HSTHETA)/
     >     (cebeam+mass_nucleon)/(1-(cebeam*cos(HSTHETA)/
     >     (cebeam+mass_nucleon))**2)
c        he = sseloss
c        hp = sscq3
c        htheta = ssthetacq 
c        hphi = ssphicq
c        cqe = hsenergy
c        cqp = hsp                        ! Hadron momentum
c        cqtheta = hstheta                ! Hadron polar angle
c        cqphi = hsphi                    ! Hadron Azimutal angle
       endif
*
*     How do we define x axes about cq.  Towards beam line?  Yes!
*     Need to rotate coord system of the hadron into the cq coord system.

c      coshcq = cos(cqtheta)*cos(htheta)
c    $     +sin(cqtheta)*sin(htheta)*cos(cqphi-hphi)
c     cthetapcq = acos(coshcq)
c     sinpcq = sqrt(1-coshcq**2)

c      cosphipcq = cos(htheta)/(cos(cqtheta)*coshcq) - 1
c     sinphipcq = sin(cqtheta)*sin(htheta)
c     $     *(sin(cqphi)*cos(hphi)-cos(cqphi)*sin(hphi))
c     $     /(cos(cqphi)*coshcq)
*      print *,sinphipcq,cosphipcq
c      cphipcq = atan2(sinphipcq,cosphipcq)
c
c      cmissing_mom = sqrt(hp**2 + cqp**2 - 2*hp*cqp*coshcq)
c      costhm = (hp*coshcq - cqp)/cmissing_mom
c      cmissing_momx = cmissing_mom*sqrt(1-costhm**2)*cosphipcq
c      cmissing_momy = cmissing_mom*sqrt(1-costhm**2)*sinphipcq
c      cmissing_momz = cmissing_mom*costhm
c*
c*     Editors Note:  I object to this verkachte sign convention on general principles
c*
c      cmissing_moms = cmissing_mom
c      if(cmissing_momx.lt.0) cmissing_moms = -cmissing_moms
*
*     Missing energy
*c
c      if(tmass_recoil.gt.mass_electron) then
c        cmissing_e = cqe - (he - (TMASS_TARGET-TMASS_RECOIL))
c     $       - cmissing_mom**2/(2*TMASS_RECOIL)
c      else
c        cmissing_e = cqe - (he - (TMASS_TARGET-TMASS_RECOIL))
c      endif
*
*     Missing mass (excitation of the residual nucleus)
*
c      type *,g_beam_target_s,cs
c      temp = g_beam_target_s + cs - 2*
c     $     ((CEBEAM+TMASS_TARGET)*(CEBEAM+he-cqe)
c     $     - CPBEAM*(CPBEAM+hp*cos(htheta)-cqp*cos(cqtheta)))
c      if(temp.lt.0) then
*        type *,hsenergy,hsp,hstheta,hsphi
*        type *,ssenergy,hsp,sstheta,ssphi
*        type *,temp
c        cmissing_mass = -TMASS_RECOIL
c      else
c        cmissing_mass = sqrt(temp) - TMASS_RECOIL
c      endif
c      print *,cmissing_mass
*      
      return
      end



