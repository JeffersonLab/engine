      SUBROUTINE C_PHYSICS(ABORT,err)
*--------------------------------------------------------
*-
*-   Purpose and Methods : Compute coincident quantities
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
* $Log$
* Revision 1.5  1996/01/22 15:08:02  saw
* (JRA) Adjust variable names.  Get particle properties from lookup
* tables
*
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
      real*4 costhm
      real*8 temp
      real*4 cqx,cqy,cqz,cqabs
      real*4 ekinrec,m_rec
      real*4 tar_amin1

*
      ABORT = .FALSE.
      err = ' '
*
      if(HSNUM_FPTRACK.le.0.or.SSNUM_FPTRACK.le.0) then
        return
      endif
*
c      cs = (hsenergy+ssenergy)**2 - hsp**2 - ssp**2 -
c     $     hsp*ssp*(cos(hstheta)*cos(sstheta) +
c     $     sin(hstheta)*sin(sstheta)*cos(hsphi-ssphi))

*
*     Need to select which arm is the hadron
*

        tar_amin1= gtarg_a(gtarg_num)-1.0
c        p_beam = sqrt(gebeam**2-mass_electron**2)
        m_rec = tar_amin1*m_amu
c        write(*,*)hstheta,sstheta
      if(hpartmass .lt. 2*mass_electron) then ! Less than 1 MeV, HMS is elec
        cqx = -hsp*cos(hsxp_tar)*sin(hstheta)
        cqy = -hsp*sin(hsxp_tar)
        cqz = gpbeam - hsp*cos(hsxp_tar)*cos(hstheta)
        cqabs= sqrt(cqx**2+cqy**2+cqz**2)
        cmissing_momx = cqx + ssp*cos(ssxp_tar)*sin(sstheta)
        cmissing_momy = cqy - ssp*sin(ssxp_tar)
        cmissing_momz = cqz - ssp*cos(ssxp_tar)*cos(sstheta)
        cmissing_mom    = sqrt(cmissing_momx**2 + cmissing_momy**2 
     >                         + cmissing_momz**2)
        cmissing_mom_par = (cmissing_momx*cqx+cmissing_momz*cqz)/cqabs
        cmissing_mom_perp = (-cmissing_momz*cqx+cmissing_momx*cqz)/cqabs
        cmissing_mom_oop = cmissing_momy
        if(cmissing_mom_perp.lt.0)then
         cmissing_mom = -cmissing_mom
        endif
        omega = gebeam-hsenergy
        if(tar_amin1.ge.999. .or.tar_amin1.lt.0.001)then
         ekinrec = 0
        else
         ekinrec = sqrt(cmissing_mom**2 + m_rec**2)-m_rec
        endif      
        cmissing_e= omega -(ssenergy - mass_nucleon) - ekinrec
        p_hms_corr = hsenergy - gebeam/(1+gebeam/0.938272*(1-cos(hstheta)))
        p_sos_corr = ssp -2.*mass_nucleon*gebeam*cos(sstheta)/
     >     (gebeam+mass_nucleon)/(1-(gebeam*cos(sstheta)/
     >     (gebeam+mass_nucleon))**2) 
C        cqe = hseloss
C        cqp = hscq3
C        cqtheta = hsthetacq 
C        cqphi = hsphicq
c        he = ssenergy
c        hp = ssp                        ! Hadron momentum
c        htheta = sstheta                ! Hadron polar angle
c        hphi = ssphi                    ! Hadron Azimutal angle
c       W2 = mass_nucleon**2 +2.*mass_nucleon*(gpbeam-hsp) - cqabs**2       
      else                              ! SOS is the electron
        cqx = -ssp*cos(ssxp_tar)*sin(sstheta)
        cqy = -ssp*sin(ssxp_tar)
        cqz = gpbeam - ssp*cos(ssxp_tar)*cos(sstheta)
        cqabs= sqrt(cqx**2+cqy**2+cqz**2)
        cmissing_momx = cqx + hsp*cos(hsxp_tar)*sin(hstheta)
        cmissing_momy = cqy - hsp*sin(hsxp_tar)
        cmissing_momz = cqz - hsp*cos(hsxp_tar)*cos(hstheta)
        cmissing_mom    = sqrt(cmissing_momx**2 + cmissing_momy**2 
     >       + cmissing_momz**2)
        cmissing_mom_par = (cmissing_momx*cqx+cmissing_momz*cqz)/cqabs
        cmissing_mom_perp = (-cmissing_momz*cqx+cmissing_momx*cqz)/cqabs
        cmissing_mom_oop = cmissing_momy
        if(cmissing_mom_perp.lt.0)then
         cmissing_mom = -cmissing_mom
        endif
        omega = gebeam-ssenergy
        if(tar_amin1.ge.999. .or.tar_amin1.lt.0.001)then
         ekinrec = 0
        else
         ekinrec = sqrt(cmissing_mom**2 + m_rec**2)-m_rec
        endif      
        cmissing_e= omega -(hsenergy - mass_nucleon) - ekinrec
        p_sos_corr = ssenergy - gebeam/(1+gebeam/0.938272*(1-cos(sstheta))) 
        p_hms_corr = hsp -2.*mass_nucleon*gebeam*cos(hstheta)/
     >       (gebeam+mass_nucleon)/(1-(gebeam*cos(hstheta)/
     >       (gebeam+mass_nucleon))**2)
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
c        cmissing_e = cqe - (he - (gtarg_mass(gtarg_num)-tmass_recoil))
c     $       - cmissing_mom**2/(2*tmass_recoil)
c      else
c        cmissing_e = cqe - (he - (gtarg_mass(gtarg_num)-tmass_recoil))
c      endif
*
*     Missing mass (excitation of the residual nucleus)
*
c      type *,g_beam_target_s,cs
c      temp = g_beam_target_s + cs - 2*
c     $     ((GEBEAM+gtarg_mass(gtarg_num))*(GEBEAM+he-cqe)
c     $     - GPBEAM*(GPBEAM+hp*cos(htheta)-cqp*cos(cqtheta)))
c      if(temp.lt.0) then
*        type *,hsenergy,hsp,hstheta,hsphi
*        type *,ssenergy,hsp,sstheta,ssphi
*        type *,temp
c        cmissing_mass = -tmass_recoil
c      else
c        cmissing_mass = sqrt(temp) - tmass_recoil
c      endif
c      print *,cmissing_mass
*      
      return
      end



