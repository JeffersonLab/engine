      SUBROUTINE C_PHYSICS(ABORT,err)
*--------------------------------------------------------
*-
*-   Purpose and Methods : Compute coincident quantities
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
* $Log$
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
*
      ABORT = .FALSE.
      err = ' '
*
      if(HSNUM_FPTRACK.le.0.or.SSNUM_FPTRACK.le.0) then
        return
      endif
c      type *,hsenergy,hsp,hstheta,hsphi
c      type *,ssenergy,hsp,sstheta,ssphi
*
      cs = (HSENERGY+SSENERGY)**2 - hsp**2 - ssp**2 -
     $     hsp*ssp*(cos(hstheta)*cos(sstheta) +
     $     sin(hstheta)*sin(sstheta)*cos(hsphi-ssphi))

*
*     Need to select which arm is the hadron
*
      if(hpartmass .lt. 2*mass_electron) then     ! Less than 1 MeV, HMS is electron
        qe = hseloss
        qp = hsq3
        qtheta = hsthetaq 
        qphi = hsphiq
        he = ssenergy
        hp = ssp                        ! Hadron momentum
        htheta = sstheta                ! Hadron polar angle
        hphi = ssphi                    ! Hadron Azimutal angle
      else                              ! SOS is the electron
        he = sseloss
        hp = ssq3
        htheta = ssthetaq 
        hphi = ssphiq
        qe = hsenergy
        qp = hsp                        ! Hadron momentum
        qtheta = hstheta                ! Hadron polar angle
        qphi = hsphi                    ! Hadron Azimutal angle
      endif
*
*     How do we define x axes about q.  Towards beam line?  Yes!
*     Need to rotate coord system of the hadron into the q coord system.

      coshq = cos(qtheta)*cos(htheta)
     $     +sin(qtheta)*sin(htheta)*cos(qphi-hphi)
      cthetapq = acos(coshq)
      sinpq = sqrt(1-coshq**2)

      cosphipq = cos(htheta)/(cos(qtheta)*coshq) - 1
      sinphipq = sin(qtheta)*sin(htheta)
     $     *(sin(qphi)*cos(hphi)-cos(qphi)*sin(hphi))
     $     /(cos(qphi)*coshq)
*      print *,sinphipq,cosphipq
      cphipq = atan2(sinphipq,cosphipq)

      cmissing_mom = sqrt(hp**2 + qp**2 - 2*hp*qp*coshq)
      costhm = (hp*coshq - qp)/cmissing_mom
      cmissing_momx = cmissing_mom*sqrt(1-costhm**2)*cosphipq
      cmissing_momy = cmissing_mom*sqrt(1-costhm**2)*sinphipq
      cmissing_momz = cmissing_mom*costhm
*
*     Editors Note:  I object to this verkachte sign convention on general principles
*
      cmissing_moms = cmissing_mom
      if(cmissing_momx.lt.0) cmissing_moms = -cmissing_moms
*
*     Missing energy
*
      if(tmass_recoil.gt.mass_electron) then
        cmissing_e = qe - (he - (TMASS_TARGET-TMASS_RECOIL))
     $       - cmissing_mom**2/(2*TMASS_RECOIL)
      else
        cmissing_e = qe - (he - (TMASS_TARGET-TMASS_RECOIL))
      endif
*
*     Missing mass (excitation of the residual nucleus)
*
c      type *,g_beam_target_s,cs
      temp = g_beam_target_s + cs - 2*
     $     ((CEBEAM+TMASS_TARGET)*(CEBEAM+he-qe)
     $     - CPBEAM*(CPBEAM+hp*cos(htheta)-qp*cos(qtheta)))
      if(temp.lt.0) then
*        type *,hsenergy,hsp,hstheta,hsphi
*        type *,ssenergy,hsp,sstheta,ssphi
*        type *,temp
        cmissing_mass = -TMASS_RECOIL
      else
        cmissing_mass = sqrt(temp) - TMASS_RECOIL
      endif
c      print *,cmissing_mass
*      
      return
      end


