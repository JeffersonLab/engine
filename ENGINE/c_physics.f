      SUBROUTINE C_PHYSICS(ABORT,err)
*--------------------------------------------------------
*-
*-   Purpose and Methods : Compute coincident quantities
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
* $Log$
* Revision 1.9.2.2  2003/12/19 19:15:55  jones
* Use ssinplane and hsinplane for calculating cqx,cqz and  ccmissing_momx,missing_momz
*
* Revision 1.9.2.1  2003/12/17 22:55:15  jones
*  update e01004
*
* Revision 1.9  2003/11/28 15:49:41  jones
* Go back to adding h_oopcentral_offset and s_oopcentral_offset to hsxp_tar and
* ssxp_tar since this in no longer done in h_physics.f and s_physics.f (MKJ)
*
* Revision 1.8  2003/09/05 21:54:44  jones
* Remove phi_offset addition to xptar. It is done in h_physics.f and s_physics.f (mkj)
*
* Revision 1.7  1996/09/04 15:31:12  saw
* (JRA) Add phi_offset to calculation of q 3 vector
*
* Revision 1.6  1996/04/29 19:13:00  saw
* (JRA) Corrections
*
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
      include 'hms_scin_tof.cmn'
      include 'sos_scin_tof.cmn'
      include 'hms_physics_sing.cmn'
      include 'sos_physics_sing.cmn'
      include 'hms_scin_parms.cmn'
      include 'sos_scin_parms.cmn'
*
*     local variables
*
      real*4 cqx,cqy,cqz,cqabs
      real*4 ekinrec,m_rec
      real*4 tar_amin1
      real*4 offset_ctime

      ABORT = .FALSE.
      err = ' '

      if(HSNUM_FPTRACK.le.0.or.SSNUM_FPTRACK.le.0) then
        return
      endif

*     Need to select which arm is the hadron
        tar_amin1= gtarg_a(gtarg_num)-1.0
        m_rec = tar_amin1*m_amu
      if(hpartmass .lt. 2*mass_electron) then ! Less than 1 MeV, HMS is elec
        cqx = -hsp*cos(hsxp_tar+h_oopcentral_offset)*sin(hsinplane)
        cqy = -hsp*sin(hsxp_tar+h_oopcentral_offset)
        cqz = gpbeam - hsp*cos(hsxp_tar+h_oopcentral_offset)*cos(hsinplane)
        cqabs= sqrt(cqx**2+cqy**2+cqz**2)
        cmissing_momx = cqx + ssp*cos(ssxp_tar+s_oopcentral_offset)*sin(ssinplane)
        cmissing_momy = cqy - ssp*sin(ssxp_tar+s_oopcentral_offset)
        cmissing_momz = cqz - ssp*cos(ssxp_tar+s_oopcentral_offset)*cos(ssinplane)
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
      else                              ! SOS is the electron
        cqx = -ssp*cos(ssxp_tar+s_oopcentral_offset)*sin(ssinplane)
        cqy = -ssp*sin(ssxp_tar+s_oopcentral_offset)
        cqz = gpbeam - ssp*cos(ssxp_tar+s_oopcentral_offset)*cos(ssinplane)
        cqabs= sqrt(cqx**2+cqy**2+cqz**2)
        cmissing_momx = cqx + hsp*cos(hsxp_tar+h_oopcentral_offset)*sin(hsinplane)
        cmissing_momy = cqy - hsp*sin(hsxp_tar+h_oopcentral_offset)
        cmissing_momz = cqz - hsp*cos(hsxp_tar+h_oopcentral_offset)*cos(hsinplane)
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
        cmissing_mass = cmissing_e*cmissing_e - cmissing_mom*cmissing_mom
        p_sos_corr = ssenergy - gebeam/(1+gebeam/0.938272*(1-cos(sstheta))) 
        p_hms_corr = hsp -2.*mass_nucleon*gebeam*cos(hstheta)/
     >       (gebeam+mass_nucleon)/(1-(gebeam*cos(hstheta)/
     >       (gebeam+mass_nucleon))**2)
c
        c_invmass = sinvmass
        c_BIGQ2 = ssbigq2
        call delta_physics(gebeam,hpartmass,ssp,ss_kpvec,hsenergy
     >    ,hs_kpvec,sinvmass,ssbigq2,C_costhcm,C_phicm)
       endif

* Coincidence timing.
      offset_ctime = - (hstime_at_fp-hstart_time_center)
     &               + (sstime_at_fp-sstart_time_center)
     &               - hspath_cor + sspath_cor + 42
      ccointime_hms = (hmisc_dec_data(10,1)-2450)/9.46 + offset_ctime
      ccointime_sos = (smisc_dec_data(9,1)-1570)/9.68 - offset_ctime
*      
      return
      end
