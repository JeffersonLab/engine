      SUBROUTINE C_PHYSICS(ABORT,err)
*--------------------------------------------------------
*-
*-   Purpose and Methods : Compute coincident quantities
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
* $Log$
* Revision 1.7.2.4  2003/07/15 19:05:20  cdaq
* use h(s)inplane in physics calcs
*
* Revision 1.7.2.3  2003/07/15 12:05:29  cdaq
* sign change to ctphix (fpi2)
*
* Revision 1.7.2.2  2003/07/03 14:06:09  cdaq
* update for fpi-2 (xu)
*
* Revision 1.7.4.1  2003/03/05 21:01:27  xu
*  new fpi version
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
*     xucc added begin
      real*4 mp,mp2,mn,mn2,md,md2,mpi0,mpi02,meta,meta2,mpi,mpi2
      real*4 deg_rad,amu,alpha,c_pi
      parameter (mp   = 0.93827231)           ! all masses in GeV/c^2
      parameter (mp2  = mp*mp)
      parameter (mn   = 0.93956563)
      parameter (mn2  = mn*mn)
      parameter (md   = 1.87561339)
      parameter (md2  = md*md)
      parameter (mpi0 = 0.1349764)
      parameter (mpi02  = mpi0*mpi0)
      parameter (meta = 0.54745)
      parameter (meta2  = meta*meta)
      parameter (deg_rad = 3.1415927/180.)
      parameter (mpi = 0.13956995)
      parameter (mpi2 = mpi*mpi)
      parameter (amu = 0.93149432)     
      parameter (alpha = 0.00072973531)
      parameter (c_pi = 3.1415926536)
*     It's natural for Volmer to intruduce these for later calculation
*     xucc added end

*
*     local variables
*
      real*4 cqx,cqy,cqz,cqabs  ! q_vec and |q|
*      real*4 ekinrec,m_rec 
*  xucc explaination on ekinrec and m_rec
*  In cvs code  ekinrec is defined as recoil energy 
*  and m_rec is recoiled mass  
*  It seems that ekinrec is not important for Volmer, so
*  we remove it
*  also m_rec is defined by volmer else where below

      real*4 tar_amin1
      real*4 offset_ctime
*     xucc added beign
*   hey, all these dummy variables!
      real*4 dummy,dot
      real*4 p_lab_x,p_lab_y,p_lab_z 
      real*4 missingmass2,mmx2,m_rec
      real*4 targmass           ! this target mass is the mass of
*                               ! the nucleon the pion recoils against
      real*4 new_x_x,new_x_y,new_x_z,new_y_x,new_y_y,new_y_z
      real*4 new_z_x,new_z_y,new_z_z,p_new_x,p_new_y,p_new_z
      real*4 p_e,p_h,energy_e,energy_h,theta_e,theta_h
      real*4 phi_e,phi_h,xp_e_tar,xp_h_tar,m_hadron,sign_hadron
      real*4 cqxzabs,phx,phy,phz
      logical deuterium
  
*     xucc added end

      ABORT = .FALSE.
      err = ' '

      if(HSNUM_FPTRACK.le.0.or.SSNUM_FPTRACK.le.0
     ^.or.scer_npe_sum.lt.0.2) then
        return
      endif





*   xucc added begin
*   the following is the great works of Mr. Volmer
*   it will be the source of some new explanation
*   of new parameters in c_ntuple_keep.f
*  and perhaps other things as well.

* INITIALIZATION

* there are currently four types of reactions covered:
* a)  1H(e,e'p)
* b)  1H(e,e'pi+)n
* c)  2H(e,e'pi+)n n_s
* d)  2H(e,e'pi-)p p_s
* the variables that take these reactions into account are the sign
* of the hadron (+/+/+/-), the mass of the recoiling particle
* (0/n/n/p), the binding energy (0/0/2.2MeV/2.2MeV)
* At this moment, the recoil mass and the binding energy only take
* effect in c_ntuple_keep.f, where the variable eexc is defined as
* missing mass - recoil mass - binding energy

c      targmass    = gtarg_mass(gtarg_num)*amu      ! target mass
c      if (abs(targmass-mp).lt.0.01) targmass = mp  ! vanity rules: get a
c      if (abs(targmass-md).lt.0.01) targmass = md  ! couple more digits
      targmass    = 0.                             ! target nucleon mass
      sign_hadron = 0.                             ! pi+/pi-/p
      m_rec       = 0.                             ! recoil mass
      c_e_bind    = 0.                             ! binding energy
      cmex        = 0.
      cmmx        = 0.
      ce_excx     = 0.

      if ((gtarg_num.eq.15).or.(gtarg_num.eq.16)) then
         c_e_bind=0.00222445
         deuterium = .true.
      endif

c      write(6,*)'c_phys: at 2'

* DEFINE, WHICH SPECTROMETER IS ELECTRON ARM AND DEFINE THE ELECTRON AND
* HADRON VARIABLES ACCORDINGLY. ALSO CHECK THAT THE OTHER ARM IS NOT SET
* FOR ELECTRONS.
* SINCE WE WILL USE THE LAB COORDINATE SYSTEM, WITH X-AXIS TO THE RIGHT,
* THETA OF SOS PARTICLE IS TAKEN NEGATIVE

      if ((hpartmass.lt.2*mass_electron) .and.
     1     (spartmass.gt.2*mass_electron)) then ! HMS electron arm
         p_e         = hsp
         p_h         = ssp
         energy_e    = hsenergy
         energy_h    = ssenergy
         xp_e_tar    = hsxp_tar + hphicentral_offset
         xp_h_tar    = ssxp_tar + sphicentral_offset
         theta_e     = hsinplane
         theta_h     = -ssinplane
         phi_e       = hsphi
         phi_h       = ssphi
         m_hadron    = spartmass
         sign_hadron = sbfield 

      else if ((spartmass.lt.2*mass_electron) .and.
     1     (hpartmass.gt.2*mass_electron)) then ! SOS electron arm
         p_e         = ssp
         p_h         = hsp
         energy_e    = ssenergy
         energy_h    = hsenergy
         xp_e_tar    = ssxp_tar + sphicentral_offset
         xp_h_tar    = hsxp_tar + hphicentral_offset
         theta_e     = -ssinplane
         theta_h     = hsinplane
         phi_e       = ssphi
         phi_h       = hsphi
         m_hadron    = hpartmass
         sign_hadron = hbfield

      else
c         write(6,*) 'c_physics: no electron arm'
c         return
      endif

c        if(m_hadron.lt.0.8) then
c        write(6,*) "hadron mass=",m_hadron       
c        endif

* CHECK FOR PION PRODUCTION OR ELASTIC SCATTERING. IF PION PRODUCTION, SET
* RECOIL PARTICLE MASS TO NEUTRON AS DEFAULT, OPTIONAL CHANGING PION
CHARGE
* SIGN AND RECOIL MASS BY SUPPLYING THE HADRON ARM MAGNETIC FIELD SIGN IN
* THE DBASE/*.kinematics FILE.

* targmass is actually the mass of the target nucleus in (quasi)free 
* scattering, therefore p for pi+, n for pi-

*      write(6,*) "ssp=",ssp
      if(abs(m_hadron-mpi).lt.0.01) then
         m_hadron = mpi
         if (sign_hadron.ge.0.) then
            targmass=mp
            m_rec=mn
c         else if ((sign_hadron.lt.0.).and.deuterium) then
         else if (sign_hadron.le.0.) then
            targmass=mn
            m_rec=mp
         else
            write(6,*) 'c_physics: return at prod/elast'
            return
         endif
      endif                                
      if (abs(m_hadron-mp) .lt. 0.01) then
        m_hadron = mp
        targmass = mp
        m_rec=mp   ! ???? should it be 0.0? xucc June 22, 2003
c         m_rec=mpi
      endif

c       if(m_hadron.lt.1.8) then
c       write(6,*) "hadron mass=",m_hadron  
c       endif

* CALCULATE q, Pm, omega, Em, Q^2, W, epsilon, Gamma_v 
* MOMENTA ARE IN THE LAB SYSTEM WITH
* z along the beam, x to the right, y down) 

      cqx = -p_e*cos(xp_e_tar)*sin(theta_e)
      cqy = -p_e*sin(xp_e_tar)
      cqz = gpbeam - p_e*cos(xp_e_tar)*cos(theta_e)
      cqabs= sqrt(cqx**2+cqy**2+cqz**2)
      cqxzabs= sqrt(cqx**2+cqz**2)
      
      phx =  p_h*cos(xp_h_tar)*sin(theta_h)
      phy =  p_h*sin(xp_h_tar)
      phz =  p_h*cos(xp_h_tar)*cos(theta_h)

      cmissing_momx = cqx - phx 
      cmissing_momy = cqy - phy
      cmissing_momz = cqz - phz
      cmissing_mom  = sqrt(cmissing_momx**2 + cmissing_momy**2 
     >       + cmissing_momz**2)

*jv safety-if question
      if(cqxzabs.eq.0.) write(6,*) 'c_physics.f: cqxzabs=0.'
      cmissing_mom_par  = (cmissing_momx*cqx+cmissing_momz*cqz)/cqxzabs
      cmissing_mom_perp = (cmissing_momz*cqx-cmissing_momx*cqz)/cqxzabs
      if (theta_e.gt.0) cmissing_mom_perp=-cmissing_mom_perp

c   we want pm_perp > 0 when pm closer to downstream beam than q
c   effectively the sign introduced by the if statement is qx/abs(qx)
c   in this way pm_perp equals pm_x', where the primed coordinate system
c   which is used to calculate phi_pi will be defined later, in case q
c   lies in the x-z plane (qy=0)

      cmissing_mom_oop  =   cmissing_momy
      
c      ekinrec = sqrt(cmissing_mom**2 + m_rec**2)-m_rec

      c_omega = gebeam-energy_e
      cmissing_e= c_omega + targmass - sqrt(p_h**2+m_hadron**2) !miss'n energy
      if (deuterium) cmex=c_omega+md-sqrt(p_h**2+m_hadron**2)

c        write(6,*)'c_phys: at 3'

      c_bigq2 = cqabs*cqabs - c_omega*c_omega             !Q^2
      c_w2 = targmass**2 + 2*targmass*c_omega - c_bigq2

c      write(6,*)'p_e=',p_e

      if (c_W2.gt.0.) then
         c_invmass = sqrt(c_w2)                             !W = inv mass
      elseif (c_W2.eq.0.) then
         c_invmass = 0.
      else
         c_invmass = 1e-6
*           write(6,*)'c_physics: Got negative W2!'
      endif

*jv safety-if question
c      write(6,*) 'c_physics: at 4'

      if((1.+2.*cqabs**2/c_bigq2*(tan(theta_e/2.))**2).eq.0.) write(6,*) 
     >     'c_physics: epsilon problem'

      c_epsilon=1./(1.+2.*cqabs**2/c_bigq2*(tan(theta_e/2.))**2)
      c_epsilon=min(c_epsilon,0.99)

       c_gamma_v=alpha/2./c_pi**2*(c_W2-targmass**2)/c_bigq2*energy_e/gebeam
     >      /(1.-c_epsilon)

c      e_pion_lab = sqrt(p_h**2 + mpi2)                    ! Pion Energy
              
* INNER PRODUCT OF q AND THE LAB HADRON MOMENTUM YIELDS ANGLE theta_pq
   
      dot = phx*cqx + phy*cqy + phz*cqz
      if((cqabs*p_h).eq.0.) write(6,*) 'c_physics: dot problem'
      if(abs(dot/(cqabs*p_h)).le.1.0) then
        cthetapq = acos(dot/(cqabs*p_h))
      else
        cthetapq = -1.
      endif
c      write(6,*) 'c_physics: at 5'

c   CALCULATE ANGLE PHI BETWEEN SCATTERING PLANE AND REACTION PLANE
c   therefore define a new system with the z axis parallel to q, and
c   the x axis inside the q-z-plane
c   z' = q, y' = q X z, x' = y' X q
c   this gives phi the way it is usually defined, i.e. phi=0 for in-plane
c   particles closer to the downstream beamline than q
c   phi=90 is above the horizontal plane when q points to the right, and
c   below the horizontal plane when q points to the left

      dummy=sqrt( (cqx**2+cqy**2) * cqabs**2 )
      if(dummy.eq.0.) write(6,*) 'c_physics: dummy x = 0'
      new_x_x = -cqx*cqz/dummy
      new_x_y = -cqy*cqz/dummy
      new_x_z = (cqx**2 + cqy**2)/dummy

      dummy   = sqrt(cqx**2 + cqy**2)
      if(dummy.eq.0.) write(6,*) 'c_physics: dummy y = 0'
      new_y_x =  cqy/dummy
      new_y_y = -cqx/dummy
      new_y_z =  0.

      if(cqabs.eq.0.) write(6,*) 'c_physics: dummy z = 0'
      new_z_x = cqx/cqabs
      new_z_y = cqy/cqabs
      new_z_z = cqz/cqabs

      p_new_x = phx*new_x_x + phy*new_x_y + phz*new_x_z
      p_new_y = phx*new_y_x + phy*new_y_y + phz*new_y_z
      p_new_z = phx*new_z_x + phy*new_z_y + phz*new_z_z

      if ((p_new_x**2+p_new_y**2).eq.0.) then
         write(6,*) 'c_physics: dummy cphipi = 0'
         cphipi = 0.
      else
         cphipi = acos(p_new_x/sqrt(p_new_x**2+p_new_y**2))
      endif
      if (p_new_y.lt.0.) then
         cphipi = 2*c_pi - cphipi
      endif
c      write(6,*) 'c_physics: at 6'
      
      
c  Mm = sqrt(Em^2 - Pm^2)

c      write(6,*) 'missine, missing_mom=', cmissing_e,cmissing_mom

      
      missingmass2 = cmissing_e**2 - cmissing_mom**2


      if (m_hadron.eq.mpi) then
         if (missingmass2.le.0) then
            cmissing_mass = -.1
         else
            cmissing_mass = sqrt(missingmass2)
         endif

         if (deuterium) then
            mmx2 = cmex**2 - cmissing_mom**2
            if (mmx2.gt.0.) then
               cmmx = sqrt(mmx2)
            else
               cmmx = -.2
            endif
         endif

         ce_exc = cmissing_mass-m_rec-c_e_bind    !??? c_e_bind here?, yes!!
         if (deuterium) ce_excx = cmmx-2.*m_rec   ! ??? c_e_bind not here?

      elseif ((m_hadron.eq.mp).and.(missingmass2.ne.0.)) then

      cmissing_mass=abs(missingmass2)/missingmass2*sqrt(abs(missingmass2))
        ce_exc = cmissing_mass

      else
        cmissing_mass = 0.
        ce_exc = 0.

      endif

c      write(6,*)'c_phys: at 7',cmissing_mass
        
*     Find t
        
c        cmin_t = c_bigq2 - m_hadron**2 + 
c     >           2*c_omega*sqrt(p_h**2+m_hadron**2) - 2*dot
c        write(6,*)'t1=',cmin_t

       cmin_t=-(mp-mn)**2+2*targmass*(sqrt(m_rec**2+cmissing_mom**2)-m_rec)
c       write(6,*)'t2=',cmin_t

c        t = (c_bigq2 - hsp*cos(theta_gp_lab))**2 +
c     >      (hsp*sin(theta_gp_lab))**2 - (omega-e_pion_lab)**2 

c        ctphix= - cmin_t * cos(cphipi)
        ctphix=   cmin_t * cos(cphipi)
        ctphiy=   cmin_t * sin(cphipi)
      
c        write(6,*)'c_phys: at 8'

* I KEPT THIS FOR FUTURE MODIFICATION IN CASE THAT WE GO OVER TO CM SYSTEM
        
c        theta_rot = -atan(cqx/cqz)
c        if(abs(cqy/cqabs).le.1.0) then 
c           phi_rot = -asin(cqy/cqabs)
c        else 
c           phi_rot = 0.0
c        endif
c        cth = cos(theta_rot)
c        sth = sin(theta_rot)
c        cph = cos(phi_rot)
c        sph = sin(phi_rot)
c        tmp_x = p_lab_x*cth + p_lab_z*sth
c        tmp_y = p_lab_y
c        tmp_z = -p_lab_x*sth + p_lab_z*cth
c        p_rot_x = tmp_x
c        p_rot_y = tmp_y*cph + tmp_z*sph
c        p_rot_z = -tmp_y*sph + tmp_z*cph

c        p_rot_mag_check = sqrt(p_rot_x**2 + p_rot_y**2 + p_rot_z**2)

c        cthetapq=-atan(p_rot_x/p_rot_z)/deg_rad
c        cphipq=asin(p_rot_y/p_rot_mag_check)/deg_rad
c        unity=p_rot_mag_check/hsp
c        write(6,*)'c_phys: at 8'

        
*     Boost to the cm system
      
c        p_cm_x = p_rot_x
c        p_cm_y = p_rot_y
c        p_cm_z = gamma*(p_rot_z - beta_cm*e_prot_lab)
c        e_prot_cm = gamma*(e_prot_lab - beta_cm*p_rot_z)
c        p_cm = sqrt(p_cm_x**2 + p_cm_y**2 + p_cm_z**2)
*        write(6,*)'c_phys: at 9'

*     Find theta/phi of proton (rel to q_vec) in the cm system.
*     Negatives are because we really want angles of
*     pi0/eta, not proton!

c        cos_theta_cm = -p_cm_z / p_cm
c        phi_cm = -atan(p_cm_y/p_cm_x)
*        write(6,*)'c_phys: at 10'
        
        if(.false.) then
*        if(.true.) then
           write(6,*)'---------------------------------------------'
           write(6,*)'c_phys: nu =',c_omega
           write(6,*)'c_phys: c_bigq2 =',c_bigq2
           write(6,*)'c_phys: c_invmass =',c_invmass
*           write(6,*)'c_phys: e_prot_lab =',e_prot_lab
           write(6,*)'c_phys: q lab comps & mag =',cqx,cqy,cqz,cqabs
           write(6,*)'c_phys: p_lab_x, y, z =',p_lab_x,p_lab_y,p_lab_z
           write(6,*)'c_phys: theta_q_lab =',atan(cqx/cqz)/deg_rad

*           write(6,*)'c_phys: theta_gp_lab ='c_theta_gp_lab
           write(6,*)'c_phys: cmissing_mass =',cmissing_mass

           write(6,*)'c_phys: t =',cmin_t
*           write(6,*)'c_phys: beta_cm, gamma =',beta_cm,gamma
*           write(6,*)'c_phys: theta_rot, phi_rot =',theta_rot,phi_rot
*           write(6,*)'c_phys: p_rot_x, y, z =',p_rot_x,p_rot_y,p_rot_z
*           write(6,*)'c_phys: p_cm_x, y, z =',p_cm_x,p_cm_y,p_cm_z
*           write(6,*)'c_phys: p_cm, e_prot_cm =',p_cm,e_prot_cm
c           write(6,*)'c_phys: cos_theta_cm =',cos_theta_cm
c           write(6,*)'c_phys: phi_cm =',phi_cm
        endif

*     bye now, Resonance boy!


* Coincidence timing.
cdjm I added the fudge factor of about 23 ns to get both scctime and
* hcctime 
* cdjm peaks near 0 nsec.

       offset_ctime = - (hstime_at_fp-hstart_time_center)
     &               + (sstime_at_fp-sstart_time_center)
     &               - hspath_cor + sspath_cor + 10. - 22.7
c      offset_ctime = - (hstime_at_fp-hstart_time_center)
c     &               + (sstime_at_fp-sstart_time_center)

cdjm I subtracted the oct97 raw ctime tdc offsets
cjv  added coincidence timing shifts from replay_1, and added 0.14881 ns
* for replay_3,
cjv  due to changed TOF calibrations
      ccointime_hms = (hmisc_dec_data(10,1)-1888)/9.46 + offset_ctime
     &                - ccointime_hms_shift + 0.14881
      ccointime_sos = (smisc_dec_data(9,1)-1980)/9.68 - offset_ctime
     &                - ccointime_sos_shift
cdjm      ccointime_hms = (hmisc_dec_data(10,1)-2450)/9.46 + offset_ctime
cdjm      ccointime_sos = (smisc_dec_data(9,1)-1570)/9.68 - offset_ctime
*      

c       write(6,*) 'ccointime_hms_shift=',ccointime_hms_shift
c       write(6,*) 'ccointime_sos_shift=',ccointime_sos_shift

*  xucc added end

* the following is for heep check events

      if(abs(spartmass).lt.0.01.and.abs(hpartmass-mp).lt.0.01)then
      offset_ctime = - (hstime_at_fp-hstart_time_center)
     &               + (sstime_at_fp-sstart_time_center)
     &               - hspath_cor + sspath_cor +42 
      ccointime_hms = (hmisc_dec_data(10,1)-2400)/9.46 + offset_ctime
      ccointime_sos = (smisc_dec_data(9,1)-1500)/9.68 - offset_ctime
      endif


  
      return
      end









