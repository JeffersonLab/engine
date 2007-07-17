      subroutine b_calc_physics(ABORT,err)

      implicit none
      save
      
      character*14 here
      parameter(here='b_calc_physics')

      logical ABORT
      character*(*) err
      
      include 'bigcal_data_structures.cmn'
c      include 'gen_units.par'
      include 'gen_constants.par'
      include 'gen_data_structures.cmn'

      integer i,j,k,ntrack,itrackmax,nprot,nrcs,nmid
      real E,x,y,z,t,R,L,tof,Rperp
      real xrot,zrot
      real thetadeg,thetarad,phideg,phirad
      real Sinth,Costh
      real m_e
      real mom,beta,c,eloss,gamma,log10betagamma
      real maxetot

      ABORT=.false.
      err=' '

      Sinth = BIGCAL_SINTHETA
      Costh = BIGCAL_COSTHETA
      R = BIGCAL_R_TGT
      m_e = mass_electron
      c = speed_of_light

      ntrack = 0
c$$$      nprot = 0
c$$$      nrcs = 0
c$$$      nmid = 0

      if(BIGCAL_ALL_NCLSTR.gt.0) then
         do i=1,BIGCAL_ALL_NCLSTR
            ntrack = ntrack + 1
c            nprot = nprot + 1
            x = BIGCAL_ALL_CLSTR_X(i)
            y = BIGCAL_ALL_CLSTR_Y(i)
            E = BIGCAL_ALL_CLSTR_ETOT(i)
            t = BIGCAL_ALL_CLSTR_T8MEAN(i)

c     correct every track for energy loss. BigCal is always electron arm
c     need to set up eloss params for BigCal absorber!
            xrot = x * Costh + R * Sinth
            zrot = -x * Sinth + R * Costh
            
            L = sqrt(xrot**2 + zrot**2 + y**2)
            ! all length units are cm

            thetarad = acos(zrot/L)
            thetadeg = 180./tt * thetarad

            phirad = atan2(y,xrot)
            phideg = 180./tt * phirad
 
            gamma = E / m_e
            beta = sqrt(1. - 1./gamma**2)

            log10betagamma = log(beta*gamma) / log(10.)

            if(gtarg_z(gtarg_num).gt.0) then
               call total_eloss(3,.true.,thetarad,log10betagamma,eloss)
            else 
               eloss = 0.
            endif
c     for now, set eloss to zero for monte carlo analysis!
c$$$            if(gen_bigcal_mc.ne.0) then
c$$$               eloss = 0.
c$$$            endif

            E = E + eloss

            mom = sqrt(E**2 - m_e**2) 
            beta = mom/E
            tof = L/(beta*c)

c            Rperp = L*sin(thetarad)

            BIGCAL_TRACK_THETARAD(ntrack) = thetarad
            BIGCAL_TRACK_THETADEG(ntrack) = thetadeg
            BIGCAL_TRACK_PHIRAD(ntrack) = phirad
            BIGCAL_TRACK_PHIDEG(ntrack) = phideg
            BIGCAL_TRACK_ENERGY(ntrack) = E
            BIGCAL_TRACK_TIME(ntrack) = t
            BIGCAL_TRACK_XFACE(ntrack) = xrot
            BIGCAL_TRACK_YFACE(ntrack) = y
            BIGCAL_TRACK_ZFACE(ntrack) = zrot
            BIGCAL_TRACK_PX(ntrack) = mom * sin(thetarad) * cos(phirad)
            BIGCAL_TRACK_PY(ntrack) = mom * sin(thetarad) * sin(phirad)
            BIGCAL_TRACK_PZ(ntrack) = mom * cos(thetarad)
            BIGCAL_TRACK_BETA(ntrack) = beta
            BIGCAL_TRACK_TOF(ntrack) = tof
            BIGCAL_TRACK_COIN_TIME(ntrack) = t - tof ! "vertex" time
         enddo
         bigcal_phys_ntrack = ntrack
      endif
     
      return 
      end
