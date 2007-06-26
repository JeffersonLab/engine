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

      integer i,j,k,ntrack,itrackmax,nprot,nrcs,nmid
      real E,x,y,z,t,R,L,tof,Rperp
      real xrot,zrot
      real thetadeg,thetarad,phideg,phirad
      real Sinth,Costh
      real m_e
      real mom,beta,c
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

      if(BIGCAL_PROT_NCLSTR.gt.0) then
         do i=1,BIGCAL_PROT_NCLSTR
            ntrack = ntrack + 1
c            nprot = nprot + 1
            x = BIGCAL_PROT_CLSTR_X(i)
            y = BIGCAL_PROT_CLSTR_Y(i)
            E = BIGCAL_PROT_CLSTR_ETOT(i)
            t = BIGCAL_PROT_CLSTR_T8BEST(i)

            xrot = x * Costh + R * Sinth
            zrot = -x * Sinth + R * Costh
            
            L = sqrt(xrot**2 + zrot**2 + y**2)
            ! all length units are cm
            mom = sqrt(E**2 - m_e**2) 
            beta = mom/E
            tof = L/(beta*c)

            thetarad = acos(zrot/L)
            thetadeg = 180./tt * thetarad
            
            Rperp = L*sin(thetarad)

            phirad = atan2(y,xrot)
            phideg = 180./tt * phirad

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
      endif

      if(BIGCAL_RCS_NCLSTR.gt.0) then
         do i=1,BIGCAL_RCS_NCLSTR
            ntrack = ntrack + 1
c            nrcs = nrcs + 1
            x = BIGCAL_RCS_CLSTR_X(i)
            y = BIGCAL_RCS_CLSTR_Y(i)
            E = BIGCAL_RCS_CLSTR_ETOT(i)
            t = BIGCAL_RCS_CLSTR_T8BEST(i)

            xrot = x * Costh + R * Sinth
            zrot = -x * Sinth + R * Costh
            
            L = sqrt(xrot**2 + zrot**2 + y**2)
            ! all length units are cm
            mom = sqrt(E**2 - m_e**2) 
            beta = mom/E
            tof = L/(beta*c)

            thetarad = acos(zrot/L)
            thetadeg = 180./tt * thetarad
            
            Rperp = L*sin(thetarad)

            phirad = atan2(y,xrot)
            phideg = 180./tt * phirad

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
      endif

      if(BIGCAL_MID_NCLSTR.gt.0) then
         do i=1,BIGCAL_MID_NCLSTR
            ntrack = ntrack + 1
c            nmid = nmid + 1

            x = BIGCAL_MID_CLSTR_X(i)
            y = BIGCAL_MID_CLSTR_Y(i)
            E = BIGCAL_MID_CLSTR_ETOT(i)
            t = BIGCAL_MID_CLSTR_T8BEST(i)

            xrot = x * Costh + R * Sinth
            zrot = -x * Sinth + R * Costh
            
            L = sqrt(xrot**2 + zrot**2 + y**2)
            ! all length units are cm
            mom = sqrt(E**2 - m_e**2) 
            beta = mom/E
            tof = L/(beta*c)

            thetarad = acos(zrot/L)
            thetadeg = 180./tt * thetarad
            
            Rperp = L*sin(thetarad)

            phirad = atan2(y,xrot)
            phideg = 180./tt * phirad

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
      endif

      BIGCAL_PHYS_NTRACK = ntrack

      ! for now, simply choose track with greatest energy as "best" track
      ! don't distinguish which section of the calorimeter the track comes from

      itrackmax = 0
      maxetot = 0.
      
      do i=1,ntrack
         if(BIGCAL_TRACK_ENERGY(i).gt.maxetot) then
            maxetot = BIGCAL_TRACK_ENERGY(i)
            itrackmax = i
         endif
      enddo

      nprot = BIGCAL_PROT_NCLSTR
      nrcs = BIGCAL_RCS_NCLSTR
      nmid = BIGCAL_MID_NCLSTR

      if(itrackmax.le.nprot) then
         BIGCAL_BEST_CLSTR_ISECTION = 1
         BIGCAL_BEST_CLSTR_ICLUSTER = itrackmax
      else if(itrackmax-nprot.le.nrcs) then
         BIGCAL_BEST_CLSTR_ISECTION = 2
         BIGCAL_BEST_CLSTR_ICLUSTER = itrackmax - nprot
      else if(itrackmax-nprot-nrcs.le.nmid) then
         BIGCAL_BEST_CLSTR_ISECTION = 3
         BIGCAL_BEST_CLSTR_ICLUSTER = itrackmax - nprot - nrcs
      else ! itrackmax out of range. should never happen
         BIGCAL_BEST_CLSTR_ISECTION = 0
         BIGCAL_BEST_CLSTR_ICLUSTER = 0
      endif
         
           
      BIGCAL_BEST_THETARAD = BIGCAL_TRACK_THETARAD(itrackmax)
      BIGCAL_BEST_THETADEG = BIGCAL_TRACK_THETADEG(itrackmax)
      BIGCAL_BEST_PHIRAD = BIGCAL_TRACK_PHIRAD(itrackmax)
      BIGCAL_BEST_PHIDEG = BIGCAL_TRACK_PHIDEG(itrackmax)
      BIGCAL_BEST_ENERGY = BIGCAL_TRACK_ENERGY(itrackmax)
      BIGCAL_BEST_TIME = BIGCAL_TRACK_TIME(itrackmax)
      BIGCAL_BEST_XFACE = BIGCAL_TRACK_XFACE(itrackmax)
      BIGCAL_BEST_YFACE = BIGCAL_TRACK_YFACE(itrackmax)
      BIGCAL_BEST_ZFACE = BIGCAL_TRACK_ZFACE(itrackmax)
      BIGCAL_BEST_PX = BIGCAL_TRACK_PX(itrackmax)
      BIGCAL_BEST_PY = BIGCAL_TRACK_PY(itrackmax)
      BIGCAL_BEST_PZ = BIGCAL_TRACK_PZ(itrackmax)
      BIGCAL_BEST_BETA = BIGCAL_TRACK_BETA(itrackmax)
      BIGCAL_BEST_TOF = BIGCAL_TRACK_TOF(itrackmax)
      BIGCAL_BEST_COIN_TIME = BIGCAL_TRACK_COIN_TIME(itrackmax)

      return 
      end
