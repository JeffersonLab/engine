      SUBROUTINE h_trans_fpp_hms(ABORT,err)
*--------------------------------------------------------
*    Hall C  HMS Focal Plane Polarimeter Code
*
*  Purpose: Fill FPP variables based on projecting HMS tracks 
*           and accumulate some relevant statistics
* 
*  Created by Edward J. Brash, September 5, 2007
*
*--------------------------------------------------------

      IMPLICIT NONE

      INCLUDE 'hms_data_structures.cmn'
      INCLUDE 'hms_geometry.cmn'
      include 'gen_detectorids.par'
      include 'gen_decode_common.cmn'
      INCLUDE 'gen_decode_F1tdc.cmn'
      INCLUDE 'hms_fpp_event.cmn'
      INCLUDE 'hms_fpp_params.cmn'

      character*11 here
      parameter (here= 'h_trans_fpp_hms')

      logical ABORT
      character*(*) err

      logical active_cluster

      integer*4 hit_pointer(H_FPP_N_PLANES,H_FPP_MAX_RAWperPLANE)

      integer*4 Ccount, hitno, rawhitidx, tdiff
      integer*4 iSet, iChamber, iLayer, iPlane, iWire, iHit, iCluster, ROC
      real*4 zlocal,xlocal,ylocal,uCoord,uCoord_wire,drift_dist_local
      integer*4 iWireHit,iSetScatter,i,j
      real*4 hit1time(H_FPP_MAX_WIRES), hit2time(H_FPP_MAX_WIRES)
      real*4 xsmear,ysmear
      real*8 grnd,zScatter,rnum,t1,t2,t3,t4,t5,t6,poisson,mu,mu2,x,ga
      real*8 xp_local,yp_local,xhms,yhms,deltaz,phi_local,theta_local
      real*8 r_in(3),r_fin(3),M(3,3),magnitude,alpha,beta,xin,yin,zin
      
      ABORT= .FALSE.
      err= ' '
      
      print *,'\n You should not be here!!!\n'
      STOP

c      write(*,*)'Basic Track information: ',hsxp_fp,hsx_fp, hsyp_fp,hsy_fp
      
      HFPP_raw_tot_hits=0

c
c decide where the scattering is going to occur - analyzer 1 or 2
c
      rnum=grnd()
      if (rnum.le.0.500) then ! first analyzer
        iSetScatter = 1
        zScatter =
     >    hdc_zpos(12)+2.0+rnum*((HFPP_Zoff(1)+
c     >    hdc_zpos(12)+2.0+0.5*((HFPP_Zoff(1)+
     >    HFPP_layerZ(1,1,1)-2.0)-(hdc_zpos(12)+2.0)) 
      elseif (rnum.le.1.000) then ! second analyzer
        iSetScatter = 2
        zScatter =
     >    (HFPP_Zoff(1)+HFPP_layerZ(1,2,3)+2.0)+rnum*((HFPP_Zoff(2)+
c     >    (HFPP_Zoff(1)+HFPP_layerZ(1,2,3)+2.0)+0.5*((HFPP_Zoff(2)+
     >    HFPP_layerZ(2,1,1)-2.0)-(HFPP_Zoff(1)+HFPP_layerZ(1,2,3)+2.0))
      else ! no scattering
        iSetScatter = 0 
      endif
c
c choose scattering angle from randomized double poisson distribution
c
      mu=2.0
      mu2=10.0
100   rnum=grnd()
      x=30*rnum
      CALL GAMMA(X,GA)
      t1=exp(-1.0*mu)
      t2=mu**x
      t3=x*ga
      t4=exp(-1.0*mu2)
      t5=mu2**x
      t6=x*ga
      poisson = (3.0*t1*t2/t3+t4*t5/t6)/4.0
      rnum=grnd()
      if(rnum.gt.poisson) goto 100
      
      theta_local=x*3.14159265/180.0
      rnum=grnd()
      phi_local=rnum*2.0*3.14159265
      xp_local=sin(theta_local)*cos(phi_local)/cos(theta_local)
      yp_local=sin(theta_local)*sin(phi_local)/cos(theta_local)

      beta = datan(dble(hsyp_fp))
      alpha = datan(dble(hsxp_fp)*dcos(beta))

      M(1,1) = dcos(alpha)
      M(1,2) = -1.d0*dsin(alpha)*dsin(beta)
      M(1,3) = dsin(alpha)*dcos(beta)

      M(2,1) = 0.d0
      M(2,2) = dcos(beta)
      M(2,3) = dsin(beta)

      M(3,1) = -1.d0*dsin(alpha)
      M(3,2) = -1.d0*dcos(alpha)*dsin(beta)
      M(3,3) = dcos(alpha)*dcos(beta)

c   Normalize new direction vector

      xin = dble(xp_local)
      yin = dble(yp_local)
      zin = 1.d0
      magnitude = dsqrt(xin*xin+yin*yin+zin*zin)
      r_in(1)=xin/magnitude
      r_in(2)=yin/magnitude
      r_in(3)=zin/magnitude

      do i=1,3
        r_fin(i)=0.d0
        do j=1,3
          r_fin(i)=r_fin(i)+M(i,j)*r_in(j)
        enddo
      enddo

c      write(*,*)r_fin(1),r_fin(2),r_fin(3)

      xp_local = r_fin(1)/r_fin(3)
      yp_local = r_fin(2)/r_fin(3)

c      write(*,*)'xp,yp final: ',xp_local,yp_local
      
      xhms=hsx_fp+hsxp_fp*zScatter
      yhms=hsy_fp+hsyp_fp*zScatter
                
c
c      write(*,*)'Z-positions:'
c      write(*,*)'hdc: ',(hdc_zpos(12)+2.0)
c      write(*,*)'fpp1-front: ',(HFPP_Zoff(1)+HFPP_layerZ(1,1,1)-2.0)
c      write(*,*)'fpp1-rear: ',(HFPP_Zoff(1)+HFPP_layerZ(1,2,3)+2.0)
c      write(*,*)'fpp2-front: ',(HFPP_Zoff(2)+HFPP_layerZ(2,1,1)-2.0)
c      write(*,*)'scattering:',iSetScatter,theta_local*180.0/3.14159,phi_local*180.0/3.14159
c      write(*,*)'x,y,z scatter: ',xhms,yhms,zScatter
c
c now, loop over both sets
c
c      write(*,*)'HMS: ',hsxp_fp,hsyp_fp
      do iSet=1, H_FPP_N_DCSETS

        HFPP_Nlayershit_set(iSet) = 0

        do iChamber=1, H_FPP_N_DCINSET
         do iLayer=1, H_FPP_N_DCLAYERS

           iPlane = H_FPP_N_DCLAYERS * H_FPP_N_DCINSET * (iSet-1)
     >            + H_FPP_N_DCLAYERS * (iChamber-1)
     >            + iLayer

           HFPP_raw_tot_hits=HFPP_raw_tot_hits+1
           
           do iWire=1,HFPP_Nwires(iPlane)
	     HFPP_hit1idx(iPlane,iWire) = 0
	     HFPP_hit2idx(iPlane,iWire) = 0
	   enddo
c
c Now, we want to calculate a position where track hits the chambers
c
           if (iSetScatter.eq.1) then!scattering in first chamber
                zlocal=HFPP_layerZ(iSet,iChamber,iLayer) + HFPP_Zoff(iSet)
                deltaz=zlocal-zScatter
                xlocal=xhms+xp_local*deltaz
                ylocal=yhms+yp_local*deltaz
           elseif (iSetScatter.eq.2) then
                if (iSet.eq.1) then
                    zlocal=HFPP_layerZ(iSet,iChamber,iLayer) + HFPP_Zoff(iSet)
                    xlocal=hsx_fp+hsxp_fp*zlocal
                    ylocal=hsy_fp+hsyp_fp*zlocal
                else
                    zlocal=HFPP_layerZ(iSet,iChamber,iLayer) + HFPP_Zoff(iSet)
                    deltaz=zlocal-zScatter
                    xlocal=xhms+xp_local*deltaz
                    ylocal=yhms+yp_local*deltaz
                endif                    
           else
                zlocal=HFPP_layerZ(iSet,iChamber,iLayer) + HFPP_Zoff(iSet)
                xlocal=hsx_fp+hsxp_fp*zlocal
                ylocal=hsy_fp+hsyp_fp*zlocal
           endif   

c           write(*,*)'Xlocal,Ylocal,Zlocal: ',xlocal,ylocal,zlocal
           
c
c Choose smearing of drift distance ...
c                
c           xsmear=0.0
           xsmear=-0.02+.04*grnd()
           
           uCoord = HFPP_direction(iSet,iChamber,iLayer,1)*xlocal+
     >               HFPP_direction(iSet,iChamber,iLayer,2)*ylocal

           iWireHit = int(0.5 + (uCoord - HFPP_layeroffset(iSet,
     >       iChamber,iLayer))/HFPP_spacing(iSet,iChamber,iLayer))

           uCoord_wire = iWireHit*HFPP_spacing(iSet,iChamber,iLayer)
     >     + HFPP_layeroffset(iSet,iChamber,iLayer)
c           write(*,*)'U-Coords = ',uCoord,uCoord_wire     
           drift_dist_local = uCoord-uCoord_wire+xsmear
c           write(*,*)'Drift distance =',drift_dist_local
           if(iWireHit.le.0.or.iWireHit.gt.HFPP_Nwires(iPlane)) then
                HFPP_raw_tot_hits=HFPP_raw_tot_hits-1
                goto 1234
           endif   
           HFPP_drift_dist(iSet,iChamber,iLayer,iWireHit)=drift_dist_local
c           write(*,*)'Slopes,dist,Wire ='
c     > ,hsxp_fp,hsyp_fp,HFPP_drift_dist(iSet,iChamber,iLayer,iWireHit),iWireHit      
c           write(*,*)'Wire Number = ',iWireHit

           HFPP_raw_plane(HFPP_raw_tot_hits)=iPlane
           HFPP_raw_wire(HFPP_raw_tot_hits)=iWireHit
           HFPP_N_planehitsraw(iPlane)=1
           HFPP_N_planehits(iPlane)=1
           HFPP_Nlayershit_set(iSet) = HFPP_Nlayershit_set(iSet)+1
	   HFPP_hit1idx(iPlane,iWireHit) = HFPP_raw_tot_hits
c           write(*,*)'Raw hits: ',iSet,iChamber,iLayer,iPlane,iWireHit,HFPP_raw_tot_hits
           
1234       continue 
         enddo ! iLayer
        enddo ! iChamber
      enddo ! iSet
 
*     * check if we have any work to do
      if (HFPP_raw_tot_hits .le. 0) RETURN


*     * now turn raw hits per plane into CLUSTERS per (set,chamber,layer)
*     * if clustering is not desired (HFPP_use_clusters), each cluster has 1 hit only
      do iSet=1, H_FPP_N_DCSETS
       if (HFPP_Nlayershit_set(iSet).ge.HFPP_minsethits-5) then ! enough hits for tracking

        do iChamber=1, H_FPP_N_DCINSET
         do iLayer=1, H_FPP_N_DCLAYERS

           iPlane = H_FPP_N_DCLAYERS * H_FPP_N_DCINSET * (iSet-1)
     >            + H_FPP_N_DCLAYERS * (iChamber-1)
     >            + iLayer

           active_cluster = .false.
	   iCluster = 0

	   do iWire=1,HFPP_Nwires(iPlane)

	     if (HFPP_hit1idx(iPlane,iWire).eq.0) then	! terminate any active cluster
	        active_cluster = .false.
	     else
c                write(*,*)'Active cluster: ',iPlane,iWire,HFPP_hit1idx(iPlane,iWire)
	        if (active_cluster) then	! add to active cluster
                  Ccount = HFPP_nHitsinCluster(iSet,iChamber,iLayer,iCluster) + 1
	        else	!start new cluster
	          active_cluster = (HFPP_use_clusters.gt.0)  ! only make clusters if instructed
	          iCluster = min(iCluster+1,H_FPP_MAX_CLUSTERS)
	          HFPP_ClusterinTrack(iSet,iChamber,iLayer,iCluster) = 0
	          Ccount = 1
	        endif
                if (Ccount.le.H_FPP_MAX_HITSperCLUSTER) then
*                 * we can only have so many hits in a cluster -- skip excess
                  HFPP_nHitsinCluster(iSet,iChamber,iLayer,iCluster) = Ccount
                  HFPP_Clusters(iSet,iChamber,iLayer,iCluster,Ccount)
     >          				 = HFPP_hit1idx(iPlane,iWire)
                endif
	     endif !HFPP_hit1idx.eq.0

	   enddo !iWire

           HFPP_nClusters(iSet,iChamber,iLayer) = iCluster
           HFPP_NplaneClusters(iPlane) = iCluster   !for CTP usage -- max 2d array

c           write(*,*)"clustering:"
c           write(*,*)iSet,iChamber,iLayer,iPlane,HFPP_nClusters(iSet,iChamber,iLayer)
         enddo !iLayer
        enddo !iChamber

       endif !Nplanes_hit
      enddo !iSet


      RETURN
      END

        SUBROUTINE GAMMA(X,GA)
C
C       ==================================================
C       Purpose: Compute the gamma function ג(x)
C       Input :  x  --- Argument of ג(x)
C                       ( x is not equal to 0,-1,-2,תתת )
C       Output:  GA --- ג(x)
C       ==================================================
C
        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
        DIMENSION G(26)
        PI=3.141592653589793D0
        IF (X.EQ.INT(X)) THEN
           IF (X.GT.0.0D0) THEN
              GA=1.0D0
              M1=X-1
              DO 10 K=2,M1
10               GA=GA*K
           ELSE
              GA=1.0D+300
           ENDIF
        ELSE
           IF (DABS(X).GT.1.0D0) THEN
              Z=DABS(X)
              M=INT(Z)
              R=1.0D0
              DO 15 K=1,M
15               R=R*(Z-K)
              Z=Z-M
           ELSE
              Z=X
           ENDIF
           DATA G/1.0D0,0.5772156649015329D0,
     &          -0.6558780715202538D0, -0.420026350340952D-1,
     &          0.1665386113822915D0,-.421977345555443D-1,
     &          -.96219715278770D-2, .72189432466630D-2,
     &          -.11651675918591D-2, -.2152416741149D-3,
     &          .1280502823882D-3, -.201348547807D-4,
     &          -.12504934821D-5, .11330272320D-5,
     &          -.2056338417D-6, .61160950D-8,
     &          .50020075D-8, -.11812746D-8,
     &          .1043427D-9, .77823D-11,
     &          -.36968D-11, .51D-12,
     &          -.206D-13, -.54D-14, .14D-14, .1D-15/
           GR=G(26)
           DO 20 K=25,1,-1
20            GR=GR*Z+G(K)
           GA=1.0D0/(GR*Z)
           IF (DABS(X).GT.1.0D0) THEN
              GA=GA*R
              IF (X.LT.0.0D0) GA=-PI/(X*GA*DSIN(PI*X))
           ENDIF
        ENDIF
        RETURN
        end
