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
      integer*4 iWireHit
      real*4 hit1time(H_FPP_MAX_WIRES), hit2time(H_FPP_MAX_WIRES)

      ABORT= .FALSE.
      err= ' '

c      write(*,*)'Basic Track information: ',hsxp_fp,hsx_fp, hsyp_fp,hsy_fp
      HFPP_raw_tot_hits=0

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

           zlocal=HFPP_layerZ(iSet,iChamber,iLayer) + HFPP_Zoff(iSet)
c	   write(*,*)'Z coordinate for plane ',iPlane,' = ',zlocal

           xlocal=hsx_fp+hsxp_fp*zlocal
           ylocal=hsy_fp+hsyp_fp*zlocal
           uCoord = HFPP_direction(iSet,iChamber,iLayer,1)*xlocal+
     >               HFPP_direction(iSet,iChamber,iLayer,2)*ylocal

           iWireHit = int(0.5 + (uCoord - HFPP_layeroffset(iSet,
     >       iChamber,iLayer))/HFPP_spacing(iSet,iChamber,iLayer))

           uCoord_wire = iWireHit*HFPP_spacing(iSet,iChamber,iLayer)
     >     + HFPP_layeroffset(iSet,iChamber,iLayer)
c           write(*,*)'U-Coords = ',uCoord,uCoord_wire     
           drift_dist_local = uCoord-uCoord_wire
c           write(*,*)'Drift distance =',drift_dist_local
           if(iWireHit.le.0)iWireHit=1
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
 
         enddo ! iLayer
        enddo ! iChamber
      enddo ! iSet
 
*     * check if we have any work to do
      if (HFPP_raw_tot_hits .le. 0) RETURN


*     * now turn raw hits per plane into CLUSTERS per (set,chamber,layer)
*     * if clustering is not desired (HFPP_use_clusters), each cluster has 1 hit only
      do iSet=1, H_FPP_N_DCSETS
       if (HFPP_Nlayershit_set(iSet).ge.HFPP_minsethits) then ! enough hits for tracking

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

         enddo !iLayer
        enddo !iChamber

       endif !Nplanes_hit
      enddo !iSet


      RETURN
      END
