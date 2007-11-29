      SUBROUTINE h_trans_fpp(ABORT,err)
*--------------------------------------------------------
*    Hall C  HMS Focal Plane Polarimeter Code
*
*  Purpose: process raw hits by layer, chamber, set
*           and accumulate some relevant statistics
* 
*  Created by Frank R. Wesselmann,  February 2004
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
      parameter (here= 'h_trans_fpp')

      logical ABORT
      character*(*) err


      logical active_cluster

      integer*4 hit_pointer(H_FPP_N_PLANES,H_FPP_MAX_RAWperPLANE)

      integer*4 Ccount, hitno, rawhitidx, tdiff
      integer*4 iSet, iChamber, iLayer, iPlane, iWire, iHit, iCluster, ROC

      real*4 hit1time(H_FPP_MAX_WIRES), hit2time(H_FPP_MAX_WIRES)


      ABORT= .FALSE.
      err= ' '


*     * check if we have any work to do
c      write(*,*)'In h_trans_fpp ... ',HFPP_raw_tot_hits
      if (HFPP_raw_tot_hits .le. 0) RETURN


*     * init storage
      do iPlane=1,H_FPP_N_PLANES
       do iWire=1,HFPP_Nwires(iPlane)
         HFPP_hit1idx(iPlane,iWire) = 0
       enddo
      enddo

      do iPlane=1,H_FPP_N_PLANES
       do iWire=1,HFPP_Nwires(iPlane)
         HFPP_hit2idx(iPlane,iWire) = 0
       enddo
      enddo

      do iSet=1, H_FPP_N_DCSETS
        HFPP_Nlayershit_set(iSet) = 0
      enddo

      do iSet=1, H_FPP_N_DCSETS
       do iChamber=1, H_FPP_N_DCINSET
        do iLayer=1, H_FPP_N_DCLAYERS
         do iCluster=1, H_FPP_MAX_CLUSTERS
           HFPP_nHitsinCluster(iSet,iChamber,iLayer,iCluster) = 0
         enddo
        enddo
       enddo
      enddo

      do iSet=1, H_FPP_N_DCSETS
       do iChamber=1, H_FPP_N_DCINSET
        do iLayer=1, H_FPP_N_DCLAYERS
          HFPP_nClusters(iSet,iChamber,iLayer) = 0
        enddo
       enddo
      enddo

      do iPlane=1,H_FPP_N_PLANES
        HFPP_NplaneClusters(iPlane) = 0
      enddo


*     * find TDC trigger time!!  skip whatever we can!
      do rawhitidx=1, HFPP_raw_tot_hits
	if (HFPP_raw_plane(rawhitidx).lt.HFPP_trigger_plane) CYCLE
	if (HFPP_raw_wire(rawhitidx) .ne.HFPP_trigger_wire)  CYCLE

        ROC = g_decode_roc(HFPP_ID,HFPP_raw_plane(rawhitidx),
     >                             HFPP_raw_wire(rawhitidx),0)
	HFPP_trigger_TDC(ROC) = HFPP_raw_TDC(rawhitidx)
      enddo !rawhitidx


*     * identify raw hits by planes -- assume unsorted raw data
      do rawhitidx=1, HFPP_raw_tot_hits

	iPlane = HFPP_raw_plane(rawhitidx)
	iWire  = HFPP_raw_wire(rawhitidx)

*       * weed out obviously bad hits and thus speed up processing
        if ((HFPP_raw_TDC(rawhitidx).ge.HFPP_minTDC) .and.
     >      (HFPP_raw_TDC(rawhitidx).le.HFPP_maxTDC)) then

	   if (iPlane.lt.1) CYCLE
	   if (iWire.lt.1) CYCLE
	   if (iPlane.gt.H_FPP_N_PLANES) CYCLE
	   if (iWire.gt.HFPP_Nwires(iPlane)) CYCLE
*          * the above cuts also weed out the already processed trigger reference time!

*          * TDC times from F1 TDC are meaningful only relative to each other
*          * thus we need to subtract the measured trigger time!
*          * also account for the case that the wire hit times
*          * may have rolled over but the trigger time did not!
           ROC = g_decode_roc(HFPP_ID,iPlane,iWire,0)

	   if (HFPP_trigger_TDC(ROC).lt.0) then  ! missing trigger time?!!
             call G_build_note(':(FPP) TDC data in ROC $ missing trigger reference!',
     &                        '$',ROC, ' ',0.,' ', err)
             call G_add_path(here,err)
	     RETURN
	   endif

*          * although we operate in COMMON STOP mode, the F1 TDCs are free-running
*          * counters, so as time passes the count value increases
*          * ignoring the overflow due to the limited counting range that means
*          * earlier events should have a smaller TDC count than later events
*          * the trigger should be the last signal and ought to have the largest
*          * value; if it does not, we have a roll-over of the trigger time
           if (HFPP_raw_TDC(rawhitidx) .gt. HFPP_trigger_TDC(ROC)) then
	     tdiff = HFPP_raw_TDC(rawhitidx) - HFPP_trigger_TDC(ROC)
     >                                    - F1TDC_WINDOW_SIZE(ROC)
	   else
	     tdiff = HFPP_raw_TDC(rawhitidx) - HFPP_trigger_TDC(ROC)
	   endif
	   HFPP_HitTime(rawhitidx) = HFPP_tDriftOffset(iPlane,iWire)
     >                          + float(tdiff) * HFPP_tdc_time_per_channel

	   hitno = HFPP_N_planehitsraw(iPlane) + 1
           if (hitno .le. H_FPP_MAX_RAWperPLANE) then
             HFPP_N_planehitsraw(iPlane) = hitno 
             hit_pointer(iPlane,hitno) = rawhitidx   ! local -- all raw hits
           endif

        else
	  print *,' NOTE: FPP hit outside accepted time window: plane,wire,TDC= ',
     >               iPlane,iWire,HFPP_raw_TDC(rawhitidx)
	endif
      enddo !rawhitidx



*     * find the earliest accpetable hit for each wire  -- assume unsorted raw data
*     * also, determine # of layers with usefull hits to see if any tracking to be done
      do iSet=1, H_FPP_N_DCSETS
       do iChamber=1, H_FPP_N_DCINSET
        do iLayer=1, H_FPP_N_DCLAYERS

          iPlane = H_FPP_N_DCLAYERS * H_FPP_N_DCINSET * (iSet-1)
     >  	 + H_FPP_N_DCLAYERS * (iChamber-1)
     >  	 + iLayer

*         * check all hits in layer
          do iHit=1, HFPP_N_planehitsraw(iPlane)
            rawhitidx = hit_pointer(iPlane,iHit)
            if (HFPP_HitTime(rawhitidx).lt.HFPP_mintime) CYCLE !skip too early hits
            if (HFPP_HitTime(rawhitidx).gt.HFPP_maxtime) CYCLE !skip too late hits

            iWire  = HFPP_raw_wire(rawhitidx)
            if (HFPP_hit1idx(iPlane,iWire).eq.0) then !first hit on wire, keep!
              HFPP_hit1idx(iPlane,iWire) = rawhitidx
              hit1time(iWire) = HFPP_HitTime(rawhitidx)
              HFPP_N_planehits(iPlane) = HFPP_N_planehits(iPlane) + 1
            elseif (HFPP_HitTime(rawhitidx).lt.hit1time(iWire)) then !replace if earlier
              HFPP_hit2idx(iPlane,iWire) = HFPP_hit1idx(iPlane,iWire)
              HFPP_hit1idx(iPlane,iWire) = rawhitidx
              hit2time(iWire) = hit1time(iWire)
              hit1time(iWire) = HFPP_HitTime(rawhitidx)
            elseif (HFPP_hit2idx(iPlane,iWire).eq.0) then !first 2nd hit on wire, keep!
              HFPP_hit2idx(iPlane,iWire) = rawhitidx
              hit2time(iWire) = HFPP_HitTime(rawhitidx)
            elseif (HFPP_HitTime(rawhitidx).lt.hit2time(iWire)) then !replace if earlier
              HFPP_hit2idx(iPlane,iWire) = rawhitidx
              hit2time(iWire) = HFPP_HitTime(rawhitidx)
            endif
          enddo

          if (HFPP_N_planehits(iPlane) .gt. 0) then
            HFPP_Nlayershit_set(iSet) = HFPP_Nlayershit_set(iSet)+1
          endif

        enddo !iLayer
       enddo !iChamber
      enddo !iSet



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

             HFPP_drift_time(iSet,iChamber,iLayer,iWire) = H_FPP_BAD_TIME
             HFPP_drift_dist(iSet,iChamber,iLayer,iWire) = H_FPP_BAD_DRIFT  !init to none

	   enddo !iWire

           HFPP_nClusters(iSet,iChamber,iLayer) = iCluster
           HFPP_NplaneClusters(iPlane) = iCluster   !for CTP usage -- max 2d array

         enddo !iLayer
        enddo !iChamber

       endif !Nplanes_hit
      enddo !iSet


      RETURN
      END
