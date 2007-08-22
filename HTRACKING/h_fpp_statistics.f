      SUBROUTINE h_fpp_statistics(ABORT,err)
*--------------------------------------------------------
*    Hall C  HMS Focal Plane Polarimeter Code
*
*  Purpose: statistical studies of FPP portion of HMS event
* 
*  Created by Frank R. Wesselmann,  February 2004
*
*--------------------------------------------------------

      IMPLICIT NONE

      INCLUDE 'hms_data_structures.cmn'
      INCLUDE 'hms_geometry.cmn'
      INCLUDE 'hms_statistics.cmn'
      include 'gen_decode_common.cmn'
      INCLUDE 'hms_fpp_event.cmn'
      INCLUDE 'hms_fpp_params.cmn'

      character*16 here
      parameter (here= 'h_fpp_statistics')

      logical ABORT
      character*(*) err

      real*4 uTrack, uWire
      real*4 mindist, rdist, rtime, residual, drift
      real*4 HMStrack(4),FPPtrack(4)

      integer*4 iPlane, iSet, iCham, iLay, iClust, iTrk, iHit, iRaw, iWire, ii

      integer*4 wires(H_FPP_N_DCINSET,H_FPP_N_DCLAYERS)


      ABORT= .FALSE.
      err= ' '


** basic efficiency determinations

*     * in each layer, find which wire a track went through
      do iSet=1,H_FPP_N_DCSETS
       if (HFPP_N_tracks(iSet).gt.0) then

         do iTrk=1,HFPP_N_tracks(iSet)
          do iCham=1,H_FPP_N_DCINSET
           do iLay=1,H_FPP_N_DCLAYERS

             call h_fpp_uTrack(iSet,iCham,iLay,iTrk,uTrack)

             HFPP_stat_shouldhit(iSet,iCham,iLay,iTrk) =
     >            int(0.5 + (uTrack - HFPP_layeroffset(iSet,iCham,iLay))
     >                      / HFPP_spacing(iSet,iCham,iLay))

*            * now lets see if ANY wire within acceptable range WAS hit
*            * and find the closest one, record the distance
             mindist = H_FPP_BAD_COORD
             HFPP_stat_diddhit(iSet,iCham,iLay,iTrk) = .false.
             if (HFPP_nClusters(iSet,iCham,iLay).gt.0) then
               do iClust=1,HFPP_nClusters(iSet,iCham,iLay)
                do iHit=1,HFPP_nHitsinCluster(iSet,iCham,iLay,iClust)

                  iRaw = HFPP_Clusters(iSet,iCham,iLay,iClust,iHit)
                  iWire = HFPP_raw_wire(iRaw)

                  rdist = uTrack - iWire * HFPP_spacing(iSet,iCham,iLay)
     >                           - HFPP_layeroffset(iSet,iCham,iLay)
                  if (abs(rdist).lt.abs(mindist)) then
                    mindist = rdist
                  endif

                  if (abs(iWire-HFPP_stat_shouldhit(iSet,iCham,iLay,iTrk))
     >                   .le. HFPP_effic_dist) then
                    HFPP_stat_diddhit(iSet,iCham,iLay,iTrk) = .true.
                  endif

                enddo !iHit
               enddo !iClust
             endif

             HFPP_stat_dist2closest(iSet,iCham,iLay,iTrk) = mindist

*            * convert to plane #s for CTP
             iPlane = H_FPP_N_DCLAYERS * H_FPP_N_DCINSET * (iSet-1)
     >       	    + H_FPP_N_DCLAYERS * (iCham-1)
     >       	    + iLay

	     HFPP_stat_planeshould(iPlane,iTrk)
     >        = HFPP_stat_shouldhit(iSet,iCham,iLay,iTrk)
	     HFPP_stat_planedidd(iPlane,iTrk)
     >        = HFPP_stat_diddhit(iSet,iCham,iLay,iTrk)

           enddo !iLay
          enddo !iCham
         enddo !iTrk

       endif
      enddo !iSet


*     * for external analysis, we need the wire number of hits, not the cluster
*     * reduce clusters to just one hit (wire) -- pick shortest drift time
      do iSet=1,H_FPP_N_DCSETS
       if (HFPP_N_tracks(iSet).ge.1) then
        do iTrk=1,HFPP_N_tracks(iSet)
         do iCham=1,H_FPP_N_DCINSET
          do iLay=1,H_FPP_N_DCLAYERS

            iClust = HFPP_TrackCluster(iSet,iCham,iLay,iTrk)
	    ii = 0
	    residual = H_FPP_BAD_DRIFT
	    if (iClust.gt.0) then

              rtime = H_FPP_BAD_TIME
	      if (HFPP_nHitsinCluster(iSet,iCham,iLay,iClust).gt.0) then
	       do iHit = 1,HFPP_nHitsinCluster(iSet,iCham,iLay,iClust)
	         iRaw = HFPP_Clusters(iSet,iCham,iLay,iClust,iHit)
	         if (HFPP_HitTime(iRaw).lt.rtime) then
	           ii = iRaw
		   rtime = HFPP_HitTime(iRaw)
	         endif
	       enddo !iHit
	      endif

*             * now also figure the residual of this hit
              iWire = HFPP_raw_wire(ii)
              uWire = HFPP_layeroffset(iSet,iCham,iLay)
     >              + HFPP_spacing(iSet,iCham,iLay)*iWire
              drift = HFPP_drift_dist(iSet,iCham,iLay,iWire)
              call h_fpp_uTrack(iSet,iCham,iLay,iTrk,uTrack)
              residual = uTrack - (uWire + drift)

	    endif !iClust
	    HFPP_TrackHit(iSet,iCham,iLay,iTrk) = ii
	    HFPP_track_residual(iSet,iCham,iLay,iTrk) = residual

	  enddo !iLay
         enddo !iCham
        enddo !iTrk
       endif
      enddo !iSet


      RETURN
      END
