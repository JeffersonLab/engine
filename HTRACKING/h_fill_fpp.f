      SUBROUTINE h_fill_fpp(ABORT,err)
*--------------------------------------------------------
*    Hall C  HMS Focal Plane Polarimeter Code
*
*  Purpose: fill FPP histograms
*           histogram IDs are from common block in file
*           hms_id_histid.cmn and assigned in h_init_histid
* 
*  Created by Frank R. Wesselmann,  February 2004
*
*--------------------------------------------------------

      IMPLICIT NONE

      INCLUDE 'hms_data_structures.cmn'
      include 'hms_scin_parms.cmn'
      include 'hms_scin_tof.cmn'
      INCLUDE 'hms_id_histid.cmn'
      include 'gen_detectorids.par'
      include 'gen_decode_common.cmn'
      INCLUDE 'hms_fpp_params.cmn'
      INCLUDE 'hms_fpp_event.cmn'
      INCLUDE 'hms_geometry.cmn'
      INCLUDE 'hms_statistics.cmn'

      character*10 here
      parameter (here= 'h_fill_fpp')

      integer*4 rad2deg
      parameter (rad2deg=57.29578)

      logical ABORT
      character*(*) err

      integer*4 DCset,iChamber,iLayer,iPlane,iWire,iHit,hit2,tdc,iTrack
      integer*4 iCluster, Nraw, iRaw,hid,hid1,hid2, iROC, ii
      real*4 dist,time, istat


      ABORT= .FALSE.
      err= ' '

*     * check if we have any work to do
      if (HFPP_raw_tot_hits .le. 0) RETURN


*     * for each ROC, histogram TDC value of trigger reference
      hid = hidFPP_tdcROC
      do ii=0,G_DECODE_MAXROCS
        iROC = HFPP_my_ROCs(ii)
	if (iROC.lt.0) EXIT  !end of list
	if(hid.gt.0) call hf2(hid,float(iROC),float(HFPP_trigger_TDC(iROC)),1.)
      enddo !ii

*     * for each plane, histogram all TDC values seen
      do iHit=1, HFPP_raw_tot_hits
        iPlane = HFPP_raw_plane(iHit)
	iWire  = HFPP_raw_wire(iHit)
        tdc = HFPP_raw_TDC(iHit)
	if (iPlane.le.H_FPP_N_PLANES) then
	  hid = hidFPP_tdc(iPlane)
	  if(hid.gt.0) call hf2(hid,float(tdc),float(iWire),1.)
	endif
      enddo

*     * for each plane, wire, histogram all hit times seen
      do iHit=1, HFPP_raw_tot_hits
        iPlane = HFPP_raw_plane(iHit)
	iWire  = HFPP_raw_wire(iHit)
	time  = HFPP_HitTime(iHit)
	if (iPlane.le.H_FPP_N_PLANES) then
	  hid = hidFPP_alltimes(iPlane)
	  if(hid.gt.0) call hf2(hid,time,float(iWire),1.)
	endif
      enddo

*     * for each plane, wire, histogram times of first hit seen
      do iPlane=1,H_FPP_N_PLANES
	hid1 = hidFPP_planetime(iPlane)
	hid2 = hidFPP_time1(iPlane)
        do iWire=1,HFPP_Nwires(iPlane)
	  iHit = HFPP_hit1idx(iPlane,iWire)
	  if (iHit.gt.0) then
	    time = HFPP_HitTime(iHit)
	    if(hid1.gt.0) call hf1(hid1,time,1.)
	    if(hid2.gt.0) call hf2(hid2,time,float(iWire),1.)
          endif
        enddo
      enddo

*     * for each plane, wire, histogram hit counts (rate)
      do iPlane=1,H_FPP_N_PLANES
	hid  = hidFPP_rate1(iPlane)
        do iWire=1,HFPP_Nwires(iPlane)
	  iHit = HFPP_hit1idx(iPlane,iWire)
	  if (iHit.gt.0) then
	    if(hid.gt.0) call hf1(hid,float(iWire),1.)  !hit rate per wire
          endif
        enddo
      enddo

*     * for each plane, wire, histogram time difference between 1st and 2nd hit seen
      do iPlane=1,H_FPP_N_PLANES
        do iWire=1,HFPP_Nwires(iPlane)
	  hit2 = HFPP_hit2idx(iPlane,iWire)
	  if (hit2.gt.0) then
	    iHit = HFPP_hit1idx(iPlane,iWire)
	    time = HFPP_HitTime(hit2) - HFPP_HitTime(iHit)
	    hid = hidFPP_time12(iPlane)
	    if(hid.gt.0) call hf2(hid,time,float(iWire),1.)
          endif
	enddo
      enddo

*     * for each plane, wire, histogram size of clusters
      do DCset=1,H_FPP_N_DCSETS
       do iChamber=1,H_FPP_N_DCINSET
        do iLayer=1,H_FPP_N_DCLAYERS
          iPlane = H_FPP_N_DCLAYERS * H_FPP_N_DCINSET * (DCset-1)
     >    	 + H_FPP_N_DCLAYERS * (iChamber-1)
     >    	 + iLayer
          hid1 = hidFPP_rawinclust(iPlane)
	  do iCluster=1,HFPP_nClusters(DCset,iChamber,iLayer)
	    Nraw = HFPP_nHitsinCluster(DCset,iChamber,iLayer,iCluster)
	    if(hid1.gt.0) call hf1(hid1,float(Nraw),1.)  !number of raw in cluster
	  enddo !iCluster
	enddo !iLayer
       enddo !iChamber
       if(hidfpp_nsimp(dcset,1).gt.0.and.hfpp_n_tracks(dcset).gt.0)
     $      call hf1(hidFPP_Nsimp(dcset,1),float(hfpp_n_simple(dcset,1)),1.0)
       if(hidfpp_nsimp(dcset,2).gt.0.and.hfpp_n_tracks(dcset).gt.0)
     $      call hf1(hidFPP_Nsimp(dcset,2),float(hfpp_n_simple(dcset,2)),1.0)
      enddo !DCset


*     * for each DCset,iChamber,iLayer, histogram in-layer distance betw hit wires and HMS track
      do DCset=1,H_FPP_N_DCSETS
       do iChamber=1,H_FPP_N_DCINSET
        do iLayer=1,H_FPP_N_DCLAYERS
	  hid = hid_HMSwire(DCset,iChamber,iLayer)
          if (HFPP_nClusters(DCset,iChamber,iLayer).gt.0) then
            do iCluster=1,HFPP_nClusters(DCset,iChamber,iLayer)
             do iHit=1,HFPP_nHitsinCluster(DCset,iChamber,iLayer,iCluster)
               iRaw = HFPP_Clusters(DCset,iChamber,iLayer,iCluster,iHit)
               iWire = HFPP_raw_wire(iRaw)
               dist = HFPP_dHMS(DCset,iChamber,iLayer,iCluster,iHit)
               if(hid.gt.0) call hf2(hid,dist,float(iWire),1.)
	     enddo !iHit
	    enddo !iCluster
	  endif
	enddo !iLayer
       enddo !iChamber
      enddo !DCset


*     * for each DCset,iChamber,iLayer, histogram drift distances
      do DCset=1,H_FPP_N_DCSETS
       do iChamber=1,H_FPP_N_DCINSET
        do iLayer=1,H_FPP_N_DCLAYERS
          iPlane = H_FPP_N_DCLAYERS * H_FPP_N_DCINSET * (DCset-1)
     >    	 + H_FPP_N_DCLAYERS * (iChamber-1)
     >    	 + iLayer
	  hid1 = hidFPP_driftT(DCset,iChamber,iLayer)
	  hid2 = hidFPP_driftX(DCset,iChamber,iLayer)
          if (HFPP_nClusters(DCset,iChamber,iLayer).gt.0) then
            do iCluster=1,HFPP_nClusters(DCset,iChamber,iLayer)
               if(hfpp_clusterintrack(dcset,ichamber,ilayer,icluster).gt.0)
     $              then
                  if(hgood_start_time.and.abs(hstart_time-hstart_time_center).lt.hstart_time_slop) then
                     do iHit=1,HFPP_nHitsinCluster(DCset,iChamber,iLayer,iCluster)
                        iRaw = HFPP_Clusters(DCset,iChamber,iLayer,iCluster,iHit)
                        iWire = HFPP_raw_wire(iRaw)
                        time = HFPP_drift_time(DCset,iChamber,iLayer,iWire)
                        dist = HFPP_drift_dist(DCset,iChamber,iLayer,iWire)
                        
                        if(hid1.gt.0) call hf2(hid1,time,float(iWire),1.)

                        if(dist.ne.h_fpp_bad_drift) then
                           
                           if(hid2.gt.0) call hf2(hid2,dist,float(iWire),1.)
                        endif                        

                     enddo      !iHit
                  endif         !cluster in track
               endif
            enddo               !iCluster
         endif
      enddo                     !iLayer
      enddo                     !iChamber
      enddo                     !DCset


*     * for each DCset, histogram simple (Nick's) efficiency:
*     * if 5+ layers of set have hit, mark all layers (in) efficient
*     * if the do (not) have a hit
      do DCset=1,H_FPP_N_DCSETS
       if (HFPP_Nlayershit_set(DCset).ge.(H_FPP_N_DCINSET*H_FPP_N_DCLAYERS-1)) then
          hid = hidFPP_NickEff(DCset)
	  do iChamber=1,H_FPP_N_DCINSET
           do iLayer=1,H_FPP_N_DCLAYERS
              iPlane = H_FPP_N_DCLAYERS * H_FPP_N_DCINSET * (DCset-1)
     >       	     + H_FPP_N_DCLAYERS * (iChamber-1)
     >       	     + iLayer
              ii     = H_FPP_N_DCLAYERS * (iChamber-1)
     >       	     + iLayer
	      if (HFPP_N_planehits(iPlane) .gt. 0) then
        	if(hid.gt.0) call hf1(hid,float(ii),1.)
	      else
        	if(hid.gt.0) call hf1(hid,float(ii),0.)
	      endif
	   enddo !iLayer
          enddo !iChamber
       endif
      enddo !DCset


*     * for each DCset,iChamber,iLayer, histogram expected hits and actual
      do DCset=1,H_FPP_N_DCSETS
       do iChamber=1,H_FPP_N_DCINSET
        do iLayer=1,H_FPP_N_DCLAYERS
	  hid1 = hidFPP_should(DCset,iChamber,iLayer)
	  hid2 = hidFPP_did(DCset,iChamber,iLayer)
          do iTrack=1,HFPP_N_tracks(DCset)
            iWire = HFPP_stat_shouldhit(DCset,iChamber,iLayer,iTrack)
	    if (HFPP_stat_diddhit(DCset,iChamber,iLayer,iTrack)) then
	      istat = 1.0
	    else
	      istat = 0.0
	    endif
	    if(hid1.gt.0) call hf1(hid1,float(iWire),1.)      ! expected hit frequency
	    if(hid2.gt.0) call hf1(hid2,float(iWire),istat)   ! hit efficiency
	  enddo !iTrack
	enddo !iLayer
       enddo !iChamber
      enddo !DCset

*     * for each DCset,iChamber,iLayer, histogram min distance betw hits and track
      do DCset=1,H_FPP_N_DCSETS
	hid = hidFPP_dist(DCset)
        do iChamber=1,H_FPP_N_DCINSET
         do iLayer=1,H_FPP_N_DCLAYERS
           ii = H_FPP_N_DCLAYERS * (iChamber-1) + iLayer
	   do iTrack=1,HFPP_N_tracks(DCset)
             dist = HFPP_stat_dist2closest(DCset,iChamber,iLayer,iTrack)
	     if(hid.gt.0) call hf2(hid,float(ii),dist,1.)
	   enddo !iTrack
	 enddo !iLayer
       enddo !iChamber
      enddo !DCset

*     * for each DCset,iChamber,iLayer, histogram linear and angular resolutions
      if (HFPP_calc_resolution.ne.0) then
       do DCset=1,H_FPP_N_DCSETS
	 hid1 = hidFPP_resol_lin(DCset)
         hid2 = hidFPP_resol_ang(DCset)
         do iChamber=1,H_FPP_N_DCINSET
          do iLayer=1,H_FPP_N_DCLAYERS
            ii = H_FPP_N_DCLAYERS * (iChamber-1) + iLayer
            do iTrack=1,HFPP_N_tracks(DCset)
              if(hid1.gt.0) call hf2(hid1,float(ii),HFPP_track_resolution(DCset,iChamber,iLayer,iTrack),1.)
              if(hid2.gt.0) call hf2(hid2,float(ii),HFPP_track_angresol(DCset,iChamber,iLayer,iTrack),1.)
            enddo !iTrack
          enddo !iLayer
         enddo !iChamber
       enddo !DCset
      else  ! fill residuals histogram:
         do dcset=1,h_fpp_n_dcsets
            hid = hidFPP_resid(dcset)
            hid1 = hidFPP_resid6(dcset)
            hid2 = hidFPP_resid5(dcset)
            do ichamber=1,h_fpp_n_dcinset
               do ilayer = 1,h_fpp_n_dclayers
                  ii = h_fpp_n_dclayers * (ichamber - 1) + ilayer
                  do itrack=1,hfpp_n_tracks(dcset)
                     if(itrack.eq.hfpp_best_track(dcset)) then
                        if(hid.gt.0) call hf2(hid,float(ii),hfpp_track_residual(dcset,ichamber,ilayer,itrack),1.)
                        if(hid1.gt.0.and.hfpp_track_nlayers(dcset,itrack).eq.6)
     $                       call hf2(hid1,float(ii),hfpp_track_residual(dcset,ichamber,ilayer,itrack),1.)
                        if(hid2.gt.0.and.hfpp_track_nlayers(dcset,itrack).eq.5)
     $                       call hf2(hid2,float(ii),hfpp_track_residual(dcset,ichamber,ilayer,itrack),1.)

                     endif
                  enddo
               enddo
            enddo
         enddo
      endif

      

*     * for each track in each set, track chi**2, mx,bx,my,by, # hits, HFPP_track_fine,
*     *  sclose,zclose,theta,phi
      do DCset=1,H_FPP_N_DCSETS
	if(hidFPP_Ntrk(DCset).gt.0) then
           if(hselectfpptrackprune.eq.0) then
              call hf1(hidFPP_Ntrk(DCset),float(HFPP_N_tracks(DCset)),1.)
           else 
              call hf1(hidFPP_Ntrk(dcset),float(hfpp_n_goodtracks(dcset)),1.0)
              
           endif
        endif
       
        do iTrack=1,HFPP_N_tracks(DCset)
           if((itrack.eq.hfpp_best_track(dcset)).or.hfppuseajptracking.eq.0) then
c     if(hfpp_track_nlayers(dcset,itrack).eq.6) then
              if(hidFPP_Nhitontrk(DCset).gt.0) 
     $             call hf1(hidFPP_Nhitontrk(DCset),float(HFPP_track_Nlayers(DCset,iTrack)),1.)
              if(hidFPP_Nrawontrk(DCset).gt.0) 
     $             call hf1(hidFPP_Nrawontrk(DCset),float(HFPP_track_Nhits(DCset,iTrack)),1.)
              if(hidFPP_trk_chi2(DCset).gt.0) 
     $             call hf1(hidFPP_trk_chi2(DCset),HFPP_track_chi2(DCset,iTrack),1.)
              if(hidFPP_trk_mx(DCset).gt.0) 
     $             call hf1(hidFPP_trk_mx(DCset),HFPP_track_dx(DCset,iTrack),1.) !fp coords
              if(hidFPP_trk_bx(DCset).gt.0) 
     $             call hf1(hidFPP_trk_bx(DCset),HFPP_track_x(DCset,iTrack),1.)
              if(hidFPP_trk_my(DCset).gt.0) 
     $             call hf1(hidFPP_trk_my(DCset),HFPP_track_dy(DCset,iTrack),1.)
              if(hidFPP_trk_by(DCset).gt.0) 
     $             call hf1(hidFPP_trk_by(DCset),HFPP_track_y(DCset,iTrack),1.)
              if(hidFPP_fine_mx(DCset).gt.0) 
     $             call hf1(hidFPP_fine_mx(DCset),HFPP_track_fine(DCset,iTrack,1),1.) !chamber coords
              if(hidFPP_fine_bx(DCset).gt.0) 
     $             call hf1(hidFPP_fine_bx(DCset),HFPP_track_fine(DCset,iTrack,2),1.)
              if(hidFPP_fine_my(DCset).gt.0) 
     $             call hf1(hidFPP_fine_my(DCset),HFPP_track_fine(DCset,iTrack,3),1.)
              if(hidFPP_fine_by(DCset).gt.0) 
     $             call hf1(hidFPP_fine_by(DCset),HFPP_track_fine(DCset,iTrack,4),1.)
              
              if(dcset.eq.2.and.hfpp2_best_reference(itrack).gt.0) then
                 if(hidFPP_sclose(DCset).gt.0) 
     $                call hf1(hidFPP_sclose(DCset),HFPP_track_sclose(DCset+1,iTrack),1.)
                 if(hidFPP_zclose(DCset).gt.0) 
     $                call hf1(hidFPP_zclose(DCset),HFPP_track_zclose(DCset+1,iTrack),1.)
                 if(hidFPP_thetapol(DCset).gt.0) 
     $                call hf1(hidFPP_thetapol(DCset),HFPP_track_theta(DCset+1,iTrack),1.)
                 if(hidFPP_phipol(DCset).gt.0) 
     $                call hf1(hidFPP_phipol(DCset),HFPP_track_phi(DCset+1,iTrack),1.)
              else
                 if(hidFPP_sclose(DCset).gt.0) 
     $                call hf1(hidFPP_sclose(DCset),HFPP_track_sclose(DCset,iTrack),1.)
                 if(hidFPP_zclose(DCset).gt.0) 
     $                call hf1(hidFPP_zclose(DCset),HFPP_track_zclose(DCset,iTrack),1.)
                 if(hidFPP_thetapol(DCset).gt.0) 
     $                call hf1(hidFPP_thetapol(DCset),HFPP_track_theta(DCset,iTrack),1.)
                 if(hidFPP_phipol(DCset).gt.0) 
     $                call hf1(hidFPP_phipol(DCset),HFPP_track_phi(DCset,iTrack),1.)
                 
              endif
              if(hidFPP_nambig(dcset).gt.0)
     $             call hf1(hidFPP_nambig(dcset),float(hfpp_track_nambig(dcset,itrack)),1.)
           endif
c        endif
	enddo                   !iTrack
      enddo                     !DCset


      RETURN
      END
