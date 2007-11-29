      SUBROUTINE h_fpp_tracking(DCset,ABORT,err)
*--------------------------------------------------------
*    Hall C  HMS Focal Plane Polarimeter Code
*
*  Purpose: tracking in one set of FPP drift chambers
* 
*  Updated by Frank R. Wesselmann,  May 2006
*  Created by Frank R. Wesselmann,  February 2004
*
*--------------------------------------------------------

      IMPLICIT NONE

      include 'gen_detectorids.par'
      include 'gen_decode_common.cmn'
      INCLUDE 'hms_data_structures.cmn'
      INCLUDE 'hms_geometry.cmn'
      INCLUDE 'hms_fpp_event.cmn'
      INCLUDE 'hms_fpp_params.cmn'
      INCLUDE 'hms_id_histid.cmn'

      character*14 here
      parameter (here= 'h_fpp_tracking')

      integer*4 DCset   ! set of FPP DCs we are working on

      logical ABORT
      character*(*) err

      real*4 SimpleTrack(6), FullTrack(6)

      integer*4 BestClusters(H_FPP_N_DCINSET,H_FPP_N_DCLAYERS)

      integer*4 myclass
      logical*4 sufficient_hits, track_good, any_track, any_good, any_great

      ABORT= .FALSE.
      err= ' '

* tracking code:
**  - determine # of hits in each layer  &  # of layers with any hits
**  - if enough layers with hits (Clusters!), loop over all possible
**    combinations of Clusters and make a BASIC track to each, finding best
**  - find best PROPER track to best set of hits, using L/R ambiguity
**  - mark hits used, start over

* considerations:
** to account for the fact that dropping the worst hit will always
** improve the chi**2, even in the case of the reduced chi**2, we
** changed the order in which we try to fit tracks:
** instead of trying every combination with AT LEAST the minimum
** number of hits, but possibly more, we now first determine the
** maximum number of hits available and try ALL combinations with
** EXACTLY that count;
** if a sufficiently good track is found, we're done
** if not, we reduce the hit count by one and again try all
** possible combinations with that count, continuing until
** we either get a sufficiently good track or run into the
** externally decreed lower hit count limit
** this biases us towards more hits on the track and also reduces
** analysis time as there are fewer combinations using 5 out of 6
** hits than there are using 4 out of 6 hits (for example)

c      write(*,*)'In fpp tracking routine ...'
      any_track = .false.
      any_good  = .false.
      any_great = .false.
      myclass = H_FPP_ET_FEWHITS

      HFPP_N_tracks(DCset) = 0    !No of tracks found


*     * see if we have enough hits to bother with tracking
      call h_fpp_tracking_FreeHitCount(DCset,sufficient_hits)
*     * try to make tracks while we have hits and room to store tracks
      do while (sufficient_hits .and. (HFPP_N_tracks(DCset).lt.H_FPP_MAX_TRACKS))

*         * first determine which hits to use by fitting track to wire positions only
          call h_fpp_tracking_simple(DCset, BestClusters,SimpleTrack, ABORT,err)
          if (ABORT) then
             call g_add_path(here,err)
             return
          endif
c          write(*,*)'Simple track: Nraw = ',SimpleTrack(6),' Chi2 = ',Simpletrack(5)

*         * we really *should* have made the simple track results into shared
*         * variables and then fill the histogram from  h_fill_fpp  but this
*         * works just fine and we will probably stop filling these soon anyway!
          if (.true.) then
            if (int(SimpleTrack(6)).le.0) then
	      call hf1(hidFPP_trkrough(DCset,6),0.,1.)  !Nraw
	    else
	      call hf1(hidFPP_trkrough(DCset,1),SimpleTrack(1),1.)  !mx  
	      call hf1(hidFPP_trkrough(DCset,2),SimpleTrack(2),1.)  !bx  
	      call hf1(hidFPP_trkrough(DCset,3),SimpleTrack(3),1.)  !my  
	      call hf1(hidFPP_trkrough(DCset,4),SimpleTrack(4),1.)  !by  
	      call hf1(hidFPP_trkrough(DCset,5),SimpleTrack(5),1.)  !chi2
	      call hf1(hidFPP_trkrough(DCset,6),SimpleTrack(6),1.)  !Nraw
	    endif
	  endif

*         * quit trying to make more tracks if we are out of hits
*         * or if we couldnt make a good one now
          if (int(SimpleTrack(6)).le.0) exit
          if (int(SimpleTrack(5)).eq.H_FPP_BAD_CHI2) exit

          any_track = .true.

          FullTrack(1) = H_FPP_BAD_COORD  ! mx
          FullTrack(2) = H_FPP_BAD_COORD  ! bx
          FullTrack(3) = H_FPP_BAD_COORD  ! my
          FullTrack(4) = H_FPP_BAD_COORD  ! by
          FullTrack(5) = H_FPP_BAD_CHI2
          FullTrack(6) = 0.

          call h_fpp_tracking_drifttrack(DCset,SimpleTrack, BestClusters,track_good,FullTrack, ABORT,err)
*         * the global tracking results are stored by this subroutine as well
*         * also note that  BestClusters()  may be changed in this call!
          if (ABORT) then
             call g_add_path(here,err)
             return
          endif
c          write(*,*)'FullTrack: Chi2 = ',FullTrack(5),' track_good = ',track_good
         

*         * update event quality flags
          if (track_good) then
            any_good = .true.
	    any_great = (any_great).or.(FullTrack(5).le.HFPP_aOK_chi2)
	  else
	    exit  !since we did not use up any hits, stop trying to make more tracks!
	  endif !track_good

*         * see if we still have enough hits to bother with tracking
          call h_fpp_tracking_FreeHitCount(DCset,sufficient_hits)

      enddo !while hits 4 tracking


*     * update event descriptor
      if (HFPP_N_tracks(DCset).gt.1) then	!multiple good tracks
         if (any_great) then
	   myclass = H_FPP_ET_MANYGREAT
	 else
	   myclass = H_FPP_ET_MANYGOOD
	 endif
      elseif (HFPP_N_tracks(DCset).eq.1) then	!only one good tracks
         if (any_great) then
	   myclass = H_FPP_ET_1GREAT
	 else
	   myclass = H_FPP_ET_1GOOD
	 endif
      elseif (any_track) then	!only bad tracks
        myclass = H_FPP_ET_BAD
      endif

c      write(*,*)'Ntracks in set ',DCset,' = ',HFPP_N_tracks(DCset),
c     &    ' myclass = ',myclass
      if (HFPP_eventclass.lt.myclass) then
        HFPP_eventclass = HFPP_eventclass
     >                  + myclass * 2**(DCset-1)
      endif
      
      RETURN
      END


c==============================================================================
c==============================================================================
c==============================================================================
c==============================================================================


      SUBROUTINE h_fpp_tracking_simple(DCset,
     >                                 BestClusters,SimpleTrack,
     >                                 ABORT,err)
*--------------------------------------------------------
*    Hall C  HMS Focal Plane Polarimeter Code
*
*  Purpose: tracking in one set of FPP drift chambers
*           find best track fitted to wire centers
*           test all possible permutations until good track found
* 
*  Created by Frank R. Wesselmann,  February 2004
*
*--------------------------------------------------------

      IMPLICIT NONE

      INCLUDE 'hms_data_structures.cmn'
      INCLUDE 'hms_geometry.cmn'
      include 'gen_detectorids.par'
      include 'gen_decode_common.cmn'
      INCLUDE 'hms_fpp_event.cmn'
      INCLUDE 'hms_fpp_params.cmn'

      character*21 here
      parameter (here= 'h_fpp_tracking_simple')

      integer*4 DCset		! IN set of FPP DCs we are working on
      integer*4 BestClusters(H_FPP_N_DCINSET,H_FPP_N_DCLAYERS)
				! OUT clusters in track for this set (implicit!), chamber, layer
      real*4 SimpleTrack(6)	! OUT track based on wire positions only

      logical ABORT
      character*(*) err


      integer*4 HitCluster(H_FPP_N_DCINSET,H_FPP_N_DCLAYERS)
      integer*4 nPoints, nHitsInTrack, nHitsRequired, iterations
      integer*4 iChamber, iLayer, ii

      real*4 iTrack(5)  ! does NOT include hit count

      ABORT= .FALSE.
      err= ' '


      SimpleTrack(1) = H_FPP_BAD_COORD  ! mx
      SimpleTrack(2) = H_FPP_BAD_COORD  ! bx
      SimpleTrack(3) = H_FPP_BAD_COORD  ! my
      SimpleTrack(4) = H_FPP_BAD_COORD  ! by
      SimpleTrack(5) = H_FPP_BAD_CHI2
      SimpleTrack(6) = 0.

      do iChamber=1, H_FPP_N_DCINSET
       do iLayer=1, H_FPP_N_DCLAYERS
         BestClusters(iChamber,iLayer) = 0
       enddo !iLayer
      enddo !iChamber
      

*     * let h_fpp_tracking_NextHitCombo find out how many we can get MAX
      nHitsRequired = H_FPP_N_PLANES + 1   ! flag for h_fpp_tracking_NextHitCombo
      iterations = 0

*     * nHitsRequired == # of hits required on track for THIS iteration
*     * drop hits to find better tracks until we reach MIN
      do while (nHitsRequired .ge. HFPP_minsethits)

*         * start by getting the first useful combo
          nHitsInTrack = 0    !# of hits in combinations -- 0 means init
          call h_fpp_tracking_NextHitCombo(DCset,nHitsRequired,nHitsInTrack, HitCluster)

*         * keep comparing permutations until we tried all possibilities
          do while (nHitsInTrack.gt.0)

              iterations = iterations+1
              if (iterations.gt.HFPP_maxcombos) exit !while nHitsInTrack

*             * fit these hits using wire centers only
              call h_fpp_fit_simple(DCset,HitCluster,nPoints,iTrack,ABORT,err)
              if (ABORT) then
        	 call g_add_path(here,err)
        	 return
              endif

*             * remember this track and set of hits as best choice
              if (iTrack(5).lt.SimpleTrack(5)) then
		do ii=1,5
        	  SimpleTrack(ii) = iTrack(ii)
        	enddo !ii
        	SimpleTrack(6)= float(nPoints)
        	do iChamber=1, H_FPP_N_DCINSET
        	 do iLayer=1, H_FPP_N_DCLAYERS
        	   BestClusters(iChamber,iLayer) = HitCluster(iChamber,iLayer)
        	 enddo !iLayer
        	enddo !iChamber
              endif

*             * get next useful combo
              call h_fpp_tracking_NextHitCombo(DCset,nHitsRequired,nHitsInTrack, HitCluster)
             enddo !while permutations to try

          if (iterations.gt.HFPP_maxcombos) then
            SimpleTrack(5) = H_FPP_BAD_CHI2
            exit
          endif

*         * we have tried all combinations with the current number of hits
*         * if the current number of hits gives us a sufficiently good track,
*         * don't bother trying to find a track with fewer hits
          if ((SimpleTrack(5).ge.0.0).and.(SimpleTrack(5).le.HFPP_aOK_chi2)) exit

*         * try again with fewer hits
          nHitsRequired = nHitsRequired-1

      enddo !while hit requirement exceeds minimum
      
      RETURN
      END



c==============================================================================
c==============================================================================
c==============================================================================
c==============================================================================


      SUBROUTINE h_fpp_tracking_drifttrack(DCset,SimpleTrack,
     >                                HitClusters,track_good,DriftTrack,
     >                                ABORT,err)
*--------------------------------------------------------
*    Hall C  HMS Focal Plane Polarimeter Code
*
*  Purpose: tracking in one set of FPP drift chambers
* 
*  Created by Frank R. Wesselmann,  February 2004
*
*--------------------------------------------------------

      IMPLICIT NONE

      INCLUDE 'hms_data_structures.cmn'
      INCLUDE 'hms_geometry.cmn'
      include 'gen_detectorids.par'
      include 'gen_decode_common.cmn'
      INCLUDE 'hms_fpp_event.cmn'
      INCLUDE 'hms_fpp_params.cmn'

      character*25 here
      parameter (here= 'h_fpp_tracking_drifttrack')

      integer*4 DCset		! IN set of FPP DCs we are working on
      real*4 SimpleTrack(6)	! IN track based on wire positions only
      integer*4 HitClusters(H_FPP_N_DCINSET,H_FPP_N_DCLAYERS)
      logical*4 track_good	! OUT flag
      real*4 DriftTrack(6)	! OUT drift based track in chamber coords
      logical ABORT
      character*(*) err


      real*4 newTrack(5)  ! does NOT include hit count
      real*4 HMStrack(4),FPPtrack(4)
      real*4 DCcoords(3), FPcoords(3)
      real*4 theta,phi
      real*4 sclose,zclose
      real*4 mydriftT,mydriftX,roughv,WirePropagation,wirepos,trackpos
      real*4 x,y,z,m5,m6

      real*4 Points(H_FPP_MAX_FITPOINTS,2),   All_Points(H_FPP_MAX_FITPOINTS,2)
      real*4 Sigma2s(H_FPP_MAX_FITPOINTS),    All_Sigma2s(H_FPP_MAX_FITPOINTS)
      real*4 Projects(H_FPP_MAX_FITPOINTS,2), All_Projects(H_FPP_MAX_FITPOINTS,2)
      real*4 Drifts(H_FPP_MAX_FITPOINTS),     DriftAbs(H_FPP_MAX_FITPOINTS)
				! IN hit clusters to fit to
      integer*4 icone

      integer*4 Chambers(H_FPP_MAX_FITPOINTS),All_Chambers(H_FPP_MAX_FITPOINTS)
      integer*4 Layers(H_FPP_MAX_FITPOINTS),  All_Layers(H_FPP_MAX_FITPOINTS)
      integer*4 Wires(H_FPP_MAX_FITPOINTS),   All_Wires(H_FPP_MAX_FITPOINTS)

      integer*4 Nlayershit, nPoints, mPoints, nClusters, mClusters
      integer*4 CSkip, LSkip, Cskipped, Lskipped, Llast
      integer*4 iChamber, iLayer, iCluster, iRaw, iHit, iWire, iTrack, ii, jj


      ABORT= .FALSE.
      err= ' '

      track_good = .FALSE.


*     * first decode Clusters into a single linear array of hits ***********

      nPoints = 0
      nClusters = 0
      do iChamber=1,H_FPP_N_DCINSET
       do iLayer=1,H_FPP_N_DCLAYERS
         iCluster = HitClusters(iChamber,iLayer)
         if (iCluster.gt.0) then
           nClusters = nClusters + 1 

	   z = HFPP_layerZ(DCset,iChamber,iLayer)
	   x = SimpleTrack(1)*z + SimpleTrack(2)
           y = SimpleTrack(3)*z + SimpleTrack(4)

c           write(*,*)'----- fpp-tracking ----'
c           write(*,*)'xyz = ',x,y,z

*          * calculate time delay due to signal propagating along sense wire
*          * depends on position of event along wire, which side the read-out card
*          * is on, and the propagation speed
*          * INVERSE rotation to V coord !!!! SPECIAL case -- usually want U
           roughv = y * HFPP_direction(DCset,iChamber,iLayer,1) 
     >     	  - x * HFPP_direction(DCset,iChamber,iLayer,2)
           roughv =  roughv / HFPP_wirespeed	! convert distance to time value
           
           do iRaw = 1,HFPP_nHitsinCluster(DCset,iChamber,iLayer,iCluster)

              iHit = HFPP_Clusters(DCset,iChamber,iLayer,iCluster,iRaw)
	      iWire = HFPP_raw_wire(iHit)

*             * fix the sign depending on readout card being on +v or -v side!
              WirePropagation = roughv * float(HFPP_cardpos(DCset,iChamber,iLayer,iWire))
c              write(*,*)'Calling h_fpp_drift ... ',WirePropagation
              call h_fpp_drift(iHit,SimpleTrack,WirePropagation,
     >                         mydriftT,mydriftX,ABORT,err)

              HFPP_drift_time(DCset,iChamber,iLayer,iWire) = mydriftT  !record for posterity
              HFPP_drift_dist(DCset,iChamber,iLayer,iWire) = H_FPP_BAD_DRIFT  !init to none

c              write(*,*)'iRaw,mydriftX =',iRaw,iHit,mydriftX
              if (mydriftX.ne.H_FPP_BAD_DRIFT) then
	     	nPoints = nPoints + 1

		if (nPoints.gt.H_FPP_MAX_FITPOINTS) then
	     	  print *,' Too many fit points in ',here,' !!'
             	  nPoints = H_FPP_MAX_FITPOINTS
	     	  exit
		endif

        	All_Chambers(nPoints)   = iChamber ! remember which chamber this hit is in
        	All_Layers(nPoints)     = iLayer   !		    layer
		All_Wires(nPoints)      = iWire    !		    wire
                All_Points(nPoints,1)	= HFPP_layeroffset(DCset,iChamber,iLayer)
     >          			+ HFPP_spacing(DCset,iChamber,iLayer)*iWire
                All_Points(nPoints,2)	= HFPP_layerZ(DCset,iChamber,iLayer)
                All_Sigma2s(nPoints)	= HFPP_resolution(DCset,iChamber,iLayer)
                All_Projects(nPoints,1) = HFPP_direction(DCset,iChamber,iLayer,1)
                All_Projects(nPoints,2) = HFPP_direction(DCset,iChamber,iLayer,2)
	     	DriftAbs(nPoints)       = mydriftX  ! and its drift distance (L/R ambiguous!!)
	      endif

           enddo !iRaw
         endif !iCluster
       enddo !iLayer
      enddo !iChamber


*     * now get the track with least chi2 based on left/right drift permutations ***********

      do iHit=1,nPoints
        Drifts(iHit) = DriftAbs(iHit)
      enddo

*     * INPUT=absolute drifts  OUTPUT=signed drifts
c      write(*,*)'Calling best_permutation ... nPoints = ',nPoints
      call h_fpp_fit_best_permutation(nPoints, All_Points, All_Sigma2s, All_Projects, Drifts, newTrack)
      DriftTrack(1) = newTrack(1)  ! mx
      DriftTrack(2) = newTrack(2)  ! bx
      DriftTrack(3) = newTrack(3)  ! my
      DriftTrack(4) = newTrack(4)  ! by
      DriftTrack(5) = newTrack(5)  ! chi2/df
      DriftTrack(6) = float(nPoints)
c      write(*,*)'Results: chi2 = ',newTrack(5),' nPoints = ',nPoints,' HFPP_min_chi2 = ',HFPP_min_chi2
      if (     (newTrack(5).le.HFPP_min_chi2)
     >    .and.(newTrack(5).ge.0.0)
     >    .and.(newTrack(5).ne.H_FPP_BAD_CHI2) ) then   ! store fit results

*         * save signed drifts
	  do iHit=1,nPoints
            iChamber = All_Chambers(iHit)
            iLayer   = All_Layers(iHit)  
	    iWire    = All_Wires(iHit)   
	    HFPP_drift_dist(DCset,iChamber,iLayer,iWire) = Drifts(iHit)
	  enddo !iHit
          track_good = .true.

      elseif (nClusters.gt.HFPP_minsethits) then   ! greater, not equal!

*         * apparently we were not able to find a good track
*         * we now try dropping each cluster, one at a time, to see if a good track
*         * can be found -- note that this corresponds to ignoring one layer at a time

	  Cskipped = 0
	  Lskipped = 0
          do CSkip=1,H_FPP_N_DCINSET
           do LSkip=1,H_FPP_N_DCLAYERS

	    mPoints = 0
            mClusters = 0
            Llast = 0
	    do iHit=1,nPoints
              if ((All_Chambers(iHit).eq.CSkip).and.
     >    	  (All_Layers(iHit).eq.LSkip)) cycle   ! selectively skip this hit altogether
	      mPoints = mPoints + 1
              Points(mPoints,1)   = All_Points(iHit,1)
              Points(mPoints,2)   = All_Points(iHit,2)
              Sigma2s(mPoints)	  = All_Sigma2s(iHit)
              Projects(mPoints,1) = All_Projects(iHit,1)
              Projects(mPoints,2) = All_Projects(iHit,2)
	      Drifts(mPoints)	  = DriftAbs(iHit)
              Chambers(mPoints)   = All_Chambers(iHit)
              Layers(mPoints)	  = All_Layers(iHit)
              Wires(mPoints)	  = All_Wires(iHit)
              if (Llast.ne.All_Layers(iHit)) then  !this works as long as hits are in order
                Llast = All_Layers(iHit)
                mClusters = mClusters + 1
              endif
	    enddo !iHit

            if (mClusters.ge.HFPP_minsethits) then

      	      call h_fpp_fit_best_permutation(mPoints, Points, Sigma2s, Projects, Drifts, newTrack)

*	      * use the new track if it is better
              if (     (newTrack(5).lt.DriftTrack(5))
     >            .and.(newTrack(5).ge.0.0)
     >            .and.(newTrack(5).ne.H_FPP_BAD_CHI2) ) then
	        DriftTrack(1) = newTrack(1)  ! mx
	        DriftTrack(2) = newTrack(2)  ! bx
	        DriftTrack(3) = newTrack(3)  ! my
	        DriftTrack(4) = newTrack(4)  ! by
	        DriftTrack(5) = newTrack(5)  ! chi2/df
	        DriftTrack(6) = float(mPoints)
	        Cskipped = CSkip ! remember skipped chamber
	        Lskipped = LSkip ! remember skipped layer
	      endif

	    endif !mClusters.ge.HFPP_minsethits

	   enddo !LSkip
	  enddo !CSkip

          if (     (DriftTrack(5).le.HFPP_min_chi2)
     >        .and.(DriftTrack(5).ge.0.0)
     >        .and.(DriftTrack(5).ne.H_FPP_BAD_CHI2) ) then   ! store fit results

*	    * update Clusters used if we skipped one layer
            if (Lskipped.ne.0) then
	      HitClusters(Cskipped,Lskipped) = 0  ! remove skipped cluster
	    endif !Lskipped

*           * save signed drifts
	    do iHit=1,mPoints
              iChamber = Chambers(iHit)
              iLayer   = Layers(iHit)  
	      iWire    = Wires(iHit)   
	      HFPP_drift_dist(DCset,iChamber,iLayer,iWire) = Drifts(iHit)
	    enddo !iHit
            track_good = .true.
            nPoints = mPoints      !use stats of new, "smaller" track
            nClusters = mClusters

	  endif !dropped_one

      else
          track_good = .false.
      endif !track with all hits is/not bad


*     * store the tracking results if there was a good track ***********
      if (track_good) then

	iTrack = HFPP_N_tracks(DCset) + 1
	if (iTrack.le.H_FPP_MAX_TRACKS) then

	  HFPP_N_tracks(DCset) = iTrack   ! number of tracks in this chamber set so far

*         * store clusters and mark them used
          Nlayershit = 0
          do iChamber=1,H_FPP_N_DCINSET
           do iLayer=1,H_FPP_N_DCLAYERS

             iCluster = HitClusters(iChamber,iLayer)
	     HFPP_TrackCluster(DCset,iChamber,iLayer,iTrack) = iCluster
             if (iCluster.gt.0) then
               Nlayershit = Nlayershit + 1
               HFPP_ClusterinTrack(DCset,iChamber,iLayer,iCluster) = iTrack
             endif !iCluster.gt.0

           enddo !iLayer
          enddo !iChamber
	  HFPP_track_Nlayers(DCset,iTrack) = Nlayershit


*         * store track
          do ii=1,4		  ! store drift-based track in chamber coords
            HFPP_track_fine(DCset,iTrack,ii) = DriftTrack(ii)
          enddo !ii
          HFPP_track_chi2(DCset,iTrack)  = DriftTrack(5)
	  HFPP_track_Nhits(DCset,iTrack) = int(DriftTrack(6))

          do ii=1,6		  ! store simple track in chamber coords
            HFPP_track_rough(DCset,iTrack,ii) = SimpleTrack(ii)
          enddo !ii


*         * store drift based track in HMS focal plane coords
          DCcoords(1) = DriftTrack(1)	 ! transform slope
          DCcoords(2) = DriftTrack(3)
          DCcoords(3) = 1.0
          call h_fpp_DC2FP(DCset,.true.,DCcoords,FPcoords)
          HFPP_track_dx(DCset,iTrack) = FPcoords(1)
          HFPP_track_dy(DCset,iTrack) = FPcoords(2)

          DCcoords(1) = DriftTrack(2)	 ! transform offsets
          DCcoords(2) = DriftTrack(4)
          DCcoords(3) = 0.0
          call h_fpp_DC2FP(DCset,.false.,DCcoords,FPcoords)

*         * still need to project reference point back to z=0!!
          HFPP_track_x(DCset,iTrack) = FPcoords(1)
     >                               - FPcoords(3)*HFPP_track_dx(DCset,iTrack)
          HFPP_track_y(DCset,iTrack) = FPcoords(2)
     >                               - FPcoords(3)*HFPP_track_dy(DCset,iTrack)


*         * find angle between incident track and re-scattered track, in FP!!
          call h_fpp_relative_angles(hsxp_fp,hsyp_fp,
     >                               HFPP_track_dx(DCset,iTrack),
     >                               HFPP_track_dy(DCset,iTrack),
     >                               theta,phi)
          HFPP_track_theta(DCset,iTrack) = theta
          HFPP_track_phi(DCset,iTrack)   = phi


*         * get point and distance of closest approach
	  HMStrack(1) = hsxp_fp
	  HMStrack(2) = hsx_fp
	  HMStrack(3) = hsyp_fp
	  HMStrack(4) = hsy_fp

	  FPPtrack(1) = HFPP_track_dx(DCset,iTrack)
	  FPPtrack(2) = HFPP_track_x(DCset,iTrack)
	  FPPtrack(3) = HFPP_track_dy(DCset,iTrack)
	  FPPtrack(4) = HFPP_track_y(DCset,iTrack)

	  call h_fpp_closest(HMStrack,FPPtrack,sclose,zclose)
          
          HFPP_track_sclose(DCset,iTrack) = sclose
          HFPP_track_zclose(DCset,iTrack) = zclose

	  icone=1

          call h_fpp_conetest(HMStrack,DCset,zclose,theta,icone)

	  HFPP_track_conetest(DCset,iTrack) = icone

*         * determine resolution measure -- if requested
          if (HFPP_calc_resolution.ne.0) then

*           * init to bad
            do CSkip=1,H_FPP_N_DCINSET
             do LSkip=1,H_FPP_N_DCLAYERS
               HFPP_track_resolution(DCset,CSkip,LSkip,iTrack) = H_FPP_BAD_COORD
               HFPP_track_angresol(DCset,CSkip,LSkip,iTrack) = H_FPP_BAD_COORD
             enddo !LSkip
            enddo !CSkip

            if (nClusters.gt.HFPP_minsethits) then

*             * tracking resolution:
*             **  - require good quality track with 6 hits
*             **  - for each layer in turn, drop hit cluster from this layer
*             **    and re-figure track using remaining 5 hit cluster
*             **  - compare position of intersection of 5-hit track and currently
*             **    investigated layer with drift-corrected position of cluster
*             **  - difference is resolution
*             *
*             * Notes:
*             **  - if multiple tracks, use all good ones with 6 hits (external!)
*             **  - for multiple hits in cluster, use each hit

              do CSkip=1,H_FPP_N_DCINSET
               do LSkip=1,H_FPP_N_DCLAYERS

*         	 * figure subset of hits to use
          	 mPoints = 0
	  	 jj=0
          	 do ihit=1,nPoints
          	   if (All_Chambers(ihit).eq.CSkip.and.
     >    	       All_Layers(ihit).eq.LSkip) then
*         	     * identify the hit in this layer with the shortest drift
          	     if ((jj.eq.0).or.(DriftAbs(ihit).lt.DriftAbs(jj))) then
	  	       jj = ihit
	  	     endif
	  	   else    ! use point on reduced track
          	     mPoints = mPoints+1
          	     Points(mPoints,1)   = All_Points(ihit,1)
          	     Points(mPoints,2)   = All_Points(ihit,2)
          	     Sigma2s(mPoints)	 = All_Sigma2s(ihit)
          	     Projects(mPoints,1) = All_Projects(ihit,1)
          	     Projects(mPoints,2) = All_Projects(ihit,2)
          	     Drifts(mPoints)	 = DriftAbs(ihit)
	  	   endif
          	 enddo !ihit

          	 call h_fpp_fit_best_permutation(mPoints,Points,Sigma2s,Projects, Drifts,newTrack)

*         	 * now figure residual in skipped layer and call that the resolution
*         	 * use the hit with the smallest drift distance in this layer, determined above
	  	 if (jj.gt.0) then

          	   iWire    = All_Wires(jj)
          	   wirepos = All_Points(jj,1)
     >    		   + HFPP_drift_dist(DCset,CSkip,LSkip,iWire)  !signed drift!

	  	   x = newTrack(1)*z + newTrack(2)
          	   y = newTrack(3)*z + newTrack(4)
          	   z = All_Points(jj,2)

	  	   trackpos = HFPP_direction(DCset,CSkip,LSkip,1) * x
     >    		    + HFPP_direction(DCset,CSkip,LSkip,2) * y

	  	   HFPP_track_resolution(DCset,CSkip,LSkip,iTrack) = trackpos - wirepos

*         	   * now figure angular resolution as difference in slope between
*         	   * standard track and resolution track
          	   m6 = DriftTrack(1) * HFPP_direction(DCset,CSkip,LSkip,1)
     >    	      + DriftTrack(3) * HFPP_direction(DCset,CSkip,LSkip,2)
          	   m5 = newTrack(1) * HFPP_direction(DCset,CSkip,LSkip,1)
     >    	      + newTrack(3) * HFPP_direction(DCset,CSkip,LSkip,2)

          	   HFPP_track_angresol(DCset,CSkip,LSkip,iTrack) = m5 - m6

	  	 endif !jj

               enddo !LSkip
              enddo !CSkip

            endif !HFPP_minsethits
          endif !HFPP_calc_resolution

	endif !iTrack.le.H_FPP_MAX_TRACKS
      endif !track_good

      
      RETURN
      END


c==============================================================================
c==============================================================================
c==============================================================================
c==============================================================================


      SUBROUTINE h_fpp_tracking_NextHitCombo(DCSet,nHitsRequired,nHitsInTrack,Hits)
*--------------------------------------------------------
*    Hall C  HMS Focal Plane Polarimeter Code
*
*  Purpose: determine next combination of hits from all possible
* 
*  Updated by Frank R. Wesselmann,  May 2006
*  Created by Frank R. Wesselmann,  February 2004
*
*--------------------------------------------------------

      IMPLICIT NONE

      INCLUDE 'hms_data_structures.cmn'
      include 'gen_detectorids.par'
      include 'gen_decode_common.cmn'
      INCLUDE 'hms_fpp_event.cmn'
      INCLUDE 'hms_fpp_params.cmn'

      integer*4 DCSet         ! set of FPP DCs we are working on
      integer*4 nHitsRequired ! # of hits we are requiring for a track
                              ! generally IN, but if set excessively large
			      ! we interpret it as a flag to OUT maximum value
      integer*4 nHitsInTrack  ! IN: No of hits in combinations last tried
                              ! OUT: No of hits in combinations returned now
                              ! note that input value of 0 means initialize
      integer*4 Hits(H_FPP_N_DCINSET,H_FPP_N_DCLAYERS)  ! pointer to current
                                                        ! set of Clusters

      integer*4 iChamber, iLayer, iHit
      integer*4 Ncombos
      logical*4 trynextlayer


      if (nHitsInTrack.le.0) then
*         * init -- first call, determine starting set of hits
*         * also, if nHitsRequired is outrageously large, we interpret
*         * this to mean that we want to discover the max possible

*         * init pointers
          do iChamber=1,H_FPP_N_DCINSET
            do iLayer=1,H_FPP_N_DCLAYERS
              Hits(iChamber,iLayer) = 0
            enddo
          enddo
          nHitsInTrack = 0

*         * determine number of possible combinations
          Ncombos = 1
          do iChamber=1,H_FPP_N_DCINSET
            do iLayer=1,H_FPP_N_DCLAYERS
	      if (HFPP_Nfreehits(DCset,iChamber,iLayer).gt.0) then
		Ncombos = Ncombos * HFPP_Nfreehits(DCset,iChamber,iLayer)
	      endif
            enddo
          enddo

*	  * if too many combinations are possible, the event is
*         * too noisy for meaningful tracking, so we'll skip it!
	  if (abs(Ncombos).gt.HFPP_maxcombos) then
            nHitsInTrack = -1

	  else
*           * preset each layer to 1st hit until we have required # of hits
            iChamber = 1
            iLayer = 0
            nHitsInTrack = 0
            do while (nHitsInTrack.lt.nHitsRequired)

*              * loop over all layers and chambers in this set
               iLayer = iLayer+1
               if (iLayer.gt.H_FPP_N_DCLAYERS) then
          	 iChamber = iChamber+1
          	 iLayer = 1
               endif
               if (iChamber.gt.H_FPP_N_DCINSET) exit  !insufficient hits available!

*              * simply set layer to first free hit, if any
               if (HFPP_Nfreehits(DCset,iChamber,iLayer).gt.0) then
          	 iHit = 1
          	 do while ( (iHit .lt. HFPP_nClusters(DCset,iChamber,iLayer))
     >    		   .and. HFPP_ClusterinTrack(DCSet,iChamber,iLayer,iHit).gt.0 )
          	   iHit = iHit+1
          	 enddo
          	 if (HFPP_ClusterinTrack(DCSet,iChamber,iLayer,iHit).eq.0) then
          	   Hits(iChamber,iLayer) = iHit
          	   nHitsInTrack = nHitsInTrack + 1
          	 else
          	   Hits(iChamber,iLayer) = 0
	  	 endif
               endif

            enddo !while

*           * initialize  nHitsRequired  based on max hits available -- if requested
	    if (nHitsRequired.gt.H_FPP_N_PLANES) then
	      nHitsRequired = max(nHitsInTrack,HFPP_minsethits)
	    endif

*           * ensure we do not bother with junk events
	    if (nHitsInTrack.lt.nHitsRequired) then
              nHitsInTrack = 0
	    endif

	  endif !excessive hits


      else !nHitsInTrack
*         * iterate -- determine next set of hits

*         * in each layer of this chamber set, cycle over all hit clusters,
*         * from NONE to first up to last like an odometer;  return every combination
*         * that results in as many hits as we require, one combination per invocation;
*         * we have a counter that identifies the number of hits in this combination

          iChamber = 1
          iLayer = 0
          trynextlayer = .true.

*         * loop over layers, adding hits until we have enough
          do while (trynextlayer)

*              * goto next layer (loop over all layers and chambers in this set)
               iLayer = iLayer+1
               if (iLayer.gt.H_FPP_N_DCLAYERS) then
                 iChamber = iChamber+1
                 iLayer = 1
               endif
               if (iChamber.gt.H_FPP_N_DCINSET) exit


*              * go to next hit in this layer, if any, or to next layer
               if (HFPP_Nfreehits(DCset,iChamber,iLayer).le.0) then
                   trynextlayer = .true.

               else	 
*                  * increase counter if we are adding a NEW hit
                   if (Hits(iChamber,iLayer).eq.0) then
                     nHitsInTrack = nHitsInTrack+1
                   endif

*                  * find next free hit
                   iHit = Hits(iChamber,iLayer)+1
                   do while ( (iHit .lt. HFPP_nClusters(DCset,iChamber,iLayer))
     >             	     .and. HFPP_ClusterinTrack(DCSet,iChamber,iLayer,iHit).gt.0 )
                     iHit = iHit+1
                   enddo

*                  * did we find a free hit?
                   if ( HFPP_ClusterinTrack(DCSet,iChamber,iLayer,iHit).gt.0 .or.
     >             	(iHit .gt. HFPP_nClusters(DCset,iChamber,iLayer)) ) then
                     Hits(iChamber,iLayer) = 0    ! no free hit
                     nHitsInTrack = nHitsInTrack - 1
                     trynextlayer = .true.	  ! try next layer
                   else
                     Hits(iChamber,iLayer) = iHit  ! found free hit
                     iLayer = 0     ! restart increment at lowest layer, chamber
                     iChamber = 1
*                    * continue until we have enough hits in this combo
                     trynextlayer = (nHitsInTrack.lt.nHitsRequired)
                   endif

               endif  !(HFPP_Nfreehits(DCset,iChamber,iLayer).le.0)
          enddo !trynextlayer
	 
*         * change hit count to none if we exited due to layer count exceeded
          if (trynextlayer) then
            nHitsInTrack = 0
	  endif

      endif !nHitsInTrack

      RETURN
      END


c==============================================================================
c==============================================================================
c==============================================================================
c==============================================================================


      SUBROUTINE h_fpp_tracking_FreeHitCount(DCSet,DoTracking)
*--------------------------------------------------------
*    Hall C  HMS Focal Plane Polarimeter Code
*
*  Purpose: determine how many layers have unused hits
* 
*  Updated by Frank R. Wesselmann,  May 2006
*  Created by Frank R. Wesselmann,  February 2004
*
*--------------------------------------------------------

      IMPLICIT NONE

      INCLUDE 'hms_data_structures.cmn'
      include 'gen_detectorids.par'
      include 'gen_decode_common.cmn'
      INCLUDE 'hms_fpp_event.cmn'
      INCLUDE 'hms_fpp_params.cmn'

      integer*4 DCSet        ! drift chamber set we are working in
      logical*4 DoTracking   ! are there enough available hits 4 tracking?

      integer*4 iChamber, iLayer, iCluster
      logical*4 anyWenough, allWmin


      HFPP_NsetlayersWfreehits(DCSet) = 0

      do iChamber=1, H_FPP_N_DCINSET

        HFPP_NlayersWfreehits(DCSet,iChamber) = 0

        do iLayer=1, H_FPP_N_DCLAYERS

           HFPP_Nfreehits(DCSet,iChamber,iLayer) = 0

           if (HFPP_nClusters(DCSet,iChamber,iLayer).gt.0) then
            do iCluster=1, HFPP_nClusters(DCSet,iChamber,iLayer)

              if (HFPP_ClusterinTrack(DCSet,iChamber,iLayer,iCluster).eq.0) then
                HFPP_Nfreehits(DCSet,iChamber,iLayer) =
     >           HFPP_Nfreehits(DCSet,iChamber,iLayer) + 1

              endif
            enddo !iCluster
           endif

           if (HFPP_Nfreehits(DCSet,iChamber,iLayer).gt.0) then
             HFPP_NlayersWfreehits(DCSet,iChamber) =
     >        HFPP_NlayersWfreehits(DCSet,iChamber) + 1
           endif

        enddo !ilayer

        HFPP_NsetlayersWfreehits(DCSet) = 
     >           HFPP_NsetlayersWfreehits(DCSet) 
     >            + HFPP_NlayersWfreehits(DCSet,iChamber)

      enddo !ichamber


*     * now see if there are enough hits to bother with tracking...
      DoTracking = .false.

      if (HFPP_NsetlayersWfreehits(DCSet).ge.HFPP_minsethits) then

         allWmin = .true.
         anyWenough = .false.

         do iChamber=1, H_FPP_N_DCINSET
           allWmin = allWmin .and.
     >       (HFPP_NlayersWfreehits(DCSet,iChamber).ge.HFPP_minchamberhits)
           anyWenough = anyWenough .or.
     >       (HFPP_NlayersWfreehits(DCSet,iChamber).ge.HFPP_optchamberhits)
         enddo !ichamber

*        * we need each chamber to have a minimum number of hit layers
*        * and at least one chamber must have the optimal number of hits
         DoTracking = allWmin .and. anyWenough

      endif


      RETURN
      END
