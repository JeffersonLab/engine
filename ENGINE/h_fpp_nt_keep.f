      subroutine h_fpp_nt_keep(ABORT,err)
*----------------------------------------------------------------------
*
*     Purpose : Add entry to the HMS FPP Ntuple
*
*     Output: ABORT      - success or failure
*           : err        - reason for failure, if any
*
*----------------------------------------------------------------------
      implicit none
      save
*
      character*13 here
      parameter (here='h_fpp_nt_keep')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 'gen_data_structures.cmn'
      INCLUDE 'gen_event_info.cmn'
      INCLUDE 'hms_data_structures.cmn'
      INCLUDE 'hms_physics_sing.cmn'
      include 'gen_decode_common.cmn'
      INCLUDE 'hms_fpp_event.cmn'
      INCLUDE 'hms_fpp_params.cmn'
      INCLUDE 'h_fpp_ntuple.cmn'

      INCLUDE 'h_fpp_ntup.cwn'
*
      logical HEXIST	!CERNLIB function
*
      real*4 jTime
      integer iSet,iTrk,iCham,iLay,iWire,cluster
      integer iHit,jHit,iRaw,jRaw
      integer i,k,n
*
*--------------------------------------------------------
      err= ' '
      ABORT = .FALSE.
*
      IF(.NOT.h_fpp_nt_exists) RETURN       !nothing to do

      if (HFPP_nt_max_segmentevents .gt. 0) then
        if (h_fpp_nt_segmentevents .gt. HFPP_nt_max_segmentevents) then
          call h_fpp_nt_change(ABORT,err)
        else
          h_fpp_nt_segmentevents = h_fpp_nt_segmentevents +1
        endif
      endif
*
************************************************

      cwnFPP_eventID = gen_event_ID_number
      cwnFPP_evtcode = HFPP_eventclass


*******  standard HMS info

      cwnFPP_hsdelta = HSDELTA
      cwnFPP_hstheta = HSTHETA
      cwnFPP_hsphi   = HSPHI
      cwnFPP_w       = HINVMASS
      cwnFPP_hszbeam = HSZBEAM
      cwnFPP_hsxfp   = HSX_FP
      cwnFPP_hsyfp   = HSY_FP
      cwnFPP_hsxpfp  = HSXP_FP
      cwnFPP_hsypfp  = HSYP_FP


*******  global FPP info

      cwnFPP_trig_TDC1 = HFPP_trigger_TDC(1)
      cwnFPP_trig_TDC2 = HFPP_trigger_TDC(2)

      cwnFPP_RawHits   = HFPP_raw_tot_hits


*******  FPP hits

      n = 0
      do iSet=1,2  ! Upstream & downstream polarimeter
       do iCham=1, H_FPP_N_DCINSET	! record all 1st hits on all wires (=all cluster hits)
	do iLay=1, H_FPP_N_DCLAYERS

          do cluster=1,HFPP_nClusters(iSet,iCham,iLay)
	    iTrk  = HFPP_ClusterinTrack(iSet,iCham,iLay,cluster)
            do iHit=1,HFPP_nHitsinCluster(iSet,iCham,iLay,cluster)
              n = min(n+1,MAX_cwn_goodhits)
              iRaw = HFPP_Clusters(iSet,iCham,iLay,cluster,iHit)

              cwnFPP_Hit1_pol(n)    = iSet
              cwnFPP_Hit1_layer(n)  = HFPP_raw_plane(iRaw)
              cwnFPP_Hit1_wire(n)   = HFPP_raw_wire(iRaw)
              cwnFPP_Hit1_time(n)   = HFPP_HitTime(iRaw)

	      if (iTrk.le.0) then
	        cwnFPP_Hit1_itrack(n) = 0
	        cwnFPP_Hit1_drift(n)  = H_FPP_BAD_DRIFT
	        cwnFPP_Hit1_resid(n)  = H_FPP_BAD_DRIFT
	      else
	        iWire = HFPP_raw_wire(iRaw)
	        cwnFPP_Hit1_itrack(n) = iTrk
	        cwnFPP_Hit1_drift(n)  = HFPP_drift_dist(iSet,iCham,iLay,iWire)
	        cwnFPP_Hit1_resid(n)  = HFPP_track_residual(iSet,iCham,iLay,iTrk)
	      endif
            enddo !iHit
          enddo !cluster

	enddo !iLay
       enddo !iCham
      enddo !iSet
      cwnFPP_Nhits1 = n

      k=0
      do i=1,H_FPP_N_PLANES
        k = k + HFPP_N_planehits(i)
      enddo
      if (k.ne.n) print *,' ERROR in ',here,': hit count mismatch set ',iSet,': ',k,' != ',n


*******  FPP tracks

      n=0
      do iSet=1,2  ! Upstream & downstream polarimeter
       do iTrk=1,HFPP_N_tracks(iSet)
	 cwnFPP_trk_pol(iTrk) = iSet
	 cwnFPP_trk_num(iTrk) = iTrk

	 cwnFPP_trk_hits(iTrk) = HFPP_track_Nlayers(iSet,iTrk)    ! # of layers w/hit on track

	 cwnFPP_simple_mx(iTrk)   = HFPP_track_rough(iSet,iTrk,1)    ! simple (=no drift) track
	 cwnFPP_simple_bx(iTrk)   = HFPP_track_rough(iSet,iTrk,2)
	 cwnFPP_simple_my(iTrk)   = HFPP_track_rough(iSet,iTrk,3)
	 cwnFPP_simple_by(iTrk)   = HFPP_track_rough(iSet,iTrk,4)

	 cwnFPP_full_mx(iTrk)	  = HFPP_track_dx(iSet,iTrk)	    ! track w/ drift
	 cwnFPP_full_bx(iTrk)	  = HFPP_track_x(iSet,iTrk)
	 cwnFPP_full_my(iTrk)	  = HFPP_track_dy(iSet,iTrk)
	 cwnFPP_full_by(iTrk)	  = HFPP_track_y(iSet,iTrk)

	 cwnFPP_chi2(iTrk)        = HFPP_track_chi2(iSet,iTrk)

         cwnFPP_zclose(iTrk)      = HFPP_track_zclose(iSet,iTrk)
         cwnFPP_sclose(iTrk)      = HFPP_track_sclose(iSet,iTrk)

         cwnFPP_theta(iTrk)       = HFPP_track_theta(iSet,iTrk)
         cwnFPP_phi(iTrk)         = HFPP_track_phi(iSet,iTrk)
       enddo !iTrk
      enddo !iSet
      cwnFPP_Ntracks = n
      

************************************************
* Fill ntuple for this event
      ABORT= .NOT.HEXIST(h_fpp_nt_ID)
      IF(ABORT) THEN
        call G_build_note(':Ntuple ID#$ does not exist',
     &                        '$',h_fpp_nt_ID,' ',0.,' ',err)
        call G_add_path(here,err)
      ELSE
        call HFNT(h_fpp_nt_ID)
      ENDIF
*
      RETURN
      END
