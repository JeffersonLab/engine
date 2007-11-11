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
      include 'gen_detectorids.par'
      include 'gen_decode_common.cmn'
      INCLUDE 'hms_fpp_event.cmn'
      INCLUDE 'hms_fpp_params.cmn'
      INCLUDE 'h_fpp_ntuple.cmn'

      INCLUDE 'h_fpp_ntup.cwn'
*
      logical HEXIST	!CERNLIB function
*
      integer iSet,iTrk,iCham,iLay,iWire,cluster
      integer iHit,iRaw
      integer n
*
*--------------------------------------------------------
      err= ' '
      ABORT = .FALSE.
*
c      write(*,*)'In h_fp_nt_keep ...',h_fpp_nt_exists

      IF(.NOT.h_fpp_nt_exists) RETURN       !nothing to do

c      write(*,*)'Segments: ',h_fpp_nt_segmentevents,HFPP_nt_max_segmentevents
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
      if(HFPP_eventclass.gt.63)HFPP_eventclass=63
      cwnFPP_evtcode = HFPP_eventclass
      cwnFPP_helicite  = gbeam_helicity


*******  standard HMS info
c      write(*,*)HSDELTA,HSTHETA,HSPHI,HINVMASS
c      WRITE(*,*)HSZBEAM,HSX_FP,HSY_FP,HSXP_FP,HSYP_FP
c      write(*,*)'Beam helicity = ',gbeam_helicity_TS,gbeam_helicity
      cwnFPP_hsdelta = HSDELTA
      cwnFPP_hstheta = HSTHETA
      cwnFPP_hsphi   = HSPHI
      cwnFPP_w       = HINVMASS
      cwnFPP_hszbeam = HSZBEAM
      cwnFPP_hsxfp   = HSX_FP
      cwnFPP_hsyfp   = HSY_FP
      cwnFPP_hsxpfp  = HSXP_FP
      cwnFPP_hsypfp  = HSYP_FP
      cwnFPP_hsytar   = HSY_TAR
      cwnFPP_hsxptar  = HSXP_TAR
      cwnFPP_hsyptar  = HSYP_TAR

*******  global FPP info
c      WRITE(*,*)HFPP_TRIGGER_TDC(1)
c      WRITE(*,*)HFPP_TRIGGER_TDC(2)
c      WRITE(*,*)HFPP_RAW_TOT_HITS
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
c              write(*,*)iSet,iCham,iLay,cluster,iHit,n
              iRaw = HFPP_Clusters(iSet,iCham,iLay,cluster,iHit)
	      iWire = HFPP_raw_wire(iRaw)
c              write(*,*)iraw
c                write(*,*)iSet,HFPP_raw_plane(iRaw),HFPP_raw_wire(iRaw),
c     >              HFPP_HitTime(iRaw)  
              cwnFPP_Hit1_pol(n)    = iSet
              cwnFPP_Hit1_layer(n)  = HFPP_raw_plane(iRaw)
              cwnFPP_Hit1_wire(n)   = iWire
              cwnFPP_Hit1_d_HMS(n)  = HFPP_dHMS(iSet,iCham,iLay,cluster,iHit)
cBAD!              cwnFPP_Hit1_time(n)   = HFPP_HitTime(iRaw)
	      if (iTrk.le.0) then
                cwnFPP_Hit1_itrack(n) = 0
                cwnFPP_Hit1_time(n)   = H_FPP_BAD_TIME
	        cwnFPP_Hit1_drift(n)  = H_FPP_BAD_DRIFT
	        cwnFPP_Hit1_resid(n)  = H_FPP_BAD_DRIFT
	      else
c                write(*,*)iTrk,
c     >                HFPP_drift_dist(iSet,iCham,iLay,iWire),
c     >                HFPP_track_residual(iSet,iCham,iLay,iTrk)
	        cwnFPP_Hit1_itrack(n) = iTrk
                cwnFPP_Hit1_time(n)   = HFPP_drift_time(iSet,iCham,iLay,iWire)
	        cwnFPP_Hit1_drift(n)  = HFPP_drift_dist(iSet,iCham,iLay,iWire)
	        cwnFPP_Hit1_resid(n)  = HFPP_track_residual(iSet,iCham,iLay,iTrk)
	      endif
            enddo !iHit
          enddo !cluster

	enddo !iLay
       enddo !iCham
      enddo !iSet
      cwnFPP_Nhits1 = n


*******  FPP tracks

      n=0
      do iSet=1,2  ! Upstream & downstream polarimeter
       do iTrk=1,HFPP_N_tracks(iSet)
c         write(*,*)iSet,iTrk,HFPP_N_tracks(iSet)
c         write(*,*)iSet,iTrk
c         write(*,*)HFPP_track_Nlayers(iSet,iTrk)
c         write(*,*)HFPP_track_rough(iSet,iTrk,1),
c     >          HFPP_track_rough(iSet,iTrk,2),
c     >          HFPP_track_rough(iSet,iTrk,3),
c     >          HFPP_track_rough(iSet,iTrk,4)
c         write(*,*)HFPP_track_dx(iSet,iTrk),
c     >          HFPP_track_x(iSet,iTrk),
c     >          HFPP_track_dy(iSet,iTrk),
c     >          HFPP_track_y(iSet,iTrk)
c         write(*,*)HFPP_track_chi2(iSet,iTrk),
c     >          HFPP_track_zclose(iSet,iTrk),
c     >          HFPP_track_sclose(iSet,iTrk)
c         write(*,*)HFPP_track_theta(iSet,iTrk),
c     >          HFPP_track_phi(iSet,iTrk)
         n=n+1      
	 cwnFPP_trk_pol(n) = iSet
	 cwnFPP_trk_num(n) = iTrk

	 cwnFPP_trk_hits(n) = HFPP_track_Nlayers(iSet,iTrk)    ! # of layers w/hit on track

	 cwnFPP_simple_mx(n)   = HFPP_track_rough(iSet,iTrk,1)    ! simple (=no drift) track
	 cwnFPP_simple_bx(n)   = HFPP_track_rough(iSet,iTrk,2)
	 cwnFPP_simple_my(n)   = HFPP_track_rough(iSet,iTrk,3)
	 cwnFPP_simple_by(n)   = HFPP_track_rough(iSet,iTrk,4)

	 cwnFPP_full_mx(n)	  = HFPP_track_dx(iSet,iTrk)	    ! track w/ drift
	 cwnFPP_full_bx(n)	  = HFPP_track_x(iSet,iTrk)
	 cwnFPP_full_my(n)	  = HFPP_track_dy(iSet,iTrk)
	 cwnFPP_full_by(n)	  = HFPP_track_y(iSet,iTrk)

	 cwnFPP_chi2(n)        = HFPP_track_chi2(iSet,iTrk)

         cwnFPP_zclose(n)      = HFPP_track_zclose(iSet,iTrk)
         cwnFPP_sclose(n)      = HFPP_track_sclose(iSet,iTrk)
         cwnFPP_conetest(n)      = HFPP_track_conetest(iSet,iTrk)

         cwnFPP_theta(n)       = HFPP_track_theta(iSet,iTrk)
         cwnFPP_phi(n)         = HFPP_track_phi(iSet,iTrk)
         
cfrw     print *,gen_event_ID_number,n,iSet,cwnFPP_full_mx(n),cwnFPP_full_bx(n),cwnFPP_full_my(n),cwnFPP_full_by(n)
         
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
