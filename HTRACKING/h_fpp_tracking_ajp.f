      subroutine h_fpp_tracking_ajp(dcset,abort,err)

      implicit none 
      save
      
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

      logical*4 sufficient_hits, track_good, any_track, any_good, any_great
      
c     define arrays for candidate tracks:

      integer*4 ngoodtracks
      integer*4 ncandidates,i,j,k,ichamber,ilayer
      integer*4 nplanestemp,icluster,bestcandidate,best6,best5
      integer*4 itrack,track

      integer*4 goodtracks(h_fpp_max_candidates)
      real*4 simpletracks(h_fpp_max_candidates,6)
      real*4 fulltracks(h_fpp_max_candidates,6)
      real*4 thetatracks(h_fpp_max_candidates) ! store track polar angle relative to HMS 
      real*4 chi2, minchi2

      logical firsttry,any6,anytheta ! at this stage of the tracking, apply a maximum theta cut on all candidate tracks
      logical any6theta ! any tracks with six planes AND passing theta cut?

      integer*4 hitcombos(h_fpp_max_candidates,h_fpp_n_dcinset,h_fpp_n_dclayers)

      logical hitsintrack(h_fpp_max_candidates,h_fpp_n_dcinset,h_fpp_n_dclayers)
      logical ontrack(h_fpp_n_dcinset,h_fpp_n_dclayers) ! keep track of whether a cluster ends up on a track

      integer*4 candidate_nplanes(h_fpp_max_candidates)
      integer*4 candidate_nhits(h_fpp_max_candidates)

      real*4 SimpleTrack(6), FullTrack(7)

      integer*4 BestClusters(H_FPP_N_DCINSET,H_FPP_N_DCLAYERS)

      real*4 HMStrack(4)
      real*4 FPPtrack(4)
      real*4 DCcoords(3), FPcoords(3)

      real*4 PI
      parameter(PI=3.141592653)

      real*4 theta,mintheta,phi,sclose,zclose
      integer*4 conetest,bestref,jtrack

      ABORT= .FALSE.
      err= ' '

      any_track = .false.
      any_good  = .false.
      any_great = .false.

      hfpp_n_tracks(dcset) = 0

      call h_fpp_tracking_freehitcount(dcset,sufficient_hits)

      do while (sufficient_hits .and. (HFPP_N_tracks(DCset).lt.H_FPP_MAX_TRACKS))

         ncandidates = 0
         any6 = .false.
         anytheta = .false.
         any6theta = .false.
         
c     the kind of loop we want here goes until we either run out of hit combos for a simple track, or
c     until we exceed the maximum number of candidate tracks:

c     initialize local tracking arrays:

         do i=1,h_fpp_max_candidates
            candidate_nplanes(i) = 0
            candidate_nhits(i) = 0
            goodtracks(i) = 0
            do ichamber=1,h_fpp_n_dcinset
               do ilayer=1,h_fpp_n_dclayers
                  hitcombos(i,ichamber,ilayer) = 0
                  hitsintrack(i,ichamber,ilayer) = .false.
               enddo
            enddo
         enddo

c         write(*,*) 'Start of tracking, FPP,ntracks=',dcset,hfpp_n_tracks(dcset)

         call h_fpp_tracking_simple_ajp(dcset,hitcombos,simpletracks,ncandidates,abort,err)
         if (ABORT) then
            call g_add_path(here,err)
            return
         endif
         
         if(ncandidates.gt.hfpp_n_simple(dcset))
     $        hfpp_n_simple(dcset) = ncandidates
c     write(*,*) 'number of candidates after simple tracking=',ncandidates
         
c$$$  if (.true.) then
c$$$               if (int(SimpleTrack(6)).le.0) then
c$$$                  if(hidFPP_trkrough(DCset,6).gt.0) call hf1(hidFPP_trkrough(DCset,6),0.,1.) !Nraw
c$$$               else
c$$$                  if(hidFPP_trkrough(DCset,1).gt.0) call hf1(hidFPP_trkrough(DCset,1),SimpleTrack(1),1.) !mx  
c$$$                  if(hidFPP_trkrough(DCset,2).gt.0) call hf1(hidFPP_trkrough(DCset,2),SimpleTrack(2),1.) !bx  
c$$$                  if(hidFPP_trkrough(DCset,3).gt.0) call hf1(hidFPP_trkrough(DCset,3),SimpleTrack(3),1.) !my  
c$$$                  if(hidFPP_trkrough(DCset,4).gt.0) call hf1(hidFPP_trkrough(DCset,4),SimpleTrack(4),1.) !by  
c$$$                  if(hidFPP_trkrough(DCset,5).gt.0) call hf1(hidFPP_trkrough(DCset,5),SimpleTrack(5),1.) !chi2
c$$$                  if(hidFPP_trkrough(DCset,6).gt.0) call hf1(hidFPP_trkrough(DCset,6),SimpleTrack(6),1.) !Nraw
c$$$               endif
c$$$            endif
            
*     * quit trying to make more tracks if we are out of hits
*     * or if we couldnt make a good one now
c            if (int(SimpleTrack(6)).le.0) exit
c            if (int(SimpleTrack(5)).eq.H_FPP_BAD_CHI2) exit
            
c            any_track = .true.
         
         if(ncandidates.eq.0) exit

         ngoodtracks = 0

         do i=1,ncandidates
            FullTrack(1) = H_FPP_BAD_COORD ! mx
            FullTrack(2) = H_FPP_BAD_COORD ! bx
            FullTrack(3) = H_FPP_BAD_COORD ! my
            FullTrack(4) = H_FPP_BAD_COORD ! by
            FullTrack(5) = H_FPP_BAD_CHI2
            FullTrack(6) = 0. ! number of hits
            FullTrack(7) = 0. ! number of planes
c            write(*,*) 'candidate ',i,' simple chi2=',simpletracks(i,5)

            do j=1,6
               simpletrack(j) = simpletracks(i,j)
c               write(*,*) 'i,simpletrack(i)=',j,simpletrack(j)
            enddo

            do ichamber=1,h_fpp_n_dcinset
               do ilayer=1,h_fpp_n_dclayers
                  bestclusters(ichamber,ilayer) = hitcombos(i,ichamber,ilayer)
c                  write(*,*) 'chbr,pln,clstr=',ichamber,ilayer,bestclusters(ichamber,ilayer)
               enddo
            enddo

            call h_fpp_tracking_drifttrack_ajp(dcset,simpletrack,bestclusters,
     $           ontrack,track_good,fulltrack,1,abort,err)
            
            
c            write(*,*) 'track_good = ',track_good
            if (ABORT) then
               call g_add_path(here,err)
               return
            endif
            
            if(track_good) then

               dccoords(1) = fulltrack(1)
               dccoords(2) = fulltrack(3)
               dccoords(3) = 1.0

               call h_fpp_dc2fp(dcset,.true.,dccoords,fpcoords)

               call h_fpp_relative_angles(hsxp_fp,hsyp_fp,
     $              fpcoords(1),fpcoords(2),theta,phi)

               if(theta.le.hfpp_prune_thetamax(dcset)*PI/180.0) 
     $              anytheta = .true.
               
               thetatracks(i) = theta
               
               do j=1,6
                  fulltracks(i,j) = fulltrack(j)
               enddo
               
               nplanestemp = int(fulltrack(7))
               
               candidate_nplanes(i) = nplanestemp
               candidate_nhits(i) = int(fulltrack(6))

               ngoodtracks = ngoodtracks + 1
               goodtracks(ngoodtracks) = i
               
               if(nplanestemp.eq.h_fpp_n_dcinset*h_fpp_n_dclayers) 
     $              any6 = .true.

               if(theta.le.hfpp_prune_thetamax(dcset)*PI/180.0
     $              .and.nplanestemp.eq.h_fpp_n_dcinset*h_fpp_n_dclayers)
     $              any6theta = .true.

               do ichamber=1,h_fpp_n_dcinset
                  do ilayer = 1,h_fpp_n_dclayers
                     if(ontrack(ichamber,ilayer))
     $                    hitsintrack(i,ichamber,ilayer) = .true.
                  enddo
               enddo

            endif
         enddo

c         write(*,*) 'number of good tracks after tracking with drift=',ngoodtracks

         if(ngoodtracks.eq.0) exit
         
         bestcandidate = 0
         firsttry = .true.
         
         do i=1,ngoodtracks
            track = goodtracks(i)
            
            chi2 = fulltracks(track,5)

c     prioritize: tracks passing theta cut take precedence over six-plane tracks.
c     we want tracks with six planes passing theta cut, but we will take a five-plane track 
c     passing theta over a six-plane track failing theta:
            if(firsttry.or.chi2.lt.minchi2) then
c$$$               if(any6theta) then ! there is at least one track passing six planes AND theta cut. Require both
c$$$                  if(candidate_nplanes(track).eq.h_fpp_n_dcinset*h_fpp_n_dclayers
c$$$     $                 .and.thetatracks(track).le.hfpp_prune_thetamax(dcset)*PI/180.0)
c$$$     $                 then
c$$$                     firsttry = .false.
c$$$                     minchi2 = chi2
c$$$                     bestcandidate = track
c$$$                  endif
c$$$               else if(anytheta) then ! there is no track passing six planes AND theta, require theta cut
c$$$                  if(thetatracks(track).le.hfpp_prune_thetamax(dcset)*PI/180.0)
c$$$     $                 then
c$$$                     firsttry = .false.
c$$$                     minchi2 = chi2
c$$$                     bestcandidate = track
c$$$                  endif
c$$$               else if(any6) then ! there is no track passing theta cut. Require six planes
               if(any6) then
                  if(candidate_nplanes(track).eq.h_fpp_n_dcinset*h_fpp_n_dclayers)
     $                 then
                     firsttry = .false.
                     minchi2 = chi2
                     bestcandidate = track
                  endif
               else             ! there is no track passing theta cut OR six planes. take best chi2
                  firsttry = .false.
                  minchi2 = chi2
                  bestcandidate = track
               endif
            endif
         enddo
        
         do i=1,ncandidates
            do ichamber=1,h_fpp_n_dcinset
               do ilayer=1,h_fpp_n_dclayers
                  icluster = hitcombos(i,ichamber,ilayer)
                  if(icluster.gt.0) then ! mark all candidate hits as unused
                     hfpp_clusterintrack(dcset,ichamber,ilayer,icluster) = 0
                  endif
               enddo
            enddo
         enddo
       
c         write(*,*) 'best candidate track, chi2=',bestcandidate,fulltracks(bestcandidate,5)
c         write(*,*) 'nplanes,nhits=',candidate_nplanes(bestcandidate),
c     $        candidate_nhits(bestcandidate)
         
c$$$         do i=1,4
c$$$            write(*,*) 'i,besttrack(i)=',i,fulltracks(bestcandidate,i)
c$$$         enddo

         if(bestcandidate.eq.0) exit
         
c     Now that we have found the best candidate track, add it to the good track array:

         itrack = hfpp_n_tracks(dcset) + 1
         
         if(itrack.le.h_fpp_max_tracks) then

            hfpp_n_tracks(dcset) = itrack

            hfpp_track_nlayers(dcset,itrack) = candidate_nplanes(bestcandidate)
c     Mark the hits in the best candidate track as used!
            do ichamber = 1,h_fpp_n_dcinset
               do ilayer = 1,h_fpp_n_dclayers
                  icluster = hitcombos(bestcandidate,ichamber,ilayer)
                  if(icluster.gt.0.and.
     $                 hitsintrack(bestcandidate,ichamber,ilayer) ) then
                     hfpp_clusterintrack(dcset,ichamber,ilayer,icluster) = itrack
                  endif
                  hfpp_trackcluster(dcset,ichamber,ilayer,itrack) = icluster
               enddo
            enddo

            do j=1,4
               hfpp_track_fine(dcset,itrack,j) = fulltracks(bestcandidate,j)
            enddo
            
            hfpp_track_chi2(dcset,itrack) = fulltracks(bestcandidate,5)
            hfpp_track_nhits(dcset,itrack) = int(fulltracks(bestcandidate,6))

            do j=1,6
               hfpp_track_rough(dcset,itrack,j) = simpletracks(bestcandidate,j)
            enddo
            
            hfpp_track_uniq(dcset,itrack) = .true.
*     transform slopes from local FPP coordinates to HMS fp coordinates:
            dccoords(1) = fulltracks(bestcandidate,1) ! dx/dz
            dccoords(2) = fulltracks(bestcandidate,3) ! dy/dz
            dccoords(3) = 1.0                         ! dz/dz

            call h_fpp_dc2fp(dcset,.true.,dccoords,fpcoords)

            hfpp_track_dx(dcset,itrack) = fpcoords(1)
            hfpp_track_dy(dcset,itrack) = fpcoords(2)

*     now transform coordinates from local FPP system to HMS fp coords:

            dccoords(1) = fulltracks(bestcandidate,2) ! x
            dccoords(2) = fulltracks(bestcandidate,4) ! y
            dccoords(3) = 0.0

            call h_fpp_dc2fp(dcset,.false.,dccoords,fpcoords)

            hfpp_track_x(dcset,itrack) = fpcoords(1) - fpcoords(3)*
     $           hfpp_track_dx(dcset,itrack)
            hfpp_track_y(dcset,itrack) = fpcoords(2) - fpcoords(3)*
     $           hfpp_track_dy(dcset,itrack)

*     calculate relative scattering angles theta and phi:

            call h_fpp_relative_angles(hsxp_fp,hsyp_fp,hfpp_track_dx(dcset,itrack),
     $           hfpp_track_dy(dcset,itrack),theta,phi)

            hfpp_track_theta(dcset,itrack) = theta
            hfpp_track_phi(dcset,itrack) = phi

*     calculate closest approach parameters:

            hmstrack(1) = hsxp_fp
            hmstrack(2) = hsx_fp
            hmstrack(3) = hsyp_fp
            hmstrack(4) = hsy_fp

            fpptrack(1) = hfpp_track_dx(dcset,itrack)
            fpptrack(2) = hfpp_track_x(dcset,itrack)
            fpptrack(3) = hfpp_track_dy(dcset,itrack)
            fpptrack(4) = hfpp_track_y(dcset,itrack)

            call h_fpp_closest(hmstrack,fpptrack,sclose,zclose)

            hfpp_track_sclose(dcset,itrack) = sclose
            hfpp_track_zclose(dcset,itrack) = zclose

            conetest = 1
            
            call h_fpp_conetest(hmstrack,dcset,zclose,theta,conetest)
            
            hfpp_track_conetest(dcset,itrack) = conetest

            if(dcset.eq.2) then

               firsttry=.true.
               bestref = 0
               
               do jtrack=1,hfpp_n_tracks(1)
                  call h_fpp_relative_angles(hfpp_track_dx(1,jtrack),
     $                 hfpp_track_dy(1,jtrack),
     $                 hfpp_track_dx(dcset,itrack),
     $                 hfpp_track_dy(dcset,itrack),
     $                 theta,phi)
                  if(firsttry.or.theta.lt.mintheta) then
                     firsttry = .false.
                     mintheta = theta
                     bestref = jtrack
                  endif
               enddo

               hfpp2_best_reference(itrack) = bestref

               if(bestref.gt.0) then
                  call h_fpp_relative_angles(hfpp_track_dx(1,bestref),
     $                 hfpp_track_dy(1,bestref),
     $                 hfpp_track_dx(dcset,itrack),
     $                 hfpp_track_dy(dcset,itrack),theta,phi)
                  hfpp_track_theta(dcset+1,itrack) = theta
                  hfpp_track_phi(dcset+1,itrack) = phi
                  
                  hmstrack(1) = hfpp_track_dx(1,bestref)
                  hmstrack(2) = hfpp_track_x(1,bestref)
                  hmstrack(3) = hfpp_track_dy(1,bestref)
                  hmstrack(4) = hfpp_track_y(1,bestref)
                  
                  fpptrack(1) = hfpp_track_dx(dcset,itrack)
                  fpptrack(2) = hfpp_track_x(dcset,itrack)
                  fpptrack(3) = hfpp_track_dy(dcset,itrack)
                  fpptrack(4) = hfpp_track_y(dcset,itrack)
                  
                  call h_fpp_closest(hmstrack,fpptrack,sclose,zclose)
                  
                  hfpp_track_sclose(dcset+1,itrack) = sclose
                  hfpp_track_zclose(dcset+1,itrack) = zclose
         
                  conetest = 1
                  
                  call h_fpp_conetest(hmstrack,dcset,zclose,theta,conetest)
                  
                  hfpp_track_conetest(dcset+1,itrack) = conetest
               endif
            endif
         endif ! end filling track common block variables
c     NOW we need to call freehitcount and start over with the remaining free hits:
         
         call h_fpp_tracking_freehitcount(dcset,sufficient_hits)

      enddo

      return
      end

      subroutine h_fpp_tracking_simple_ajp(dcset,hitcombos,simpletracks,
     $     ncandidates,abort,err)

      implicit none
      save

      INCLUDE 'hms_data_structures.cmn'
      INCLUDE 'hms_geometry.cmn'
      include 'gen_detectorids.par'
      include 'gen_decode_common.cmn'
      INCLUDE 'hms_fpp_event.cmn'
      INCLUDE 'hms_fpp_params.cmn'

      character*21 here
      parameter (here= 'h_fpp_tracking_simple')

      integer*4 dcset
      integer*4 hitcombos(h_fpp_max_candidates,h_fpp_n_dcinset,h_fpp_n_dclayers)
      real*4 simpletracks(h_fpp_max_candidates,6)

      integer*4 HitCluster(H_FPP_N_DCINSET,H_FPP_N_DCLAYERS)

      integer*4 ncandidates,npoints
      integer*4 nhitsrequired,iterations,ichamber,ilayer,i,j
      integer*4 nhitsintrack

      logical abort
      character*(*) err

      real*4 temptrack(5) ! not including hit count

      abort = .false.
      err= ' '

      do i=1,h_fpp_max_candidates
         do ichamber=1,h_fpp_n_dcinset
            do ilayer=1,h_fpp_n_dclayers
               hitcombos(i,ichamber,ilayer) = 0
            enddo
         enddo
         do j=1,5
            simpletracks(i,j) = h_fpp_bad_coord
         enddo
         simpletracks(i,6) = 0.0
      enddo

      nhitsrequired = h_fpp_n_planes + 1
      iterations = 0

      do while(nhitsrequired .ge. hfpp_minsethits.and.ncandidates.lt.
     $     h_fpp_max_candidates)

         nhitsintrack = 0
*     call next combo with large nhitsrequired to get the first useful combo as well as the max
*     number of hits available
         ncandidates = 0

         call h_fpp_tracking_nexthitcombo(dcset,nhitsrequired,nhitsintrack,hitcluster)

*     keep comparing permutations until all possibilities are tried:

         do while(nhitsintrack.gt.0.and.ncandidates.lt.h_fpp_max_candidates)

            iterations = iterations + 1

            if(iterations.gt.hfpp_maxcombos) exit

            call h_fpp_fit_simple(dcset,hitcluster,npoints,temptrack,abort,err)

            if (ABORT) then
               call g_add_path(here,err)
               return
            endif
*     in contrast to Frank's algorithm, here we keep track of any hit combos which pass the "reasonable chi2"
*     criterion:

            if(temptrack(5).ge.0.0.and.
     $           temptrack(5).le.hfpp_aok_chi2) then ! add a new candidate track to the array:
               ncandidates = ncandidates + 1
               do ichamber=1,h_fpp_n_dcinset
                  do ilayer=1,h_fpp_n_dclayers
                     hitcombos(ncandidates,ichamber,ilayer) = 
     $                    hitcluster(ichamber,ilayer)
                  enddo
               enddo
               
               do j=1,5
                  simpletracks(ncandidates,j) = temptrack(j)
               enddo
               simpletracks(ncandidates,6) = float(npoints)
            endif

            call h_fpp_tracking_nexthitcombo(dcset,nhitsrequired,nhitsintrack,hitcluster)
            
         enddo

         if(iterations.gt.hfpp_maxcombos) then
            ncandidates = 0
            exit
         endif
         
         if(ncandidates.gt.0) exit

         nhitsrequired = nhitsrequired - 1

      enddo

      return
      end
            
            
