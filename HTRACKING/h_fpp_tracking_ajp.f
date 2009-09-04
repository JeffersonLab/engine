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

      integer*4 nhitsrequired

      integer*4 ngoodtracks
      integer*4 ncandidates,i,j,k,ichamber,ilayer
      integer*4 nplanestemp,icluster,bestcandidate,best6,best5
      integer*4 itrack,track,hit,ihit,wire

      integer*4 goodtracks(h_fpp_max_candidates)
      real*4 simpletracks(h_fpp_max_candidates,6)
      real*4 fulltracks(h_fpp_max_candidates,6)
      real*4 thetatracks(h_fpp_max_candidates) ! store track polar angle relative to HMS 
      real*4 sclosetracks(h_fpp_max_candidates) ! store track distance of closest approach relative to HMS or FPP1 
      real*4 zclosetracks(h_fpp_max_candidates) ! store track z of closest approach relative to HMS or FPP1
      real*4 phitracks(h_fpp_max_candidates)
      integer*4 conetesttracks(h_fpp_max_candidates) ! store cone test of tracks relative to HMS or FPP1
      integer*4 bestreftracks(h_fpp_max_candidates)
      logical greattrack(h_fpp_max_candidates)
      real*4 chi2, minchi2

c      logical ambiguity(h_fpp_max_candidates)
      logical firsttheta,firstsclose
      logical firsttry,any6,anytheta ! at this stage of the tracking, apply a maximum theta cut on all candidate tracks
      logical any6theta ! any tracks with six planes AND passing theta cut?
      logical anyzclose,any6zclose ! any tracks with zclose passing prune tests?
      logical anyconetest,any6conetest

      integer*4 hitcombos(h_fpp_max_candidates,h_fpp_n_dcinset,h_fpp_n_dclayers)

      logical hitsintrack(h_fpp_max_candidates,h_fpp_n_dcinset,h_fpp_n_dclayers)
      logical ontrack(h_fpp_n_dcinset,h_fpp_n_dclayers) ! keep track of whether a cluster ends up on a track

      integer*4 candidate_nplanes(h_fpp_max_candidates)
      integer*4 candidate_nhits(h_fpp_max_candidates)

c      integer*4 candidate_wires(h_fpp_max_candidates,h_fpp_max_fitpoints)
c      real*4 candidate_drifts(h_fpp_max_candidates,h_fpp_max_fitpoints)

      real*4 SimpleTrack(6), FullTrack(7)

      integer*4 BestClusters(H_FPP_N_DCINSET,H_FPP_N_DCLAYERS)

      real*4 HMStrack(4)
      real*4 FPPtrack(4),newFPPtrack(4)
      real*4 DCslopes(3),DCcoords(3), FPcoords(3), FPslopes(3)

      real*4 PI
      parameter(PI=3.141592653)

      real*4 theta,mintheta,phi,sclose,minsclose,zclose
      real*4 criterion,mincriterion
      integer*4 conetest,bestref,jtrack
      integer*4 iteration

      logical zgood
      logical goodhms

      real*4 prob

      ABORT= .FALSE.
      err= ' '

      any_track = .false.
      any_good  = .false.
      any_great = .false.

      hfpp_n_tracks(dcset) = 0

      call h_fpp_tracking_freehitcount(dcset,sufficient_hits)

      iteration = 0

      goodhms = abs(hsxp_tar).le..08.and.abs(hsyp_tar).le..04.and.abs(hsy_tar)<5..and.
     $     abs(hsdelta)<10.

      do while (sufficient_hits .and. (HFPP_N_tracks(DCset).lt.H_FPP_MAX_TRACKS))
         
c$$$         if(goodhms) then
c$$$            write(*,*) 'FPP tracking: chamber = ',dcset,' iteration = ',iteration
c$$$         endif
         iteration = iteration + 1

         ncandidates = 0
         any6 = .false.
         anytheta = .false.
         any6theta = .false.
         anyzclose = .false.
         any6zclose = .false.
         anyconetest = .false.
         any6conetest = .false.
 
         any_great = .false.

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

         nhitsrequired = 0

c         write(*,*) 'before simple tracking, nhits required=',nhitsrequired

         call h_fpp_tracking_simple_ajp(dcset,hitcombos,simpletracks,ncandidates,nhitsrequired,abort,err)
         if (ABORT) then
            call g_add_path(here,err)
            return
         endif

c$$$         write(*,*) 'after simple tracking, nhits required,ncandidate=',
c$$$     $        nhitsrequired,ncandidates

         if(iteration.eq.1)
     $        hfpp_n_simple(dcset,1) = ncandidates
         
c$$$         if(goodhms) then
c$$$            write(*,*) 'number of candidates after simple tracking=',ncandidates
c$$$         endif
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

 134     do i=1,ncandidates
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
     $           ontrack,track_good,fulltrack,nhitsrequired,1,abort,err)
            
            
c            write(*,*) 'track_good = ',track_good
            if (ABORT) then
               call g_add_path(here,err)
               return
            endif
            
            if(track_good) then ! tracking with drift resulted in a reasonable track:

*     calculate everything so we can check its quality/value against quantities other than just chi2:
*     transform from FPP to focal plane coordinate system: 
*     slopes first:
               dcslopes(1) = fulltrack(1)
               dcslopes(2) = fulltrack(3)
               dcslopes(3) = 1.0
               
               call h_fpp_dc2fp(dcset,.true.,dcslopes,fpslopes)

*     Next, transform coordinates from FPP to focal plane system:
               dccoords(1) = fulltrack(2)
               dccoords(2) = fulltrack(4)
               dccoords(3) = 0.0
               
               call h_fpp_dc2fp(dcset,.false.,dccoords,fpcoords)
               
               fpcoords(1) = fpcoords(1) - fpslopes(1)*fpcoords(3)
               fpcoords(2) = fpcoords(2) - fpslopes(2)*fpcoords(3)

               fpptrack(1) = fpcoords(1)
               fpptrack(2) = fpcoords(2)
               fpptrack(3) = fpslopes(1)
               fpptrack(4) = fpslopes(2)

               call h_fpp_align(dcset,fpptrack,newfpptrack)
*     initialize bestref to zero:
               bestref = 0
*     initialize "hmstrack" to the actual hms track:
               hmstrack(1) = hsxp_fp
               hmstrack(2) = hsx_fp
               hmstrack(3) = hsyp_fp
               hmstrack(4) = hsy_fp
*     initialize "fpptrack" to the current track:
c$$$               fpptrack(1) = fpslopes(1)
c$$$               fpptrack(2) = fpcoords(1)
c$$$               fpptrack(3) = fpslopes(2)
c$$$               fpptrack(4) = fpcoords(2)
               fpptrack(1) = newfpptrack(3) ! dx/dz
               fpptrack(2) = newfpptrack(1) ! x
               fpptrack(3) = newfpptrack(4) ! dy/dz
               fpptrack(4) = newfpptrack(2) ! y

*     calculate closest approach parameters relative to hms track:
               call h_fpp_closest(hmstrack,fpptrack,sclose,zclose)
               call h_fpp_relative_angles(hmstrack(1),hmstrack(3),fpptrack(1),fpptrack(3),theta,phi)
*     in FPP2, we use the "best" track from FPP1 as a reference track:
               mintheta = theta
               bestref = 0
               if(dcset.eq.2.and.hfpp_n_tracks(1).gt.0) then
                  do jtrack = 1, hfpp_n_tracks(1)
                     hmstrack(1) = hfpp_track_dx(1,jtrack)
                     hmstrack(2) = hfpp_track_x(1,jtrack)
                     hmstrack(3) = hfpp_track_dy(1,jtrack)
                     hmstrack(4) = hfpp_track_y(1,jtrack)
                     call h_fpp_closest(hmstrack,fpptrack,sclose,zclose)
                     call h_fpp_relative_angles(hmstrack(1),hmstrack(3),fpptrack(1),fpptrack(3),theta,phi)
                     if(theta.lt.mintheta) then
                        bestref = jtrack
                        bestreftracks(i) = bestref
                        mintheta = theta
                     endif
                  enddo
c     now, calculate everything one more time using bestref:
                  if(bestref.gt.0) then
                     hmstrack(1) = hfpp_track_dx(1,bestref)
                     hmstrack(2) = hfpp_track_x(1,bestref)
                     hmstrack(3) = hfpp_track_dy(1,bestref)
                     hmstrack(4) = hfpp_track_y(1,bestref)
                  else ! revert to HMS track:
                     hmstrack(1) = hsxp_fp
                     hmstrack(2) = hsx_fp
                     hmstrack(3) = hsyp_fp
                     hmstrack(4) = hsy_fp
                  endif

                  call h_fpp_closest(hmstrack,fpptrack,sclose,zclose)
                  call h_fpp_relative_angles(hmstrack(1),hmstrack(3),fpptrack(1),fpptrack(3),theta,phi)

               endif

               bestreftracks(i) = bestref

               conetest = 1
               
               call h_fpp_conetest(hmstrack,dcset,zclose,theta,conetest)

               thetatracks(i) = theta
               phitracks(i) = phi
               
               sclosetracks(i) = sclose
               zclosetracks(i) = zclose

               conetesttracks(i) = conetest

               if(theta.le.hfpp_prune_thetamax(dcset)*PI/180.0) 
     $              anytheta = .true.

               do j=1,6
                  fulltracks(i,j) = fulltrack(j)
               enddo
               
               nplanestemp = int(fulltrack(7))
               
               candidate_nplanes(i) = nplanestemp
               candidate_nhits(i) = int(fulltrack(6))

               ngoodtracks = ngoodtracks + 1
               goodtracks(ngoodtracks) = i
               
               greattrack(i) = ( conetest.eq.1.and.
     $              sclose.le.hfpp_prune_sclose(dcset)
     $              .and.zclose.ge.hfpp_prune_zclose(2*(dcset-1)+1)-
     $              hfpp_prune_zslop(dcset)/tan(theta).and.zclose.le.
     $              hfpp_prune_zclose(2*(dcset-1)+2) +
     $              hfpp_prune_zslop(dcset)/tan(theta) .and. 
     $              nplanestemp.eq.6 )
               if(dcset.eq.2.and.bestref.gt.0) then
                  greattrack(i) = ( conetest.eq.1.and.
     $              sclose.le.hfpp_prune_sclose(1)
     $              .and.zclose.ge.hfpp_prune_zclose(2*(dcset-1)+1)-
     $              hfpp_prune_zslop(dcset)/tan(theta).and.zclose.le.
     $              hfpp_prune_zclose(2*(dcset-1)+2) +
     $              hfpp_prune_zslop(dcset)/tan(theta) .and. 
     $              nplanestemp.eq.6 )
               endif


               if(greattrack(i)) any_great = .true.

               if(nplanestemp.eq.h_fpp_n_dcinset*h_fpp_n_dclayers) 
     $              any6 = .true.

               if(conetest.eq.1) then 
                  anyconetest = .true.
                  if(nplanestemp.eq.h_fpp_n_dcinset*h_fpp_n_dclayers) 
     $                 then
                     any6conetest = .true.
                  endif
               endif

               if(zclose.ge.hfpp_prune_zclose(2*(dcset-1)+1) -
     $              hfpp_prune_zslop(dcset)/tan(theta).and.
     $              zclose.le.hfpp_prune_zclose(2*(dcset-1)+2) + 
     $              hfpp_prune_zslop(dcset)/tan(theta)) then
                  anyzclose = .true.
                  if(nplanestemp.eq.h_fpp_n_dcinset*h_fpp_n_dclayers)
     $                 then
                     any6zclose = .true.
                  endif
               endif

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
         
c$$$  if(ngoodtracks.gt.hfpp_n_simple(dcset,2)) then
c$$$            hfpp_n_simple(dcset,2) = ngoodtracks
c$$$         endif

         if(iteration.eq.1) then
            hfpp_n_simple(dcset,2) = ngoodtracks
         endif
         
c     if the left-right fixing routine is turned on and we don't prune on the number of planes 
c     on the track, then don't require six planes:
         
         if(hfpp_prune_nplanes.eq.0.and.hfppfixleftright.gt.0) then
            any6 = .false.
         endif
            
c$$$         if(goodhms) then
c$$$            write(*,*) 
c$$$     $           'number of good tracks after tracking with drift=',ngoodtracks
c$$$         endif

c         write(*,*) 'ngood drift tracks = ',ngoodtracks

         if(ngoodtracks.eq.0) then
            if(nhitsrequired.gt.hfpp_minsethits) then
               nhitsrequired = nhitsrequired - 1
               goto 134
            else
               exit
            endif
         endif
         
         bestcandidate = 0
         firsttry = .true.
         firsttheta = .true.
         firstsclose = .true.
*     get minimum chi2, sclose, and theta, always checking for existence of six-hit tracks first:
         do i=1,ngoodtracks
            track=goodtracks(i)
            chi2 = fulltracks(track,5)
            sclose = sclosetracks(track)
            theta = thetatracks(track)
            if(candidate_nplanes(track).eq.6.or..not.any6) then
               if(firsttry) then
                  firsttry = .false.
                  mintheta = theta 
                  minchi2 = chi2
                  minsclose = sclose
               else
                  if(theta.lt.mintheta) mintheta = theta
                  if(chi2.lt.minchi2) minchi2 = chi2
                  if(sclose.lt.minsclose) minsclose = sclose               
               endif
            endif
         enddo
         firsttry = .true.
         do i=1,ngoodtracks     ! here is where we try to pick the best track:
            
            track = goodtracks(i)
            
            chi2 = fulltracks(track,5)
            sclose = sclosetracks(track)
            theta = thetatracks(track)
            zclose = zclosetracks(track)
            conetest = conetesttracks(track)
            bestref = bestreftracks(track)
            phi = phitracks(track)

c            if(ngoodtracks.gt.1) then
c            if(.not.any6.or.candidate_nplanes(track).eq.6) then
c$$$               if(goodhms) then
c$$$                  write(*,*) 'track ',track,' theta=',theta*180.0/PI,
c$$$     $                 ' chi2=',chi2,
c$$$     $                 ' sclose=',sclose,' zclose=',zclose,' phi=',phi*180.0/PI,
c$$$     $                 ' nplanes=',candidate_nplanes(track),
c$$$     $                 ' nhits=',candidate_nhits(track),
c$$$     $                 ' conetest=',conetest
c$$$                  if(dcset.eq.2) write(*,*) 'bestref=',bestref
c$$$               endif
c            endif
          
c     try chi2 + sclose^2/sigmasclose^2
c            criterion = chi2 + (sclose*hfpp_sclose_weight(dcset))**2

c            criterion = chi2 + minchi2*(hfpp_sclose_weight(dcset)*(sclose/minsclose)**2 + 
c     $           hfpp_theta_weight(dcset)*prob(dcset,mintheta)/prob(dcset,theta))
            
c            if(dcset.eq.2.and.bestref.gt.0) then
c               criterion = chi2 + minchi2*(hfpp_sclose_weight(1)*(sclose/minsclose)**2 + 
c     $              hfpp_theta_weight(1)*prob(1,mintheta)/prob(1,theta))
c            endif

            zgood = zclose.ge.hfpp_prune_zclose(2*(dcset-1)+1) - 
     $           hfpp_prune_zslop(dcset)/tan(theta) .and.
     $           zclose.le.hfpp_prune_zclose(2*(dcset-1)+2) + 
     $           hfpp_prune_zslop(dcset)/tan(theta)

c     try picking smallest theta HERE instead:
            if(hselectfpptrackprune.eq.1) then
               criterion = theta
               any_great = .false.
            else if(hselectfpptrackprune.eq.2) then
               criterion = chi2 + minchi2 * sclose**2
            else if(hselectfpptrackprune.eq.3) then
               criterion = chi2
            else if(hselectfpptrackprune.eq.4) then
               criterion = sclose
            else
               criterion = chi2
            endif

            any_great = .false.

c            if(candidate_nplanes(track).eq.h_fpp_n_dcinset*
c     $           h_fpp_n_dclayers.or..not.any6) then
            if( greattrack(track).or. .not. any_great) then
               if(candidate_nplanes(track).eq.6.or..not.any6) then
                  if(firsttry.or.criterion.lt.mincriterion) then
                     mincriterion = criterion
                     firsttry = .false.
                     bestcandidate = track
                  endif
               endif
            endif
         enddo
        
c$$$         if(goodhms) then
c$$$            write(*,*) 'chosen track=',bestcandidate
c$$$         endif
         if(hidFPP_trkrough(DCset,1).gt.0) call hf1(hidFPP_trkrough(DCset,1),SimpleTracks(bestcandidate,1),1.) !mx  
         if(hidFPP_trkrough(DCset,2).gt.0) call hf1(hidFPP_trkrough(DCset,2),SimpleTracks(bestcandidate,2),1.) !bx  
         if(hidFPP_trkrough(DCset,3).gt.0) call hf1(hidFPP_trkrough(DCset,3),SimpleTracks(bestcandidate,3),1.) !my  
         if(hidFPP_trkrough(DCset,4).gt.0) call hf1(hidFPP_trkrough(DCset,4),SimpleTracks(bestcandidate,4),1.) !by  
         if(hidFPP_trkrough(DCset,5).gt.0) call hf1(hidFPP_trkrough(DCset,5),SimpleTracks(bestcandidate,5),1.) !chi2
         if(hidFPP_trkrough(DCset,6).gt.0) call hf1(hidFPP_trkrough(DCset,6),SimpleTracks(bestcandidate,6),1.) !Nraw

         if(hidFPP_roughchi2vsnhit(dcset).gt.0) call hf2(hidFPP_roughchi2vsnhit(dcset),simpletracks(bestcandidate,6),
     $        simpletracks(bestcandidate,5),1.)

         do i=1,ncandidates
            do ichamber=1,h_fpp_n_dcinset
               do ilayer=1,h_fpp_n_dclayers
                  icluster = hitcombos(i,ichamber,ilayer)
                  if(icluster.gt.0) then ! mark all candidate hits as unused
                     hfpp_clusterintrack(dcset,ichamber,ilayer,icluster) = 0
c     reset drift time and drift distance for all wire hits in the cluster:
c     If they are not used in a track, do not store their drift time and distance:
                     do ihit=1,hfpp_nhitsincluster(dcset,ichamber,ilayer,icluster)
                        hit = hfpp_clusters(dcset,ichamber,ilayer,icluster,ihit)
                        wire = hfpp_raw_wire(hit)
                        hfpp_drift_time(dcset,ichamber,ilayer,wire) = h_fpp_bad_time
                        hfpp_drift_dist(dcset,ichamber,ilayer,wire) = h_fpp_bad_drift
                     enddo
                  endif
               enddo
            enddo
         enddo
         
c         write(*,*) 'best candidate track, chi2=',bestcandidate,fulltracks(bestcandidate,5)
c     write(*,*) 'nplanes,nhits=',candidate_nplanes(bestcandidate),
c     $        candidate_nhits(bestcandidate)
         
c$$$  do i=1,4
c$$$  write(*,*) 'i,besttrack(i)=',i,fulltracks(bestcandidate,i)
c$$$  enddo

         if(bestcandidate.eq.0) exit

c     fit the track one more time to get the correct drift distance, as it may have changed:
c     also, improve drift time/dist calculation using the full track:
         do j=1,6
            simpletrack(j) = simpletracks(bestcandidate,j)
c     write(*,*) 'i,simpletrack(i)=',j,simpletrack(j)
         enddo
         
         do ichamber=1,h_fpp_n_dcinset
            do ilayer=1,h_fpp_n_dclayers
               bestclusters(ichamber,ilayer) = hitcombos(bestcandidate,ichamber,ilayer)
c     write(*,*) 'chbr,pln,clstr=',ichamber,ilayer,bestclusters(ichamber,ilayer)
            enddo
         enddo
         
c         write(*,*) 'before refitting, track=',(fulltracks(bestcandidate,j),j=1,6)

         call h_fpp_tracking_drifttrack_ajp(dcset,simpletrack,bestclusters,
     $        ontrack,track_good,fulltrack,nhitsrequired,1,abort,err)

         if(.not. track_good) exit
         
         do j=1,6
            fulltracks(bestcandidate,j) = fulltrack(j)
         enddo

c         write(*,*) 'after refitting, track=',(fulltracks(bestcandidate,j),j=1,6)
         
c     Now that we have found the best candidate track, add it to the good track array:

         itrack = hfpp_n_tracks(dcset) + 1
         
         if(itrack.le.h_fpp_max_tracks) then
            
            hfpp_n_tracks(dcset) = itrack

            hfpp_track_nlayers(dcset,itrack) = candidate_nplanes(bestcandidate)
c            hfpp_track_nlayers(dcset,itrack) = 

            do j=1,4
               hfpp_track_fine(dcset,itrack,j) = fulltracks(bestcandidate,j)
            enddo

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

c$$$            hfpp_track_dx(dcset,itrack) = fpcoords(1)
c$$$            hfpp_track_dy(dcset,itrack) = fpcoords(2)

            fpptrack(3) = fpcoords(1) ! x' in fp coords
            fpptrack(4) = fpcoords(2) ! y' in fp coords

*     now transform coordinates from local FPP system to HMS fp coords:

            dccoords(1) = fulltracks(bestcandidate,2) ! x
            dccoords(2) = fulltracks(bestcandidate,4) ! y
            dccoords(3) = 0.0

            call h_fpp_dc2fp(dcset,.false.,dccoords,fpcoords)

c$$$            hfpp_track_x(dcset,itrack) = fpcoords(1) - fpcoords(3)*
c$$$     $           hfpp_track_dx(dcset,itrack)
c$$$            hfpp_track_y(dcset,itrack) = fpcoords(2) - fpcoords(3)*
c$$$     $           hfpp_track_dy(dcset,itrack)

            fpptrack(1) = fpcoords(1) - fpcoords(3) * fpptrack(3) ! x in fp coords at z=0
            fpptrack(2) = fpcoords(2) - fpcoords(3) * fpptrack(4) ! y in fp coords at z=0

            call h_fpp_align(dcset,fpptrack,newfpptrack)

            hfpp_track_x(dcset,itrack) = newfpptrack(1)
            hfpp_track_y(dcset,itrack) = newfpptrack(2)
            hfpp_track_dx(dcset,itrack) = newfpptrack(3)
            hfpp_track_dy(dcset,itrack) = newfpptrack(4)

*     calculate relative scattering angles theta and phi:

            call h_fpp_relative_angles(hsxp_fp,hsyp_fp,hfpp_track_dx(dcset,itrack),
     $           hfpp_track_dy(dcset,itrack),theta,phi)

            hfpp_track_theta(dcset,itrack) = theta
            hfpp_track_phi(dcset,itrack) = phi

*     calculate closest approach parameters with respect to HMS, REGARDLESS of which FPP!

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

c               firsttry=.true.
               bestref = 0

c               mintheta = hfpp_track_theta(dcset,itrack)
c               do jtrack=1,hfpp_n_tracks(1)
c     how this works: always figure out the "best reference" track for this FPP2 track in FPP1: 
c     after that, make a subjective judgement, based on theta, of whether or not to compare this track 
c     to FPP1 or the HMS:
               if(hfpp_n_tracks(1).gt.0) then
                  do jtrack = 1, hfpp_n_tracks(1)
                     call h_fpp_relative_angles(hfpp_track_dx(1,jtrack),
     $                    hfpp_track_dy(1,jtrack),
     $                    hfpp_track_dx(dcset,itrack),
     $                    hfpp_track_dy(dcset,itrack),
     $                    theta,phi)
                     if(jtrack.eq.1.or.theta.lt.mintheta) then
                        bestref = jtrack
                        mintheta = theta
                     endif
                  enddo
               endif

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

               hfpp2_best_reference(itrack) = bestref

               if(bestref.gt.0.and.hfpp_track_theta(dcset,itrack).lt.
     $              hfpp_track_theta(dcset+1,itrack)) hfpp2_best_reference(itrack) = 0

            endif
            
            if(hfppfixleftright.gt.0) 
     $           call h_fpp_check_leftright(dcset,itrack)

         endif                  ! end filling track common block variables
c     NOW we need to call freehitcount and start over with the remaining free hits:
         
         call h_fpp_tracking_freehitcount(dcset,sufficient_hits)

      enddo

      return
      end

      subroutine h_fpp_tracking_simple_ajp(dcset,hitcombos,simpletracks,
     $     ncandidates,nhitsrequired,abort,err)

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
      integer*4 n3hit,n2hit ! tabulate number of three-hit and two hit clusters.

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

c$$$         do ichamber=1,h_fpp_n_dcinset
c$$$            do ilayer=1,h_fpp_n_dclayers
c$$$               hitcluster(ichamber,ilayer) = 0
c$$$            enddo
c$$$         enddo

         call h_fpp_tracking_nexthitcombo(dcset,nhitsrequired,nhitsintrack,hitcluster)

*     keep comparing permutations until all possibilities are tried:

         do while(nhitsintrack.gt.0.and.ncandidates.lt.h_fpp_max_candidates)

            iterations = iterations + 1

            if(iterations.gt.hfpp_maxcombos) then
               write(*,*) 'WARNING: max FPP hit combos reached.'
               write(*,*) 'consider raising the limit or using '
               write(*,*) 'tighter raw timing cuts'
               exit
            endif

            call h_fpp_fit_simple(dcset,hitcluster,npoints,temptrack,abort,err)

            if (ABORT) then
               call g_add_path(here,err)
               return
            endif

            n2hit = 0
            n3hit = 0

            do ichamber=1,h_fpp_n_dcinset
               do ilayer=1,h_fpp_n_dclayers
                  if(hitcluster(ichamber,ilayer).gt.0) then
                     if(hfpp_nhitsincluster(dcset,ichamber,ilayer,
     $                    hitcluster(ichamber,ilayer)).eq.2) n2hit = n2hit + 1
                     if(hfpp_nhitsincluster(dcset,ichamber,ilayer,hitcluster(ichamber,ilayer))
     $                    .eq.3) n3hit = n3hit + 1
                  endif
               enddo
            enddo
*     in contrast to Frank's algorithm, here we keep track of any hit combos which pass the "reasonable chi2"
*     criterion, and we don't choose a track until we look at the drift based tracking.
            
c$$$            if(temptrack(5).ge.0.0.and.
c$$$     $           temptrack(5).le.hfpp_aok_chi2+
c$$$     $           (12.*float(n2hit)+48.*float(n3hit))/float(nhitsrequired-4)/
c$$$     $           float(nhitsrequired-4 + n2hit + 2*n3hit)
c$$$     $           ) then           ! add a new candidate track to the array:
            if(temptrack(5).ge.0.0.and.temptrack(5).le.hfpp_aok_chi2) 
     $           then
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

      real*4 function prob(chbr,angle)
      
      integer*4 chbr
      real*4 angle
c     approximate cumulative probability that a track scatters in fpp with 
c     an angle theta < angle
c      prob = min(1.0e-9,1.0 - .1662*angle**(-0.2202)*exp(-4.031*angle**0.981))

      if(chbr.eq.1) prob = exp(-2.946*angle)
      else prob = exp(-3.485*angle)

      return
      end
      
      subroutine h_fpp_check_leftright(ifpp,itrack)

      implicit none
      save

      include 'gen_detectorids.par'
      include 'gen_decode_common.cmn'
      include 'gen_event_info.cmn'
      INCLUDE 'hms_data_structures.cmn'
      INCLUDE 'hms_geometry.cmn'
      INCLUDE 'hms_fpp_event.cmn'
      INCLUDE 'hms_fpp_params.cmn'
      INCLUDE 'hms_id_histid.cmn'

c     fortunately, this is made easier by the fact that we know which clusters are on a track and 
c     we know which hits within a cluster are on a track from their drift distance variable, so 
c     we can revisit the left-right combo here:

      integer*4 nambig,ambigcombos(64)
      integer*4 sign,oldsign,j
      integer*4 npoints,ncombos,point,ipoint,icombo,combo
      integer*4 ifpp,itrack,track,jtrack
      integer*4 ichamber,ilayer,iwire,wire
      integer*4 icluster,cluster,ihit,hit
      integer*4 oldbestcombo,newbestcombo
      real*4 combochi2(64)
      real*4 combotheta(64)
      real*4 combophi(64)
      real*4 combosclose(64)
      real*4 combozclose(64)
      integer*4 comboconetest(64)
      logical ambig(64),anyambig,first

      real*4 chi2,theta,phi,sclose,zclose
      real*4 oldchi2,oldtheta,oldphi,oldsclose,oldzclose
   
      integer*4 oldconetest,conetest

      real*4 plusminustest(h_fpp_max_fitpoints)
      real*4 plusminusbest(h_fpp_max_fitpoints)

      integer*4 chambers(h_fpp_max_fitpoints)
      integer*4 layers(h_fpp_max_fitpoints)
      integer*4 wires(h_fpp_max_fitpoints)

      real*4 points(h_fpp_max_fitpoints,2) ! w and z
      real*4 drifts(h_fpp_max_fitpoints) ! drift distance
      real*4 projects(h_fpp_max_fitpoints,2) ! Px and Py coefficients
      real*4 sigma2s(h_fpp_max_fitpoints)  ! sigma^2 for chi2 calc.
      real*4 hitpos(h_fpp_max_fitpoints,2) ! arrays for track fitting routine

      real*4 dccoords(3),fpcoords(3)
      real*4 reftrack(4),fpptrack(4),hmstrack(4),newfpptrack(4)

      real*4 testtrack(5)

      real*4 minsclose,mintheta
      real*4 minstest,minthetatest,maxambig,maxdtheta,maxthetadiff
      real*4 wtrack,xtrack,ytrack,xptrack,yptrack,Px,Py,z
      real*4 zmin,zmax

      external jbit             ! cernlib bit routine
      external jibset
      integer*4 jbit
      integer*4 jibset          ! Declare to help f2c

      hfpp_track_nambig(ifpp,itrack) = 0

      if(itrack.le.0.or.itrack.gt.hfpp_n_tracks(ifpp).or.
     $     hfpp_track_nhits(ifpp,itrack).ne.
     $     hfpp_track_nlayers(ifpp,itrack)) return
      
*     initialize points on the track:

c      write(*,*) 'event = ',gen_event_id_number

      npoints = 0

      oldbestcombo = 0

      xptrack = hfpp_track_fine(ifpp,itrack,1)
      xtrack = hfpp_track_fine(ifpp,itrack,2)
      yptrack = hfpp_track_fine(ifpp,itrack,3)
      ytrack = hfpp_track_fine(ifpp,itrack,4)

      do ichamber=1,h_fpp_n_dcinset
         do ilayer=1,h_fpp_n_dclayers
            icluster = hfpp_trackcluster(ifpp,ichamber,ilayer,itrack)

            if(icluster.gt.0) then
               do ihit=1,hfpp_nhitsincluster(ifpp,ichamber,ilayer,icluster)
                  hit = hfpp_clusters(ifpp,ichamber,ilayer,icluster,ihit)
                  
                  wire = hfpp_raw_wire(hit)
               
                  if(hfpp_drift_dist(ifpp,ichamber,ilayer,wire).ne.h_fpp_bad_drift) 
     $                 then     ! this hit is on the track:
                     npoints = npoints + 1
                     if(npoints.le.h_fpp_max_fitpoints) then
                        points(npoints,1) = hfpp_layeroffset(ifpp,ichamber,ilayer)
     $                       + hfpp_spacing(ifpp,ichamber,ilayer)*float(wire)
                        points(npoints,2) = hfpp_layerz(ifpp,ichamber,ilayer)
                        sigma2s(npoints) = hfpp_resolution(ifpp,ichamber,ilayer)
                        projects(npoints,1) = hfpp_direction(ifpp,ichamber,ilayer,1)
                        projects(npoints,2) = hfpp_direction(ifpp,ichamber,ilayer,2)
                        drifts(npoints) = abs(hfpp_drift_dist(ifpp,ichamber,ilayer,wire))
                        
                        chambers(npoints) = ichamber
                        layers(npoints) = ilayer
                        wires(npoints) = wire

                        Px = projects(npoints,1)
                        Py = projects(npoints,2)

                        z = points(npoints,2)

                        wtrack = Px*(xtrack + xptrack*z) + Py*(ytrack + yptrack*z)

                        if(abs(wtrack - points(npoints,1) - drifts(npoints)) 
     $                       .le.abs(wtrack - points(npoints,1) + drifts(npoints)) )
     $                       then ! track crosses on + side of wire-->assume hit originally had a + sign!
                           oldbestcombo = oldbestcombo + 2**(npoints-1)
                        endif

                     endif
                  endif
               enddo
            endif
         enddo
      enddo
         
      if(npoints.ne.hfpp_track_nhits(ifpp,itrack)) return
      if(npoints.lt.6) return

      oldchi2 = hfpp_track_chi2(ifpp,itrack)
      oldtheta = hfpp_track_theta(ifpp,itrack)
      oldphi = hfpp_track_phi(ifpp,itrack)
      oldsclose = hfpp_track_sclose(ifpp,itrack)
      oldzclose = hfpp_track_zclose(ifpp,itrack)
      oldconetest = hfpp_track_conetest(ifpp,itrack)

      reftrack(1) = hsxp_fp
      reftrack(2) = hsx_fp
      reftrack(3) = hsyp_fp
      reftrack(4) = hsy_fp

      if(ifpp.eq.2.and.hfpp2_best_reference(itrack).gt.0) then
         track = hfpp2_best_reference(itrack)

         oldtheta = hfpp_track_theta(ifpp+1,itrack)
         oldphi = hfpp_track_phi(ifpp+1,itrack)
         oldsclose = hfpp_track_sclose(ifpp+1,itrack)
         oldzclose = hfpp_track_zclose(ifpp+1,itrack)
         oldconetest = hfpp_track_conetest(ifpp+1,itrack)

         reftrack(1) = hfpp_track_dx(1,track)
         reftrack(2) = hfpp_track_x(1,track)
         reftrack(3) = hfpp_track_dy(1,track)
         reftrack(4) = hfpp_track_y(1,track)
         
      endif

c     now we have baseline against which to compare other left-right combos. 

      zmin = hfpp_prune_zclose(2*(ifpp-1)+1) 
      zmax = hfpp_prune_zclose(2*(ifpp-1)+2)

      ncombos = 2**npoints

      first = .true.

      newbestcombo = -1

      maxthetadiff = 0.0

      minsclose = oldsclose
      mintheta = oldtheta

      nambig = 0

      do icombo=0,ncombos-1
         
         ambig(icombo+1) = .false.

         if(icombo.ne.oldbestcombo) then
            do ipoint=1,npoints
               if(jbit(icombo,ipoint).eq.1) then
                  plusminustest(ipoint) = 1.0
               else 
                  plusminustest(ipoint) = -1.0
               endif
               
               hitpos(ipoint,1) = points(ipoint,1) + 
     $              drifts(ipoint)*plusminustest(ipoint)
               hitpos(ipoint,2) = points(ipoint,2)

            enddo

            call h_fpp_fit3d_ajp(npoints,hitpos,sigma2s,projects,testtrack)

            chi2 = testtrack(5)

            combochi2(icombo+1) = chi2

            if(chi2.le.hfpp_min_chi2.and.chi2-oldchi2.le.hfpp_ambig_chi2cut(1)
     $           .and.chi2/oldchi2.le.hfpp_ambig_chi2cut(2)) then ! "ambiguity"

c     first check whether any hits on this combo which disagree with the best chi2 combo have 
c     "large" drift distances, i.e., big enough that changing the sign makes a non-trivial difference in track
c     reconstruction:

               anyambig = .false.
               
               maxambig = 0.0

               do ipoint=1,npoints
                  sign = jbit(icombo,ipoint)
                  oldsign = jbit(oldbestcombo,ipoint)
                  if(sign.ne.oldsign.and.
     $                 abs(drifts(ipoint)).ge.1.25*sqrt(sigma2s(ipoint)))
     $                 then
                     anyambig = .true.

                     if(2.0*drifts(ipoint).gt.maxambig) then
                        maxambig = 2.0*drifts(ipoint)
                     endif

                  endif
               enddo

c     maximum angular displacement of new track from old track is approximately equal to atan(maxambig/dz)
               
               maxdtheta = atan(maxambig/abs(points(npoints,2)-points(1,2)) )
c$$$               write(*,*) 
c$$$     $              'max. possible angular displacement this track=',maxdtheta
               
               if(maxdtheta.gt.maxthetadiff) maxthetadiff = maxdtheta

               if(anyambig) then
                  ambig(icombo+1) = .true.
c     this combo is "ambiguous": similar chi2 to the best combo and differs in sign from the best
c     combo in at least one "large" drift distance hit. Calculate theta,phi,sclose,and zclose:
                  
c     first, transform from dc to fp coords:

                  nambig = nambig + 1
                  ambigcombos(nambig) = icombo

                  dccoords(1) = testtrack(1) ! dx/dz
                  dccoords(2) = testtrack(3) ! dy/dz
                  dccoords(3) = 1.0          ! dz/dz

                  call h_fpp_dc2fp(ifpp,.true.,dccoords,fpcoords)

                  fpptrack(3) = fpcoords(1) ! dx/dz
                  fpptrack(4) = fpcoords(2) ! dy/dz
                  
                  dccoords(1) = testtrack(2) ! x
                  dccoords(2) = testtrack(4) ! y
                  dccoords(3) = 0.0          ! z
                  
                  call h_fpp_dc2fp(ifpp,.false.,dccoords,fpcoords)

                  fpptrack(1) = fpcoords(1) - fpcoords(3)*fpptrack(3) ! x - x'z
                  fpptrack(2) = fpcoords(2) - fpcoords(3)*fpptrack(4) ! y - y'z

                  call h_fpp_align(ifpp,fpptrack,newfpptrack)

                  fpptrack(1) = newfpptrack(3)
                  fpptrack(2) = newfpptrack(1)
                  fpptrack(3) = newfpptrack(4)
                  fpptrack(4) = newfpptrack(2)

                  call h_fpp_relative_angles(reftrack(1),reftrack(3),fpptrack(1),fpptrack(3),theta,phi)
                  call h_fpp_closest(reftrack,fpptrack,sclose,zclose)
                  conetest = 1
                  call h_fpp_conetest(reftrack,ifpp,zclose,theta,conetest)

                  if(theta.lt.mintheta) mintheta = theta
                  if(sclose.lt.minsclose) minsclose = sclose

                  combotheta(icombo+1) = theta
                  combophi(icombo+1) = phi
                  combosclose(icombo+1) = sclose
                  combozclose(icombo+1) = zclose
                  comboconetest(icombo+1) = conetest
             
               endif
            endif
         endif
      enddo

      if(nambig.gt.0.and.(mintheta.lt.oldtheta.or.minsclose.lt.oldsclose)) 
     $     then

c         hfpp_track_nambig(ifpp,itrack) = nambig
         
c     reset minsclose and oldsclose:
         if(mintheta.le.hfpp_ambig_smallthetatest) then ! track consistent with theta=0: 
c     use smallest theta regardless of chi2,sclose,zclose:
            minthetatest = mintheta
            mintheta = oldtheta
            do icombo=1,nambig
               combo = ambigcombos(icombo)+1
               if(combotheta(combo).lt.mintheta) then
                  newbestcombo = combo-1
                  mintheta = combotheta(combo)
               endif
            enddo
         else ! track not consistent with theta=0: use sclose instead:
            minstest = hfpp_ambig_sclosetest
            do icombo=1,nambig
               combo = ambigcombos(icombo)+1

               zmin = hfpp_prune_zclose(2*(ifpp-1)+1) - hfpp_prune_zslop(ifpp)/tan(combotheta(combo))
               zmax = hfpp_prune_zclose(2*(ifpp-1)+2) + hfpp_prune_zslop(ifpp)/tan(combotheta(combo))

               if( (combosclose(combo)/oldsclose)**2.lt.minstest .and.
     $              zmin.lt.combozclose(combo).and.
     $              zmax.gt.combozclose(combo) ) then
                  minstest = (combosclose(combo)/oldsclose)**2
                  newbestcombo = combo-1
               endif
            enddo
         endif
      endif

      if(newbestcombo.ge.0) then

         hfpp_track_nambig(ifpp,itrack) = nambig
c     refit the track using the "new best combo"

c$$$         write(*,*) 'found new best combo FPP,itrack=',ifpp,itrack
c$$$         write(*,*) 'nhits=',npoints
c$$$         write(*,*) 'old combo, nambig = ',oldbestcombo,nambig
c$$$         write(*,*) 'old chi2,sclose,zclose,theta,phi,conetest=',oldchi2,oldsclose,
c$$$     $        oldzclose,oldtheta,oldphi,oldconetest

         do ipoint=1,npoints
            if(jbit(newbestcombo,ipoint).eq.1) then
               plusminusbest(ipoint) = 1.0
            else
               plusminusbest(ipoint) = -1.0
            endif

            ichamber = chambers(ipoint)
            ilayer = layers(ipoint)
            iwire = wires(ipoint)

            hfpp_drift_dist(ifpp,ichamber,ilayer,iwire) = 
     $           plusminusbest(ipoint)*drifts(ipoint)

            hitpos(ipoint,1) = points(ipoint,1) + 
     $           plusminusbest(ipoint)*drifts(ipoint)
            hitpos(ipoint,2) = points(ipoint,2)
         enddo

         call h_fpp_fit3d_ajp(npoints,hitpos,sigma2s,projects,testtrack)
c     copy the chi2 of the new track to common block variables:
         hfpp_track_chi2(ifpp,itrack) = testtrack(5)

         do j=1,4
            hfpp_track_fine(ifpp,itrack,j) = testtrack(j)
         enddo

c     re-calculate everything that was modified by this routine:

         dccoords(1) = testtrack(1) ! dx/dz
         dccoords(2) = testtrack(3) ! dy/dz
         dccoords(3) = 1.0          ! dz/dz

         call h_fpp_dc2fp(ifpp,.true.,dccoords,fpcoords)
         
         fpptrack(3) = fpcoords(1)
         fpptrack(4) = fpcoords(2)
         
         dccoords(1) = testtrack(2) ! x
         dccoords(2) = testtrack(4) ! y
         dccoords(3) = 0.0          ! z
         
         call h_fpp_dc2fp(ifpp,.false.,dccoords,fpcoords)

         fpptrack(1) = fpcoords(1) - fpcoords(3)*fpptrack(3) ! x - x'z
         fpptrack(2) = fpcoords(2) - fpcoords(3)*fpptrack(4) ! y - y'z

         call h_fpp_align(ifpp,fpptrack,newfpptrack)

         fpptrack(1) = newfpptrack(3)
         fpptrack(2) = newfpptrack(1)
         fpptrack(3) = newfpptrack(4)
         fpptrack(4) = newfpptrack(2)

         hfpp_track_dx(ifpp,itrack) = fpptrack(1)
         hfpp_track_x(ifpp,itrack) = fpptrack(2)
         hfpp_track_dy(ifpp,itrack) = fpptrack(3)
         hfpp_track_y(ifpp,itrack) = fpptrack(4)

         hmstrack(1) = hsxp_fp
         hmstrack(2) = hsx_fp
         hmstrack(3) = hsyp_fp
         hmstrack(4) = hsy_fp

         call h_fpp_relative_angles(hmstrack(1),hmstrack(3),fpptrack(1),
     $        fpptrack(3),theta,phi)

         hfpp_track_theta(ifpp,itrack) = theta
         hfpp_track_phi(ifpp,itrack) = phi
         
         call h_fpp_closest(hmstrack,fpptrack,sclose,zclose)
         
         hfpp_track_sclose(ifpp,itrack) = sclose
         hfpp_track_zclose(ifpp,itrack) = zclose

         conetest = 1 
         call h_fpp_conetest(hmstrack,ifpp,zclose,theta,conetest)

         hfpp_track_conetest(ifpp,itrack) = conetest

         if(ifpp.eq.2.and.hfpp2_best_reference(itrack).gt.0) then
c     reftrack has already been initialized to the best track chosen from
c     FPP1:
            reftrack(1) = hfpp_track_dx(1,hfpp2_best_reference(itrack))
            reftrack(2) = hfpp_track_x(1,hfpp2_best_reference(itrack))
            reftrack(3) = hfpp_track_dy(1,hfpp2_best_reference(itrack))
            reftrack(4) = hfpp_track_y(1,hfpp2_best_reference(itrack))

            call h_fpp_relative_angles(reftrack(1),reftrack(3),fpptrack(1),
     $           fpptrack(3),theta,phi)
            hfpp_track_theta(ifpp+1,itrack) = theta
            hfpp_track_phi(ifpp+1,itrack) = phi
           
            call h_fpp_closest(reftrack,fpptrack,sclose,zclose)
            
            hfpp_track_sclose(ifpp+1,itrack) = sclose
            hfpp_track_zclose(ifpp+1,itrack) = zclose

            conetest = 1
            call h_fpp_conetest(reftrack,ifpp,zclose,theta,conetest)

            hfpp_track_conetest(ifpp+1,itrack) = conetest

         endif    
c$$$         write(*,*) 'new combo=',newbestcombo
c$$$         write(*,*) 'new chi2,sclose,zclose,theta,phi,conetest=',testtrack(5),sclose,zclose,theta,phi,conetest
        
      endif

      return
      end
