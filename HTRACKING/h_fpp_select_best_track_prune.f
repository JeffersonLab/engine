      subroutine h_fpp_select_best_track_prune(abort,err)

      implicit none
      save

      character*29 here
      parameter(here='h_fpp_select_best_track_prune')

      logical abort 
      character*(*) err

      include 'hms_data_structures.cmn'
      include 'gen_detectorids.par'
      include 'gen_decode_common.cmn'
      include 'hms_fpp_event.cmn'
      include 'hms_geometry.cmn'
      include 'hms_fpp_params.cmn'

      integer ifpp,itrack ! = 1, 2 for FPP1, FPP2
      integer ngood(2)
      integer besttrack(2)
      integer icone

      integer nplanesprune,bestreferencetrack

      real zslop ! theta-dependent tolerance parameter for zclose

      real*4 chi2,minchi2(2)
      real*4 criterion,mincriterion(2),scloseweight(2)
      real*4 ztest(2,2)
      real*4 theta,phi,sclose,zclose
      
      real*4 trackin(4),trackout(4)

      real*4 PI
      parameter(PI=3.141592653)

      logical firsttry

      logical keep(2,h_fpp_max_tracks)

c     what prune tests should we include? 
c     definitely sclose, but also zclose:
c     prune_thetamin and prune_thetamax:
c     if multiple tracks pass prune tests, 
c     use Sitnik's selection: select on minimum of 
c     chi2(HMS) + chi2(FPP) + weight * sclose^2,
c     where weight = chi2(HMS) + chi2best(FPP)

c     after learning a little bit about this tracking algorithm, it is clear that we also want to prune on the
c     number of hits on a track--many of the bad tracks are five-hit tracks. But should we do this test before or 
c     after the theta prune test, conetest, sclose and zclose pruning? Seems like AFTER is the way to go.

c     first step: initialize keep on all tracks to true:
c     while we're at it, find the track with minimum chi2 in each FPP:


      do ifpp=1,2
c         firsttry=.true.
         do itrack=1,hfpp_n_tracks(ifpp)
            keep(ifpp,itrack) = .true.

c            chi2 = hfpp_track_chi2(ifpp,itrack)
c            if(firsttry.or.chi2.lt.minchi2(ifpp)) then
c               minchi2(ifpp) = chi2
c               firsttry = .false.
c            endif

         enddo
      enddo

c     zeroth prune test = conetest
c     for FPP2, check conetest using either HMS or FPP1 as reference track
      do ifpp=1,2
         ngood(ifpp) = 0
         do itrack=1,hfpp_n_tracks(ifpp)
            if(hfpp_track_conetest(ifpp,itrack).eq.1.and.
     $           keep(ifpp,itrack)) then
               ngood(ifpp) = ngood(ifpp) + 1
            else if(ifpp.eq.2.and.hfpp2_best_reference(itrack).gt.0.and.
     $              hfpp_track_conetest(ifpp+1,itrack).eq.1.and.
     $              keep(ifpp,itrack)) then
               ngood(ifpp) = ngood(ifpp) + 1
            endif
         enddo
         
         if(ngood(ifpp).gt.0) then
            do itrack=1,hfpp_n_tracks(ifpp)
               if(hfpp_track_conetest(ifpp,itrack).eq.0) then
                  if(ifpp.eq.1) then 
                     keep(ifpp,itrack) = .false.
                  else if(hfpp2_best_reference(itrack).eq.0.or.
     $                    hfpp_track_conetest(ifpp+1,itrack).eq.0) then
                     keep(ifpp,itrack) = .false.
                  endif
               endif
            enddo
         endif
      enddo
c$$$  c     fifth prune test: number of layers on a track: if we have six, throw out all other five-hit
c$$$  c     tracks
c$$$  
      nplanesprune = h_fpp_n_dcinset * hfpp_optchamberhits
c$$$  
      do ifpp=1,2
         ngood(ifpp) = 0
         do itrack=1,hfpp_n_tracks(ifpp)
            if(keep(ifpp,itrack).and.hfpp_track_nlayers(ifpp,itrack).ge.
     $           nplanesprune) then
               ngood(ifpp) = ngood(ifpp) + 1
            endif
         enddo
         
         if(ngood(ifpp).gt.0) then
            do itrack=1,hfpp_n_tracks(ifpp)
               if(hfpp_track_nlayers(ifpp,itrack).lt.nplanesprune) then
                  keep(ifpp,itrack) = .false.
               endif
            enddo
         endif
      enddo
      

c     first prune tests: prune separately on minimum and maximum 
c     polar scattering angle theta>thetamin:
c     for FPP2, check using either FPP1 or HMS as reference track:
      do ifpp=1,2
         ngood(ifpp) = 0
         do itrack=1,hfpp_n_tracks(ifpp)
            if(hfpp_track_theta(ifpp,itrack).ge.hfpp_prune_thetamin(ifpp)*PI/180.0
     $           .and.keep(ifpp,itrack))then
               ngood(ifpp) = ngood(ifpp) + 1
            else if(ifpp.eq.2.and.hfpp2_best_reference(itrack).gt.0.and.
     $              hfpp_track_theta(ifpp+1,itrack).ge.
     $              hfpp_prune_thetamin(ifpp)*PI/180.0.and.
     $              keep(ifpp,itrack)) then
               ngood(ifpp) = ngood(ifpp) + 1
            endif
         enddo

         if(ngood(ifpp).gt.0) then
            do itrack=1,hfpp_n_tracks(ifpp)
               if(hfpp_track_theta(ifpp,itrack).lt.hfpp_prune_thetamin(ifpp)*PI/180.0) 
     $              then
                  if(ifpp.eq.1) then
                     keep(ifpp,itrack) = .false.
                  else if(hfpp2_best_reference(itrack).eq.0.or.
     $                    hfpp_track_theta(ifpp+1,itrack).lt.
     $                    hfpp_prune_thetamin(ifpp)*PI/180.0) then
                     keep(ifpp,itrack) = .false.
                  endif
               endif
            enddo
         endif
      enddo

c     second test: polar scattering angle theta<thetamax:
c     for FPP2, check using either FPP1 or HMS as reference track:      
      do ifpp=1,2
         ngood(ifpp) = 0
         do itrack=1,hfpp_n_tracks(ifpp)
            if(keep(ifpp,itrack).and.hfpp_track_theta(ifpp,itrack).le.
     $           hfpp_prune_thetamax(ifpp)*PI/180.0) then
               ngood(ifpp) = ngood(ifpp) + 1
            else if(ifpp.eq.2.and.hfpp2_best_reference(itrack).gt.0.and.
     $              hfpp_track_theta(ifpp+1,itrack).le.
     $              hfpp_prune_thetamax(ifpp)*PI/180.0.and.
     $              keep(ifpp,itrack)) then
               ngood(ifpp) = ngood(ifpp) + 1
            endif
         enddo
         
         if(ngood(ifpp).gt.0) then
            do itrack=1,hfpp_n_tracks(ifpp)
               if(hfpp_track_theta(ifpp,itrack).gt.hfpp_prune_thetamax(ifpp)*PI/180.0)
     $              then
                  if(ifpp.eq.1) then
                     keep(ifpp,itrack) = .false.
                  else if(hfpp2_best_reference(itrack).eq.0.or.
     $                    hfpp_track_theta(ifpp+1,itrack).gt.
     $                    hfpp_prune_thetamax(ifpp)*PI/180.0) then
                     keep(ifpp,itrack) = .false.
                  endif
               endif
            enddo
         endif
      enddo
      
c     third prune test: sclose = distance of closest approach between two tracks:
      
      do ifpp=1,2
         ngood(ifpp) = 0
         do itrack=1,hfpp_n_tracks(ifpp)
            if(keep(ifpp,itrack).and.hfpp_track_sclose(ifpp,itrack).le.
     $           hfpp_prune_sclose(ifpp)) then
               ngood(ifpp) = ngood(ifpp) + 1
            else if(keep(ifpp,itrack).and.ifpp.eq.2.and.
     $              hfpp2_best_reference(itrack).gt.0.and.
     $              hfpp_track_sclose(ifpp+1,itrack).le.
     $              hfpp_prune_sclose(1)) then
               ngood(ifpp) = ngood(ifpp) + 1
            endif
         enddo
         
         if(ngood(ifpp).gt.0) then
            do itrack=1,hfpp_n_tracks(ifpp)
               if(hfpp_track_sclose(ifpp,itrack).gt.
     $              hfpp_prune_sclose(ifpp)) then
                  if(ifpp.eq.1) then
                     keep(ifpp,itrack) = .false.
                  else if(hfpp2_best_reference(itrack).eq.0.or.
     $                    hfpp_track_sclose(ifpp+1,itrack).gt.
     $                    hfpp_prune_sclose(1)) then
                     keep(ifpp,itrack) = .false.
                  endif
               endif
            enddo
         endif
      enddo

c     fourth prune test: zclose = z of point of closest approach between the two tracks:
c     ideally want zclose within the analyzer. However, we don't measure zclose very well at 
c     small values of theta. zclose resolution diverges as tan^-1 theta at small theta
c     We would like to have a "slop" parameter for zclose that 
c     allows for zclose values well outside the analyzer in the situation that theta is small:

      do ifpp=1,2
         
         ztest(ifpp,1) = hfpp_prune_zclose(2*(ifpp-1)+1)
         ztest(ifpp,2) = hfpp_prune_zclose(2*(ifpp-1)+2)

         ngood(ifpp) = 0
         do itrack=1,hfpp_n_tracks(ifpp)
            zslop = hfpp_prune_zslop(ifpp) * 
     $           max(1.0,min(1000.0,1.0/tan(hfpp_track_theta(ifpp,itrack))))
            if(keep(ifpp,itrack).and.hfpp_track_zclose(ifpp,itrack).ge.
     $           ztest(ifpp,1)-zslop.and.
     $           hfpp_track_zclose(ifpp,itrack).le.
     $           ztest(ifpp,2)+zslop) then
               ngood(ifpp) = ngood(ifpp) + 1
            else if(keep(ifpp,itrack).and.
     $              hfpp2_best_reference(itrack).gt.0.and.
     $              hfpp_track_zclose(ifpp+1,itrack).ge.ztest(ifpp,1)-zslop
     $              .and.hfpp_track_zclose(ifpp+1,itrack).le.ztest(ifpp,2)+zslop)
     $              then
               ngood(ifpp) = ngood(ifpp) + 1
            endif
         enddo

         if(ngood(ifpp).gt.0) then
            do itrack=1,hfpp_n_tracks(ifpp)
               zslop = hfpp_prune_zslop(ifpp) * 
     $              max(1.0,min(1000.0,1.0/tan(hfpp_track_theta(ifpp,itrack))))
               if(hfpp_track_zclose(ifpp,itrack).lt.
     $              ztest(ifpp,1)-zslop.or.
     $              hfpp_track_zclose(ifpp,itrack).gt.
     $              ztest(ifpp,2)+zslop) then
                  if(ifpp.eq.1) then
                     keep(ifpp,itrack) = .false.
                  else if(hfpp2_best_reference(itrack).eq.0.or.
     $                    hfpp_track_zclose(ifpp+1,itrack).lt.
     $                    ztest(ifpp,1)-zslop.or.
     $                    hfpp_track_zclose(ifpp+1,itrack).gt.
     $                    ztest(ifpp,2)+zslop) then
                     keep(ifpp,itrack) = .false.
                  endif
               endif
            enddo
         endif
      enddo

c     NOW find the minimum chi2 of tracks with keep==true

c$$$      do ifpp=1,2
c$$$         firsttry=.true.
c$$$         do itrack=1,hfpp_n_tracks(ifpp)
c$$$            if(keep(ifpp,itrack).and.(hfpp_track_chi2(ifpp,itrack)
c$$$     $           .lt.minchi2(ifpp).or.firsttry)) then
c$$$               firsttry = .false.
c$$$               minchi2(ifpp) = hfpp_track_chi2(ifpp,itrack)
c$$$            endif
c$$$         enddo
c$$$      enddo

c     now we choose the best track based on smallest polar scattering angle theta

      do ifpp=1,2
         firsttry=.true.
c     scloseweight(ifpp) = minchi2(ifpp) + hschi2perdeg
         
         besttrack(ifpp) = 0

         do itrack=1,hfpp_n_tracks(ifpp)
c     criterion = hschi2perdeg + hfpp_track_chi2(ifpp,itrack) + 
c     $           scloseweight(ifpp) * (hfpp_track_sclose(ifpp,itrack))**2
c     instead of this criterion, do track selection based on smallest theta:
            criterion = hfpp_track_theta(ifpp,itrack)
c     for FPP2, compare theta of current track with both HMS and FPP1 as reference track:
c     routine will select the single track from FPP2 whose theta wrt EITHER the HMS track or 
c     any FPP1 track is minimum.
            if(ifpp.eq.2) criterion = 
     $           min(hfpp_track_theta(ifpp,itrack),hfpp_track_theta(ifpp+1,itrack))

            if(keep(ifpp,itrack).and.
     $           (firsttry.or.criterion.lt.mincriterion(ifpp))) then
               mincriterion(ifpp) = criterion
               firsttry = .false.
               besttrack(ifpp) = itrack
            endif
            
c$$$            if(ifpp.eq.2.and.hfpp_best_track(1).gt.0) then 
c$$$c     calculate and store theta/phi/sclose and zclose and conetest of FPP2 track 
c$$$c     relative to "best" FPP1 track for comparison with the same quantities relative to 
c$$$c     HMS track:
c$$$               call h_fpp_relative_angles(hfpp_track_dx(1,hfpp_best_track(1)),
c$$$     $              hfpp_track_dy(1,hfpp_best_track(1)),hfpp_track_dx(ifpp,itrack),
c$$$     $              hfpp_track_dy(ifpp,itrack),theta,phi)
c$$$               hfpp_track_theta(ifpp+1,itrack) = theta
c$$$               hfpp_track_phi(ifpp+1,itrack) = phi
c$$$               
c$$$               trackin(1) = hfpp_track_dx(1,hfpp_best_track(1))
c$$$               trackin(2) = hfpp_track_x(1,hfpp_best_track(1))
c$$$               trackin(3) = hfpp_track_dy(1,hfpp_best_track(1))
c$$$               trackin(4) = hfpp_track_y(1,hfpp_best_track(1))
c$$$               
c$$$               trackout(1) = hfpp_track_dx(ifpp,itrack)
c$$$               trackout(2) = hfpp_track_x(ifpp,itrack)
c$$$               trackout(3) = hfpp_track_dy(ifpp,itrack)
c$$$               trackout(4) = hfpp_track_y(ifpp,itrack)
c$$$               
c$$$               call h_fpp_closest(trackin,trackout,sclose,zclose)
c$$$               
c$$$               hfpp_track_sclose(ifpp+1,itrack) = sclose
c$$$               hfpp_track_zclose(ifpp+1,itrack) = zclose
c$$$               
c$$$               icone = 1
c$$$               
c$$$               call h_fpp_conetest(trackin,ifpp,zclose,theta,icone)
c$$$               
c$$$               hfpp_track_conetest(ifpp+1,itrack) = icone
c$$$               
c$$$            endif

         enddo
         
         hfpp_best_track(ifpp) = besttrack(ifpp)

      enddo

      abort = .false.
      err=''

      return
      end
