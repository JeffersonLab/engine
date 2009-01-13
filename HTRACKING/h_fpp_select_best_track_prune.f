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
      integer ngood(3)
      integer besttrack(2)
      integer icone

      integer nplanesprune,bestreferencetrack

      real zslop ! theta-dependent tolerance parameter for zclose

      real*4 chi2,minchi2(3)
      real*4 criterion,mincriterion(2),scloseweight,scloseweight3
      real*4 ztest(2,2)
      real*4 theta,phi,sclose,zclose
      
      real*4 trackin(4),trackout(4)

      real*4 PI
      parameter(PI=3.141592653)

      logical firsttry

      logical keep(3,h_fpp_max_tracks)

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
            if(ifpp.eq.2) keep(3,itrack) = .false.
            if(ifpp.eq.2.and.hfpp2_best_reference(itrack).gt.0) 
     $           keep(3,itrack) = .true.
         enddo
      enddo

c     zeroth prune test = conetest
c     for FPP2, check conetest using either HMS or FPP1 as reference track
      do ifpp=1,2
         ngood(ifpp) = 0
         ngood(3) = 0
         do itrack=1,hfpp_n_tracks(ifpp)
            if(hfpp_track_conetest(ifpp,itrack).eq.1.and.
     $           keep(ifpp,itrack)) then
               ngood(ifpp) = ngood(ifpp) + 1
            endif
            
            if(ifpp.eq.2.and.keep(3,itrack).and.
     $           hfpp_track_conetest(3,itrack).eq.1) then
               ngood(3) = ngood(3) + 1
            endif
         enddo
         
         if(ngood(ifpp).gt.0) then
            do itrack=1,hfpp_n_tracks(ifpp)
               if(hfpp_track_conetest(ifpp,itrack).eq.0) then 
                  keep(ifpp,itrack) = .false.
               endif
            enddo
         endif

         if(ifpp.eq.2.and.ngood(3).gt.0) then
            do itrack=1,hfpp_n_tracks(2)
               if(hfpp_track_conetest(3,itrack).eq.0) then
                  keep(3,itrack) = .false.
               endif
            enddo
         endif
      enddo
c     fifth prune test: number of layers on a track: if we have six, throw out all other five-hit
c     tracks
  
      nplanesprune = h_fpp_n_dcinset * hfpp_optchamberhits
c$$$  
      if(hfpp_prune_nplanes.ne.0) then ! apply this test iff flag is set.
         do ifpp=1,2
            ngood(ifpp) = 0
            ngood(3) = 0
            do itrack=1,hfpp_n_tracks(ifpp)
               if(keep(ifpp,itrack).and.hfpp_track_nlayers(ifpp,itrack).ge.
     $              nplanesprune) then
                  ngood(ifpp) = ngood(ifpp) + 1
               endif

               if(ifpp.eq.2.and.keep(3,itrack).and.hfpp_track_nlayers(ifpp,itrack)
     $              .ge.nplanesprune) then
                  ngood(3) = ngood(3) + 1
               endif
            enddo
            
            if(ngood(ifpp).gt.0) then
               do itrack=1,hfpp_n_tracks(ifpp)
                  if(hfpp_track_nlayers(ifpp,itrack).lt.nplanesprune) then
                     keep(ifpp,itrack) = .false.
                  endif
               enddo
            endif

            if(ifpp.eq.2.and.ngood(3).gt.0) then
               do itrack=1,hfpp_n_tracks(2)
                  if(hfpp_track_nlayers(ifpp,itrack).lt.nplanesprune) then
                     keep(3,itrack) = .false.
                  endif
               enddo
            endif
         enddo
      endif

c     first prune tests: prune separately on minimum and maximum 
c     polar scattering angle theta>thetamin:
c     for FPP2, check using either FPP1 or HMS as reference track:
      do ifpp=1,2
         ngood(ifpp) = 0
         ngood(3) = 0
         do itrack=1,hfpp_n_tracks(ifpp)
            if(hfpp_track_theta(ifpp,itrack).ge.hfpp_prune_thetamin(ifpp)*PI/180.0
     $           .and.keep(ifpp,itrack))then
               ngood(ifpp) = ngood(ifpp) + 1
            endif
            
            if(ifpp.eq.2.and.keep(3,itrack).and.
     $           hfpp_track_theta(3,itrack).ge.hfpp_prune_thetamin(1)*PI/180.0)
     $           then
               ngood(3) = ngood(3) + 1
            endif
         enddo

         if(ngood(ifpp).gt.0) then
            do itrack=1,hfpp_n_tracks(ifpp)
               if(hfpp_track_theta(ifpp,itrack).lt.hfpp_prune_thetamin(ifpp)*PI/180.0) 
     $              then
                  keep(ifpp,itrack) = .false.
               endif
            enddo
         endif
         
         if(ifpp.eq.2.and.ngood(3).gt.0) then
            do itrack=1,hfpp_n_tracks(2)
               if(hfpp_track_theta(3,itrack).lt.hfpp_prune_thetamin(1)*PI/180.0)
     $              then
                  keep(3,itrack) = .false.
               endif
            enddo
         endif
      enddo

c     second test: polar scattering angle theta<thetamax:
c     for FPP2, check using either FPP1 or HMS as reference track:      
      do ifpp=1,2
         ngood(ifpp) = 0
         ngood(3) = 0
         do itrack=1,hfpp_n_tracks(ifpp)
            if(keep(ifpp,itrack).and.hfpp_track_theta(ifpp,itrack).le.
     $           hfpp_prune_thetamax(ifpp)*PI/180.0) then
               ngood(ifpp) = ngood(ifpp) + 1
            endif
            if(ifpp.eq.2.and.keep(3,itrack).and.hfpp_track_theta(3,itrack)
     $           .le.hfpp_prune_thetamax(1)*PI/180.0) then
               ngood(3) = ngood(3) + 1
            endif
         enddo
         
         if(ngood(ifpp).gt.0) then
            do itrack=1,hfpp_n_tracks(ifpp)
               if(hfpp_track_theta(ifpp,itrack).gt.hfpp_prune_thetamax(ifpp)*PI/180.0)
     $              then
                  keep(ifpp,itrack) = .false.
               endif
            enddo
         endif

         if(ifpp.eq.2.and.ngood(3).gt.0) then
            do itrack=1,hfpp_n_tracks(2)
               if(hfpp_track_theta(3,itrack).gt.hfpp_prune_thetamax(1)*PI/180.0)
     $              then
                  keep(3,itrack) = .false.
               endif
            enddo
         endif
      enddo
      
c     third prune test: sclose = distance of closest approach between two tracks:
      
      do ifpp=1,2
         ngood(ifpp) = 0
         ngood(3) = 0
         do itrack=1,hfpp_n_tracks(ifpp)
            if(keep(ifpp,itrack).and.hfpp_track_sclose(ifpp,itrack).le.
     $           hfpp_prune_sclose(ifpp)) then
               ngood(ifpp) = ngood(ifpp) + 1
            endif
            if(ifpp.eq.2.and.keep(3,itrack).and.hfpp_track_sclose(3,itrack)
     $           .le.hfpp_prune_sclose(1)) then
               ngood(3) = ngood(3) + 1
            endif
         enddo
         
         if(ngood(ifpp).gt.0) then
            do itrack=1,hfpp_n_tracks(ifpp)
               if(hfpp_track_sclose(ifpp,itrack).gt.
     $              hfpp_prune_sclose(ifpp)) then
                  keep(ifpp,itrack) = .false.
               endif
            enddo
         endif

         if(ifpp.eq.2.and.ngood(3).gt.0) then
            do itrack=1,hfpp_n_tracks(2)
               if(hfpp_track_sclose(3,itrack).gt.
     $              hfpp_prune_sclose(1)) then
                  keep(3,itrack) = .false.
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

c         write(*,*) 'FPP,zlow,zhigh=',ifpp,ztest(ifpp,1),ztest(ifpp,2)

         ngood(ifpp) = 0
         ngood(3) = 0
         do itrack=1,hfpp_n_tracks(ifpp)
            zslop = hfpp_prune_zslop(ifpp) * 
     $           max(1.0,min(1000.0,1.0/tan(hfpp_track_theta(ifpp,itrack))))
c            write(*,*) 'FPP,zslop=',ifpp,zslop

            if(keep(ifpp,itrack).and.hfpp_track_zclose(ifpp,itrack).ge.
     $           ztest(ifpp,1)-zslop.and.
     $           hfpp_track_zclose(ifpp,itrack).le.
     $           ztest(ifpp,2)+zslop) then
               ngood(ifpp) = ngood(ifpp) + 1
            endif

            if(ifpp.eq.2) zslop = hfpp_prune_zslop(ifpp) * 
     $           max(1.0,min(1000.0,1.0/tan(hfpp_track_theta(3,itrack))))
            
            if(ifpp.eq.2.and.keep(3,itrack).and.
     $           hfpp_track_zclose(3,itrack).ge.ztest(2,1)-zslop.and.
     $           hfpp_track_zclose(3,itrack).le.ztest(2,2)+zslop) then
               ngood(3) = ngood(3) + 1
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
                  keep(ifpp,itrack) = .false.
               endif
            enddo
         endif
         
         if(ifpp.eq.2.and.ngood(3).gt.0) then
            do itrack=1,hfpp_n_tracks(2)
               zslop = hfpp_prune_zslop(2) * 
     $              max(1.0,min(1000.0,1.0/tan(hfpp_track_theta(3,itrack))))
               if(hfpp_track_zclose(3,itrack).lt.
     $              ztest(2,1)-zslop.or.
     $              hfpp_track_zclose(3,itrack).gt.
     $              ztest(2,2)+zslop) then
                  keep(3,itrack) = .false.
               endif
            enddo
         endif
      enddo

c     NOW find the minimum chi2 of tracks with keep==true
      if(hselectfpptrackprune.eq.2) then ! use Sitnik's criterion for track selection instead of smallest theta:
         do ifpp=1,2
            firsttry=.true.
            do itrack=1,hfpp_n_tracks(ifpp)
               if(keep(ifpp,itrack).and.(hfpp_track_chi2(ifpp,itrack)
     $              .lt.minchi2(ifpp).or.firsttry)) then
                  firsttry = .false.
                  minchi2(ifpp) = hfpp_track_chi2(ifpp,itrack)
               endif
            enddo
         enddo
         firsttry=.true.
         do itrack=1,hfpp_n_tracks(2)
            if(keep(3,itrack).and.(hfpp_track_chi2(2,itrack).lt.
     $           minchi2(3).or.firsttry) ) then
               firsttry = .false.
               minchi2(3) = hfpp_track_chi2(2,itrack)
            endif
         enddo
      endif
c     now we choose the best track based on smallest polar scattering angle theta
c     need to treat FPP1 and FPP2 differently:
c     for FPP1, the selection method is simply smallest theta wrt the incident HMS track:

      firsttry = .true.

      besttrack(1) = 0

      ngood(1) = 0

      do itrack=1,hfpp_n_tracks(1)
         if(hselectfpptrackprune.eq.2) then
            scloseweight = hschi2perdeg + minchi2(1)
            criterion = hfpp_track_chi2(1,itrack) + hschi2perdeg + 
     $           scloseweight * (hfpp_track_sclose(1,itrack))**2
         else
            criterion = hfpp_track_theta(1,itrack)
         endif
         
         if(keep(1,itrack).and.
     $        (firsttry.or.criterion.lt.mincriterion(1)) ) then
            firsttry = .false.
            besttrack(1) = itrack
            mincriterion(1) = criterion
         endif
         if(keep(1,itrack)) ngood(1) = ngood(1) + 1
      enddo
            
      hfpp_n_goodtracks(1) = ngood(1)

      besttrack(2) = 0

      firsttry = .true.

      ngood(2) = 0

      do itrack=1,hfpp_n_tracks(2)
         if(keep(2,itrack).and.keep(3,itrack)) then ! either HMS or FPP1 could be the best reference track
            if(hselectfpptrackprune.eq.2) then
               scloseweight = hschi2perdeg + minchi2(2)
               scloseweight3 = hfpp_track_chi2(1,hfpp2_best_reference(itrack)) + 
     $              minchi2(3)
               criterion = min( hfpp_track_chi2(2,itrack)+hschi2perdeg + 
     $              scloseweight * (hfpp_track_sclose(2,itrack))**2, 
     $              hfpp_track_chi2(2,itrack)+hfpp_track_chi2(1,hfpp2_best_reference(itrack))
     $              + scloseweight3 * (hfpp_track_sclose(3,itrack))**2 )
               if(hfpp_track_chi2(2,itrack)+hschi2perdeg + scloseweight * 
     $              (hfpp_track_sclose(2,itrack))**2.lt.hfpp_track_chi2(2,itrack) + 
     $              hfpp_track_chi2(1,hfpp2_best_reference(itrack)) + scloseweight3 * 
     $              (hfpp_track_sclose(3,itrack))**2 ) then
                  hfpp2_best_reference(itrack) = 0
               endif
            else
               criterion = min(hfpp_track_theta(2,itrack),
     $              hfpp_track_theta(3,itrack) )
               if(hfpp_track_theta(2,itrack).lt.
     $              hfpp_track_theta(3,itrack)) then ! if thetaHMS < thetaFPP1, use HMS as reference
                  hfpp2_best_reference(itrack) = 0
               endif
            endif
         else if(keep(2,itrack)) then ! HMS track is unambiguously the best reference track for this track
            if(hselectfpptrackprune.eq.2) then
               scloseweight = hschi2perdeg + minchi2(2)
               criterion = hschi2perdeg + hfpp_track_chi2(2,itrack) +
     $              scloseweight * (hfpp_track_sclose(2,itrack))**2
            else
               criterion = hfpp_track_theta(2,itrack)
            endif
            hfpp2_best_reference(itrack) = 0
         else if(keep(3,itrack)) then ! FPP1 is unambiguously the best reference track for this track
            if(hselectfpptrackprune.eq.2) then
               scloseweight = hfpp_track_chi2(1,hfpp2_best_reference(itrack)) + 
     $              minchi2(3)
               criterion = hfpp_track_chi2(1,hfpp2_best_reference(itrack)) + 
     $              hfpp_track_chi2(2,itrack) + scloseweight * 
     $              (hfpp_track_sclose(3,itrack))**2
            else
               criterion = hfpp_track_theta(3,itrack)
            endif
         endif
         if( (keep(2,itrack).or.keep(3,itrack) ).and.
     $        (firsttry.or.criterion.lt.mincriterion(2)) ) then
            mincriterion(2) = criterion
            firsttry = .false.
            besttrack(2) = itrack
         endif
         
         if(keep(2,itrack).or.keep(3,itrack) ) ngood(2) = ngood(2) + 1
         
      enddo
      
      hfpp_n_goodtracks(2) = ngood(2)
      
      do ifpp=1,2
         hfpp_best_track(ifpp) = besttrack(ifpp)
      enddo
      
      abort = .false.
      err=''
      
      return
      end
