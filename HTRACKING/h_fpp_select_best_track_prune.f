      subroutine h_fpp_select_best_track_prune(ifpp,abort,err)

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

      integer jfpp

      integer trksmallesttheta,trksmallestchi2,trksmallestsclose

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
c     after the theta prune test, conetest, sclose and zclose pruning? Seems like BEFORE is the way to go.

c     first step: initialize keep on all tracks to true:
c     while we're at it, find the track with minimum chi2 in each FPP:


c      do ifpp=1,2
c         firsttry=.true.
      do itrack=1,hfpp_n_tracks(ifpp)
         keep(ifpp,itrack) = .true.
         
c     chi2 = hfpp_track_chi2(ifpp,itrack)
c     if(firsttry.or.chi2.lt.minchi2(ifpp)) then
c     minchi2(ifpp) = chi2
c     firsttry = .false.
c     endif
c$$$         if(ifpp.eq.2) keep(3,itrack) = .false.
c$$$         if(ifpp.eq.2.and.hfpp2_best_reference(itrack).gt.0) 
c$$$     $        keep(3,itrack) = .true.
      enddo
c      enddo

      jfpp = ifpp
c      if(ifpp.eq.2.and.hfpp_n_tracks(1).gt.0) jfpp = ifpp+1

c     first, prune the number of planes on the track: tracks with only five hits are not
c     nearly as useful as six-hit tracks:

      nplanesprune = h_fpp_n_dcinset * hfpp_optchamberhits
c$$$  
      if(hfpp_prune_nplanes.ne.0) then ! apply this test iff flag is set.
c         do ifpp=1,2
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
c     enddo
      endif

c     second prune test = conetest
c     for FPP2, check conetest using either HMS or FPP1 as reference track
c      do ifpp=1,2
      ngood(ifpp) = 0
c      ngood(3) = 0
      do itrack=1,hfpp_n_tracks(ifpp)
         jfpp = ifpp
         if(ifpp.eq.2.and.hfpp2_best_reference(itrack).gt.0) jfpp = ifpp+1
         if(hfpp_track_conetest(jfpp,itrack).eq.1.and.
     $        keep(ifpp,itrack)) then
            ngood(ifpp) = ngood(ifpp) + 1
         endif
         
c$$$         if(ifpp.eq.2.and.keep(3,itrack).and.
c$$$     $        hfpp_track_conetest(3,itrack).eq.1) then
c$$$            ngood(3) = ngood(3) + 1
c$$$         endif
      enddo
      
      if(ngood(ifpp).gt.0) then
         do itrack=1,hfpp_n_tracks(ifpp)
            jfpp = ifpp
            if(ifpp.eq.2.and.hfpp2_best_reference(itrack).gt.0) jfpp = ifpp+1
            if(hfpp_track_conetest(jfpp,itrack).eq.0) then 
               keep(ifpp,itrack) = .false.
            endif
         enddo
      endif
      
c$$$      if(ngood(3).gt.0) then
c$$$         do itrack=1,hfpp_n_tracks(2)
c$$$            if(hfpp_track_conetest(3,itrack).eq.0) then
c$$$               keep(3,itrack) = .false.
c$$$            endif
c$$$         enddo
c$$$      endif
c     enddo

c     third prune tests: prune separately on minimum and maximum 
c     polar scattering angle theta>thetamin:
c     for FPP2, check using either FPP1 or HMS as reference track:
c      do ifpp=1,2
      ngood(ifpp) = 0
c      ngood(3) = 0
      do itrack=1,hfpp_n_tracks(ifpp)
         jfpp = ifpp
         if(ifpp.eq.2.and.hfpp2_best_reference(itrack).gt.0) jfpp = ifpp+1
         if(hfpp_track_theta(jfpp,itrack).ge.hfpp_prune_thetamin(ifpp)*PI/180.0
     $        .and.keep(ifpp,itrack))then
            ngood(ifpp) = ngood(ifpp) + 1
         endif
         
c$$$         if(ifpp.eq.2.and.keep(3,itrack).and.
c$$$     $        hfpp_track_theta(3,itrack).ge.hfpp_prune_thetamin(1)*PI/180.0)
c$$$     $        then
c$$$            ngood(3) = ngood(3) + 1
c$$$         endif
      enddo

      if(ngood(ifpp).gt.0) then
         do itrack=1,hfpp_n_tracks(ifpp)
            jfpp = ifpp
            if(ifpp.eq.2.and.hfpp2_best_reference(itrack).gt.0) jfpp = ifpp+1
            if(hfpp_track_theta(jfpp,itrack).lt.hfpp_prune_thetamin(ifpp)*PI/180.0) 
     $           then
               keep(ifpp,itrack) = .false.
            endif
         enddo
      endif
      
c$$$      if(ngood(3).gt.0) then
c$$$         do itrack=1,hfpp_n_tracks(2)
c$$$            if(hfpp_track_theta(3,itrack).lt.hfpp_prune_thetamin(2)*PI/180.0)
c$$$     $           then
c$$$               keep(3,itrack) = .false.
c$$$            endif
c$$$         enddo
c$$$      endif
c      enddo

c     fourth test: polar scattering angle theta<thetamax:
c     for FPP2, check using either FPP1 or HMS as reference track:      
c      do ifpp=1,2
      ngood(ifpp) = 0
c      ngood(3) = 0
      do itrack=1,hfpp_n_tracks(ifpp)
         jfpp = ifpp
         if(ifpp.eq.2.and.hfpp2_best_reference(itrack).gt.0) jfpp = ifpp+1
         if(keep(ifpp,itrack).and.hfpp_track_theta(jfpp,itrack).le.
     $        hfpp_prune_thetamax(ifpp)*PI/180.0) then
            ngood(ifpp) = ngood(ifpp) + 1
         endif
c$$$         if(ifpp.eq.2.and.keep(3,itrack).and.hfpp_track_theta(3,itrack)
c$$$     $        .le.hfpp_prune_thetamax(1)*PI/180.0) then
c$$$            ngood(3) = ngood(3) + 1
c$$$         endif
      enddo
      
      if(ngood(ifpp).gt.0) then
         do itrack=1,hfpp_n_tracks(ifpp)
            jfpp = ifpp
            if(ifpp.eq.2.and.hfpp2_best_reference(itrack).gt.0) jfpp = ifpp+1
            if(hfpp_track_theta(jfpp,itrack).gt.hfpp_prune_thetamax(ifpp)*PI/180.0)
     $           then
               keep(ifpp,itrack) = .false.
            endif
         enddo
      endif
      
c$$$      if(ngood(3).gt.0) then
c$$$         do itrack=1,hfpp_n_tracks(2)
c$$$            if(hfpp_track_theta(3,itrack).gt.hfpp_prune_thetamax(1)*PI/180.0)
c$$$     $           then
c$$$               keep(3,itrack) = .false.
c$$$            endif
c$$$         enddo
c$$$      endif
c      enddo
      
c     fifth prune test: sclose = distance of closest approach between two tracks:
      
c      do ifpp=1,2
      ngood(ifpp) = 0
c      ngood(3) = 0
      do itrack=1,hfpp_n_tracks(ifpp)
         jfpp = ifpp
         if(ifpp.eq.2.and.hfpp2_best_reference(itrack).gt.0) jfpp = ifpp+1
         if(keep(ifpp,itrack).and.hfpp_track_sclose(jfpp,itrack).le.
     $        hfpp_prune_sclose(ifpp)) then
            ngood(ifpp) = ngood(ifpp) + 1
         endif
c$$$         if(ifpp.eq.2.and.keep(3,itrack).and.hfpp_track_sclose(3,itrack)
c$$$     $        .le.hfpp_prune_sclose(1)) then
c$$$            ngood(3) = ngood(3) + 1
c$$$         endif
      enddo
      
      if(ngood(ifpp).gt.0) then
         do itrack=1,hfpp_n_tracks(ifpp)
            jfpp = ifpp
            if(ifpp.eq.2.and.hfpp2_best_reference(itrack).gt.0) jfpp = ifpp+1
            if(hfpp_track_sclose(jfpp,itrack).gt.
     $           hfpp_prune_sclose(ifpp)) then
               keep(ifpp,itrack) = .false.
            endif
         enddo
      endif
      
c$$$      if(ngood(3).gt.0) then
c$$$         do itrack=1,hfpp_n_tracks(2)
c$$$            if(hfpp_track_sclose(3,itrack).gt.
c$$$     $           hfpp_prune_sclose(1)) then
c$$$               keep(3,itrack) = .false.
c$$$            endif
c$$$         enddo
c$$$      endif
c      enddo

c     sixth prune test: zclose = z of point of closest approach between the two tracks:
c     ideally want zclose within the analyzer. However, we don't measure zclose very well at 
c     small values of theta. zclose resolution diverges as tan^-1 theta at small theta
c     We would like to have a "slop" parameter for zclose that 
c     allows for zclose values well outside the analyzer in the situation that theta is small:

c     do ifpp=1,2
      
      ztest(ifpp,1) = hfpp_prune_zclose(2*(ifpp-1)+1)
      ztest(ifpp,2) = hfpp_prune_zclose(2*(ifpp-1)+2)
      
c     write(*,*) 'FPP,zlow,zhigh=',ifpp,ztest(ifpp,1),ztest(ifpp,2)
      
      ngood(ifpp) = 0
c      ngood(3) = 0
      do itrack=1,hfpp_n_tracks(ifpp)
         jfpp = ifpp
         if(ifpp.eq.2.and.hfpp2_best_reference(itrack).gt.0) jfpp = ifpp+1

         zslop = hfpp_prune_zslop(ifpp) * 
     $        max(1.0,min(1000.0,1.0/tan(hfpp_track_theta(jfpp,itrack))))
c     write(*,*) 'FPP,zslop=',ifpp,zslop

         if(keep(ifpp,itrack).and.hfpp_track_zclose(jfpp,itrack).ge.
     $        ztest(ifpp,1)-zslop.and.
     $        hfpp_track_zclose(jfpp,itrack).le.
     $        ztest(ifpp,2)+zslop) then
            ngood(ifpp) = ngood(ifpp) + 1
         endif
         
c$$$         if(ifpp.eq.2) zslop = hfpp_prune_zslop(ifpp) * 
c$$$     $        max(1.0,min(1000.0,1.0/tan(hfpp_track_theta(3,itrack))))
c$$$         
c$$$         if(ifpp.eq.2.and.keep(3,itrack).and.
c$$$     $        hfpp_track_zclose(3,itrack).ge.ztest(2,1)-zslop.and.
c$$$     $        hfpp_track_zclose(3,itrack).le.ztest(2,2)+zslop) then
c$$$            ngood(3) = ngood(3) + 1
c$$$         endif
      enddo
      
      if(ngood(ifpp).gt.0) then
         do itrack=1,hfpp_n_tracks(ifpp)
            jfpp = ifpp
            if(ifpp.eq.2.and.hfpp2_best_reference(itrack).gt.0) jfpp = ifpp+1
            zslop = hfpp_prune_zslop(ifpp) * 
     $           max(1.0,min(1000.0,1.0/tan(hfpp_track_theta(jfpp,itrack))))
            if(hfpp_track_zclose(jfpp,itrack).lt.
     $           ztest(ifpp,1)-zslop.or.
     $           hfpp_track_zclose(jfpp,itrack).gt.
     $           ztest(ifpp,2)+zslop) then
               keep(ifpp,itrack) = .false.
            endif
         enddo
      endif
      
c$$$      if(ngood(3).gt.0) then
c$$$         do itrack=1,hfpp_n_tracks(2)
c$$$            zslop = hfpp_prune_zslop(2) * 
c$$$     $           max(1.0,min(1000.0,1.0/tan(hfpp_track_theta(3,itrack))))
c$$$            if(hfpp_track_zclose(3,itrack).lt.
c$$$     $           ztest(2,1)-zslop.or.
c$$$     $           hfpp_track_zclose(3,itrack).gt.
c$$$     $           ztest(2,2)+zslop) then
c$$$               keep(3,itrack) = .false.
c$$$            endif
c$$$         enddo
c$$$      endif
c     enddo

c     NOW find the minimum chi2 of tracks with keep==true
      if(hselectfpptrackprune.eq.2) then ! use Sitnik's criterion for track selection instead of smallest theta:
c         do ifpp=1,2
         firsttry=.true.
         do itrack=1,hfpp_n_tracks(ifpp)
            if(keep(ifpp,itrack).and.(hfpp_track_chi2(ifpp,itrack)
     $           .lt.minchi2(ifpp).or.firsttry)) then
               firsttry = .false.
               minchi2(ifpp) = hfpp_track_chi2(ifpp,itrack)
            endif
         enddo
c     enddo
c$$$         firsttry=.true.
c$$$         do itrack=1,hfpp_n_tracks(2)
c$$$            if(keep(3,itrack).and.(hfpp_track_chi2(2,itrack).lt.
c$$$     $           minchi2(3).or.firsttry) ) then
c$$$               firsttry = .false.
c$$$               minchi2(3) = hfpp_track_chi2(2,itrack)
c$$$            endif
c$$$         enddo
      endif
c     now we choose the best track based on smallest polar scattering angle theta
c     need to treat FPP1 and FPP2 differently:
c     for FPP1, the selection method is simply smallest theta wrt the incident HMS track:
      
      firsttry = .true.
      
      besttrack(ifpp) = 0
      
      ngood(ifpp) = 0
      
c     write(*,*) 'Entering final FPP1 track selection'
c      if(ifpp.eq.1) then
      do itrack=1,hfpp_n_tracks(ifpp)
         jfpp = ifpp
         if(ifpp.eq.2.and.hfpp2_best_reference(itrack).gt.0) jfpp = ifpp+1
         if(hselectfpptrackprune.eq.2) then ! use "Sitnik method"
c            scloseweight = hschi2perdeg + minchi2(ifpp)
            scloseweight = minchi2(ifpp)
c            criterion = hfpp_track_chi2(ifpp,itrack) + hschi2perdeg + 
c     $           scloseweight * (hfpp_track_sclose(jfpp,itrack))**2
            criterion = hfpp_track_chi2(ifpp,itrack) +
     $           scloseweight * (hfpp_track_sclose(jfpp,itrack))**2
         else if(hselectfpptrackprune.eq.3) then ! use smallest chi2 track selection
            criterion = hfpp_track_chi2(ifpp,itrack)
         else if(hselectfpptrackprune.eq.4.or.
     $           hselectfpptrackprune.eq.6) then         ! use smallest sclose track selection
            criterion = hfpp_track_sclose(jfpp,itrack)
         else                   ! use smallest theta track selection
            criterion = hfpp_track_theta(jfpp,itrack)
         endif
         
c$$$  if(hfpp_n_tracks(1).gt.1) then
c$$$  write(*,*) 'FPP1 track ',itrack,' theta=',hfpp_track_theta(1,itrack)*180./PI,
c$$$  $           ' chi2=',hfpp_track_chi2(1,itrack),' sclose=',hfpp_track_sclose(1,itrack),
c$$$  $           ' zclose=',hfpp_track_zclose(1,itrack),' phi=',hfpp_track_phi(1,itrack)*180./PI,
c$$$  $           ' nplanes=',hfpp_track_nlayers(1,itrack),' nhits=',hfpp_track_nhits(1,itrack)
c$$$  endif
         
         if(keep(ifpp,itrack).and.
     $        (firsttry.or.criterion.lt.mincriterion(ifpp)) ) then
            firsttry = .false.
            besttrack(ifpp) = itrack
            mincriterion(ifpp) = criterion
         endif
         if(keep(ifpp,itrack)) ngood(ifpp) = ngood(ifpp) + 1
      enddo
      hfpp_n_goodtracks(ifpp) = ngood(ifpp)
      
c$$$      else
c$$$
c$$$c         besttrack(2) = 0
c$$$         
c$$$c         firsttry = .true.
c$$$         
c$$$c         ngood(2) = 0
c$$$         
c$$$         do itrack=1,hfpp_n_tracks(2)
c$$$            if(keep(2,itrack).and.keep(3,itrack)) then ! either HMS or FPP1 could be the best reference track
c$$$               if(hselectfpptrackprune.eq.2) then ! use "Sitnik method"
c$$$                  scloseweight = hschi2perdeg + minchi2(2)
c$$$                  scloseweight3 = hfpp_track_chi2(1,hfpp2_best_reference(itrack)) + 
c$$$     $                 minchi2(3)
c$$$                  criterion = min( hfpp_track_chi2(2,itrack)+hschi2perdeg + 
c$$$     $                 scloseweight * (hfpp_track_sclose(2,itrack))**2, 
c$$$     $                 hfpp_track_chi2(2,itrack)+hfpp_track_chi2(1,hfpp2_best_reference(itrack))
c$$$     $                 + scloseweight3 * (hfpp_track_sclose(3,itrack))**2 )
c$$$                  if(hfpp_track_chi2(2,itrack)+hschi2perdeg + scloseweight * 
c$$$     $                 (hfpp_track_sclose(2,itrack))**2.lt.hfpp_track_chi2(2,itrack) + 
c$$$     $                 hfpp_track_chi2(1,hfpp2_best_reference(itrack)) + scloseweight3 * 
c$$$     $                 (hfpp_track_sclose(3,itrack))**2 ) then
c$$$                     hfpp2_best_reference(itrack) = 0
c$$$                  endif
c$$$               else if(hselectfpptrackprune.eq.3) then ! use smallest chi2 track selection
c$$$                  criterion = hfpp_track_chi2(2,itrack)               
c$$$                  if(hfpp_track_theta(2,itrack).lt.
c$$$     $                 hfpp_track_theta(3,itrack)) then ! if thetaHMS < thetaFPP1, use HMS as reference
c$$$                     hfpp2_best_reference(itrack) = 0
c$$$                  endif
c$$$               else if(hselectfpptrackprune.eq.4) then ! use smallest sclose track selection
c$$$                  criterion = min(hfpp_track_sclose(2,itrack),
c$$$     $                 hfpp_track_sclose(3,itrack))
c$$$                  if(hfpp_track_sclose(2,itrack).lt.
c$$$     $                 hfpp_track_sclose(3,itrack)) then
c$$$                     hfpp2_best_reference(itrack) = 0
c$$$                  endif
c$$$c                  criterion = hfpp_track_sclose(2,itrack)
c$$$
c$$$               else             ! use smallest theta track selection
c$$$                  criterion = min(hfpp_track_theta(2,itrack),
c$$$     $                 hfpp_track_theta(3,itrack) )
c$$$                  if(hfpp_track_theta(2,itrack).lt.
c$$$     $                 hfpp_track_theta(3,itrack)) then ! if thetaHMS < thetaFPP1, use HMS as reference
c$$$                     hfpp2_best_reference(itrack) = 0
c$$$                  endif
c$$$
c$$$c                  criterion = hfpp_track_theta(2,itrack)
c$$$
c$$$               endif
c$$$            else if(keep(2,itrack)) then ! HMS track is unambiguously the best reference track for this track
c$$$               if(hselectfpptrackprune.eq.2) then
c$$$                  scloseweight = hschi2perdeg + minchi2(2)
c$$$                  criterion = hschi2perdeg + hfpp_track_chi2(2,itrack) +
c$$$     $                 scloseweight * (hfpp_track_sclose(2,itrack))**2
c$$$               else if(hselectfpptrackprune.eq.3) then ! use smallest chi2 track
c$$$                  criterion = hfpp_track_chi2(2,itrack)
c$$$               else if(hselectfpptrackprune.eq.4) then ! use smallest sclose track
c$$$                  criterion = hfpp_track_sclose(2,itrack)
c$$$               else             ! use smallest theta track
c$$$                  criterion = hfpp_track_theta(2,itrack)
c$$$               endif
c$$$               hfpp2_best_reference(itrack) = 0
c$$$            else if(keep(3,itrack)) then ! FPP1 is unambiguously the best reference track for this track
c$$$               if(hselectfpptrackprune.eq.2) then
c$$$                  scloseweight = hfpp_track_chi2(1,hfpp2_best_reference(itrack)) + 
c$$$     $                 minchi2(3)
c$$$                  criterion = hfpp_track_chi2(1,hfpp2_best_reference(itrack)) + 
c$$$     $                 hfpp_track_chi2(2,itrack) + scloseweight * 
c$$$     $                 (hfpp_track_sclose(3,itrack))**2
c$$$               else if(hselectfpptrackprune.eq.3) then ! use smallest chi2 track
c$$$                  criterion = hfpp_track_chi2(2,itrack)
c$$$               else if(hselectfpptrackprune.eq.4) then ! use smallest sclose track
c$$$                  criterion = hfpp_track_sclose(3,itrack)
c$$$               else
c$$$                  criterion = hfpp_track_theta(3,itrack)
c$$$               endif
c$$$            endif
c$$$            if( (keep(2,itrack).or.keep(3,itrack) ).and.
c$$$     $           (firsttry.or.criterion.lt.mincriterion(2)) ) then
c$$$               mincriterion(2) = criterion
c$$$               firsttry = .false.
c$$$               besttrack(2) = itrack
c$$$            endif
c$$$            
c$$$            if(keep(2,itrack).or.keep(3,itrack) ) ngood(2) = ngood(2) + 1
c$$$            
c$$$         enddo
c$$$         
c$$$         hfpp_n_goodtracks(2) = ngood(2)
c$$$         
c$$$      endif

c      do ifpp=1,2
      hfpp_best_track(ifpp) = besttrack(ifpp)
c      enddo
      
      abort = .false.
      err=''
      
      return
      end
