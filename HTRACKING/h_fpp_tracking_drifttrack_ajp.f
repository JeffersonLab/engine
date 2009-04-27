      subroutine h_fpp_tracking_drifttrack_ajp(DCset,SimpleTrack,
     $     HitClusters,OnTrack,track_good,DriftTrack,nhitsrequired,mode,ABORT,err)
*     ALTERNATIVE LEFT-RIGHT DETERMINATION ROUTINE BY AJP
      implicit none
      save
      
      include 'gen_event_info.cmn'
      INCLUDE 'hms_data_structures.cmn'
      INCLUDE 'hms_geometry.cmn'
      include 'gen_detectorids.par'
      include 'gen_decode_common.cmn'
      INCLUDE 'hms_fpp_event.cmn'
      INCLUDE 'hms_fpp_params.cmn'

      character*30 here
      parameter(here='h_fpp_tracking_drifttrack_ajp')

      external jbit             ! cernlib bit routine
      external jibset
      integer*4 jbit
      integer*4 jibset          ! Declare to help f2c

      integer*4 mode            ! indicates whether to fill the final track common block
      integer*4 nhitsrequired   ! indicates whether to allow tracks to go below 6 planes
      integer*4 DCset
      real*4 SimpleTrack(6)
      integer*4 HitClusters(h_fpp_n_dcinset,h_fpp_n_dclayers)
      logical OnTrack(h_fpp_n_dcinset,h_fpp_n_dclayers)
      logical track_good
      real*4 DriftTrack(7)
      real*4 TestTrack(5)
      real*4 BestTrack(5)
      
      logical ABORT
      character*(*) err

      real*4 newTrack(5)
      real*4 HMStrack(4)
      real*4 FPPtrack(4)
      real*4 DCcoords(3), FPcoords(3)
      real*4 theta,phi,sclose,zclose
      real*4 mydriftT,mydriftX,WirePropagation,wirepos,trackpos

      real*4 combochi2(64),combosclose(64),combozclose(64),combotheta(64)
      real*4 combophi(64)
      integer*4 comboconetest(64)

      integer*4 chambers(h_fpp_max_fitpoints),trackchambers(h_fpp_max_fitpoints)
      integer*4 layers(h_fpp_max_fitpoints),tracklayers(h_fpp_max_fitpoints)
      integer*4 wires(h_fpp_max_fitpoints),trackwires(h_fpp_max_fitpoints)

      integer*4 track2chambers(h_fpp_max_fitpoints)
      integer*4 track2layers(h_fpp_max_fitpoints)
      integer*4 track2wires(h_fpp_max_fitpoints)

      real*4 points(h_fpp_max_fitpoints,2),trackpoints(h_fpp_max_fitpoints,2) ! u and z
      real*4 sigma2s(h_fpp_max_fitpoints), tracksigma2s(h_fpp_max_fitpoints)
      real*4 trackcorrsigma2s(h_fpp_max_fitpoints)
      real*4 projects(h_fpp_max_fitpoints,2), trackprojects(h_fpp_max_fitpoints,2)
      real*4 drifts(h_fpp_max_fitpoints), trackdrifts(h_fpp_max_fitpoints)

      real*4 plusminusbest(h_fpp_max_fitpoints)
      real*4 plusminustest(h_fpp_max_fitpoints)
      
      real*4 sclosweight,criterion,theta1,theta2,mintheta

      real*4 zmin,zmax

      real*4 chi2_1,chi2_2,sclos1,sclos2,chi2_H

      real*4 x,y,z,u,v,roughu,roughv,u1,u2,rough_wprime,rough_trkthetaw
      real*4 fine_wprime,fine_trkthetaw

      real*4 hitpos(h_fpp_max_fitpoints,2) ! generalized coordinate u and z with drift

      integer*4 npoints,nclusters,npointsthislayer(h_fpp_n_dcinset,h_fpp_n_dclayers)

      integer*4 ntrackpoints,bestcombo,worstpoint,ntrackplanes,newbestcombo
      integer*4 bestref,jtrack

      logical plusminusfixed(h_fpp_max_fitpoints),notunique

      logical firsttry,anyambig
      
      real test,mintest
      real minchi2,chi2
      real*4 fourbestchi2(4)
      integer fourbestcombos(4)

      real*4 residual,maxresidual,minresidual

      integer ndf,icone

      integer*4 ichamber,ilayer,ihit,hit,wire,sign,bestsign,iplane
      integer*4 icluster,ipoint,icombo,point1,point2,j,k
      integer*4 ncombos
      integer*4 nfree,nfixed,ifree ! number of points w/ fixed and free left-right choice, respectively

      integer itrack,nlayershit,lowpoint,midpoint,highpoint
      integer bestcombo3(2) ! the two points with drift that are closer together in a cluster of three adjacent hits

      integer n3hitclustersontrack ! number of three hit clusters on a track
      integer ntrackstotest ! = 2^n3hitclustersontrack = number of tracks we have to test to find throw out the worst hit from each 3 hit cluster

      integer copypoint
      integer sortedcluster3(3)
      integer pointmiddle(h_fpp_n_dcinset,h_fpp_n_dclayers) ! pointer to the middle hit in a 3-hit cluster

      integer pointleft(h_fpp_n_dcinset,h_fpp_n_dclayers) ! pointer to the hit to the left of middle hit in a 3-hit cluster
      integer pointright(h_fpp_n_dcinset,h_fpp_n_dclayers) ! pointer to the hit to the right of middle hit in a 3-hit cluster

      abort = .false. 
      err = ' '

      track_good = .false.

      npoints = 0
      nclusters = 0

c      write(*,*) 'NEW call to AJP FPP tracking'

      do ichamber=1,h_fpp_n_dcinset
         do ilayer=1,h_fpp_n_dclayers
            icluster = hitclusters(ichamber,ilayer)
c     always initialize to zero
            npointsthislayer(ichamber,ilayer) = 0
            OnTrack(ichamber,ilayer) = .false.

            iplane = 6 * (dcset - 1) 
     $           + 3 * (ichamber - 1)
     $           + ilayer

            if(icluster.gt.0) then
               nclusters = nclusters + 1

               z = hfpp_layerz(dcset,ichamber,ilayer)
               x = simpletrack(1)*z + simpletrack(2)
               y = simpletrack(3)*z + simpletrack(4)

               roughv = y * hfpp_direction(dcset,ichamber,ilayer,1) - 
     $              x * hfpp_direction(dcset,ichamber,ilayer,2)
               roughv = roughv / hfpp_wirespeed

c     record the slope of the simple track along the coordinate measured by this layer
c     for optional correction to drift distance and z of the hit:

               rough_wprime = simpletrack(1) * hfpp_direction(dcset,ichamber,ilayer,1) + 
     $              simpletrack(3) * hfpp_direction(dcset,ichamber,ilayer,2)
               rough_trkthetaw = atan(rough_wprime)

               do ihit=1,hfpp_nhitsincluster(dcset,ichamber,ilayer,icluster)
                  hit = hfpp_clusters(dcset,ichamber,ilayer,icluster,ihit)
                  
                  wire = hfpp_raw_wire(hit)

                  wirepropagation = roughv * float(hfpp_cardpos(dcset,ichamber,ilayer,wire))

                  call h_fpp_drift(hit,simpletrack,wirepropagation,mydriftT,mydriftX,abort,err)
c     if the first hit had a bad drift distance because it was too early, then if the wire has a second hit which gives a "good"
c     drift distance,
c     then let us attempt to use it:
                  if( mydriftX.eq.h_fpp_bad_drift .and. 
     $                 hfpp_hit2idx(iplane,wire) .gt. 0 .and.
     $                 hfpp_use_multihit_wire.ne.0 ) then
c                     write(*,*) 'event=',gen_event_id_number
                     hit = hfpp_hit2idx(iplane,wire)
c                     write(*,*) 'old drift time = ',mydriftT
                     call h_fpp_drift(hit,simpletrack,wirepropagation,mydriftT,mydriftX,abort,err)
c     if the hit now has a "GOOD" drift distance, then we are golden!
c$$$                     if(mydriftX.ne.h_fpp_bad_drift) then
c$$$                   
c$$$c$$$                   write(*,*) 'found good drift distance for 2nd hit'
c$$$c$$$                   write(*,*) 'plane,wire=',iplane,wire
c$$$c$$$                   write(*,*) 'new drift time, dist=',mydriftT,mydriftX
c$$$                   
c$$$                     endif
                  endif

                  hfpp_drift_time(dcset,ichamber,ilayer,wire) = mydriftT
                  hfpp_drift_dist(dcset,ichamber,ilayer,wire) = h_fpp_bad_drift

                  if(mydriftX.ne.h_fpp_bad_drift) then
                     npointsthislayer(ichamber,ilayer) = npointsthislayer(ichamber,ilayer) + 1
c                     ontrack(ichamber,ilayer) = .true.
                     npoints = npoints + 1

                     if(npoints.le.h_fpp_max_fitpoints) then
                        chambers(npoints) = ichamber
                        layers(npoints) = ilayer
                        wires(npoints) = wire
                        points(npoints,1) = hfpp_layeroffset(dcset,ichamber,ilayer)
     $                       + hfpp_spacing(dcset,ichamber,ilayer)*wire
                        points(npoints,2) = hfpp_layerz(dcset,ichamber,ilayer)
                        sigma2s(npoints) = hfpp_resolution(dcset,ichamber,ilayer)
                        projects(npoints,1) = hfpp_direction(dcset,ichamber,ilayer,1)
                        projects(npoints,2) = hfpp_direction(dcset,ichamber,ilayer,2)
                        drifts(npoints) = mydriftX
                     endif
                  endif
               enddo            ! ihit
            endif ! cluster exists in this layer
         enddo ! ilayer
      enddo ! ichamber

* NOW this is where we do things differently from Frank. Having recorded the number of hits per layer,
* we can apply our logic to get the appropriate number of left right combos and fix the left-right
* choice for those hits that are in a cluster of two adjacent hits. For a cluster of three adjacent hits, 
* use ONLY the middle hit and test BOTH left-right combinations:

 101  continue

c      write(*,*) 'AJP tracking with drift, iFPP=',dcset
c      write(*,*) 'npoints on potential track = ',npoints

      nfree = 0
      nfixed = 0

      ntrackpoints = 0
      ntrackplanes = 0

      ipoint = 0

*     How to handle three-hit clusters: Goal is to throw out one of the hits.

      n3hitclustersontrack = 0

      do ichamber=1,h_fpp_n_dcinset
         do ilayer=1,h_fpp_n_dclayers
c            write(*,*) 'number of points chamber ',ichamber,'layer ',ilayer,
c     $           '=',npointsthislayer(ichamber,ilayer)
            if(npointsthislayer(ichamber,ilayer).gt.0) then
               ntrackplanes = ntrackplanes + 1
            endif

            if(npointsthislayer(ichamber,ilayer).eq.1) then 
*     only one hit in this chamber/layer. Don't fix left-right choice 
               nfree = nfree + 1 ! increment number of hits with free left-right
               ntrackpoints = ntrackpoints + 1 ! increment number of points to be used on the track
               ipoint = ipoint + 1 ! increment point array counter
               plusminusfixed(ntrackpoints) = .false. ! set flag for fixed LR to false
*     copy all the properties of this point into the track points array
               trackchambers(ntrackpoints) = ichamber 
               tracklayers(ntrackpoints) = ilayer
               trackwires(ntrackpoints) = wires(ipoint)
               trackpoints(ntrackpoints,1) = points(ipoint,1)
               trackpoints(ntrackpoints,2) = points(ipoint,2)
               tracksigma2s(ntrackpoints) = sigma2s(ipoint)
               trackprojects(ntrackpoints,1) = projects(ipoint,1)
               trackprojects(ntrackpoints,2) = projects(ipoint,2)
               trackdrifts(ntrackpoints) = drifts(ipoint)
            else if(npointsthislayer(ichamber,ilayer).eq.2) then
               nfixed = nfixed + 2

c               point1 = ipoint+1
c               point2 = ipoint+2

*     we wish to fix the left right combination of these hits to force the track to go between the two adjacent wires that 
*     fired:
               if(points(ipoint+1,1).le.points(ipoint+2,1)) then
                  plusminustest(ntrackpoints+1) = 1.0
                  plusminustest(ntrackpoints+2) = -1.0
               else
                  plusminustest(ntrackpoints+1) = -1.0
                  plusminustest(ntrackpoints+2) = 1.0
               endif
               
               plusminusfixed(ntrackpoints+1) = .true.
               plusminusfixed(ntrackpoints+2) = .true.
*     COPY all properties of point1(2) into trackpoints(ntrackpoints+1(2))
               
               do j=1,2
                  trackchambers(ntrackpoints+j) = ichamber
                  tracklayers(ntrackpoints+j) = ilayer
                  trackwires(ntrackpoints+j) = wires(ipoint+j)
                  trackpoints(ntrackpoints+j,1) = points(ipoint+j,1)
                  trackpoints(ntrackpoints+j,2) = points(ipoint+j,2)
                  tracksigma2s(ntrackpoints+j) = sigma2s(ipoint+j)
                  trackprojects(ntrackpoints+j,1) = projects(ipoint+j,1)
                  trackprojects(ntrackpoints+j,2) = projects(ipoint+j,2)
                  trackdrifts(ntrackpoints+j) = drifts(ipoint+j)
               enddo
               ipoint = ipoint + 2
               ntrackpoints = ntrackpoints + 2
            else if(npointsthislayer(ichamber,ilayer).eq.3) then ! find out which hit is the middle hit:
*     first task: find out which hit is the middle hit, which hit is to the right, and which hit is to the left:
               
*     initialize order of hits to 1,2,3:
               do j=1,3
                  sortedcluster3(j) = j
               enddo
*     sort hits in order of increasing wire position:
               do j=1,3
                  do k=j+1,3
                     if(points(ipoint+j,1).gt.points(ipoint+k,1)) then ! pointj > pointk==>swap:
                        copypoint = sortedcluster3(j)
                        sortedcluster3(j) = sortedcluster3(k)
                        sortedcluster3(k) = copypoint
                     endif
                  enddo
               enddo
*     store references to pointmiddle,pointright,and pointleft:
               pointmiddle(ichamber,ilayer) = ipoint+sortedcluster3(2)
               pointleft(ichamber,ilayer) = ipoint+sortedcluster3(1)
               pointright(ichamber,ilayer) = ipoint+sortedcluster3(3)
*     have to be very careful about this. In reality, we want to increase points on the track by just 2, but we need a way 
*     of storing the information of both pointleft and pointright:
               
*     ADD the MIDDLE hit FIRST:
               plusminusfixed(ntrackpoints+1) = .false.
               trackchambers(ntrackpoints+1) = ichamber
               tracklayers(ntrackpoints+1) = ilayer
               trackwires(ntrackpoints+1) = wires(pointmiddle(ichamber,ilayer))
               trackpoints(ntrackpoints+1,1) = points(pointmiddle(ichamber,ilayer),1)
               trackpoints(ntrackpoints+1,2) = points(pointmiddle(ichamber,ilayer),2)
               tracksigma2s(ntrackpoints+1) = sigma2s(pointmiddle(ichamber,ilayer))
               trackprojects(ntrackpoints+1,1) = projects(pointmiddle(ichamber,ilayer),1)
               trackprojects(ntrackpoints+1,2) = projects(pointmiddle(ichamber,ilayer),2)
               trackdrifts(ntrackpoints+1) = drifts(pointmiddle(ichamber,ilayer))

               plusminusfixed(ntrackpoints+2) = .true. ! we will have to defer initialization of the second track point until 
!     we are dealing with an explicit left-right choice of the middle hit in the ncombos loop
               trackchambers(ntrackpoints+2) = ichamber
               tracklayers(ntrackpoints+2) = ilayer
               nfree = nfree + 1
               nfixed = nfixed + 1
               ntrackpoints = ntrackpoints + 2
               ipoint = ipoint + 3

            endif
         enddo
      enddo

c      write(*,*) 'ntrackpoints=',ntrackpoints
c      write(*,*) 'nfree=',nfree
c      write(*,*) 'nfixed=',nfixed
c      write(*,*) 'logical plusminusfixed=',plusminusfixed

      if(nfree + nfixed .ne. ntrackpoints) then
         abort = .true.
         call g_add_path(here,err)
         return
      endif

      if(ntrackpoints.lt.hfpp_minsethits.or.
     $     ntrackplanes.lt.hfpp_minsethits.or.
     $     ntrackplanes.lt.nhitsrequired) then
         track_good = .false.
         return
      endif

*     NUMBER OF POSSIBLE LEFT-RIGHT COMBINATIONS EQUALS 2^NFREEHITS
*     note that it is only possible to have ONE free hit per layer in this routine:

      ncombos = 2**nfree 

**************** HERE IS WHERE THE LEFT-RIGHT DETERMINATION FOR ALL THE FREE HITS OCCURS ********************************            
      firsttry = .true.

c      notunique = .false. variable not needed

      bestcombo = -1

      do icombo=0,ncombos-1
c     set the left right combination on all the "free" hits according to the bit value of the ihit-th bit of icombo:
         ifree = 0

c         write(*,*) 'LR combo ',icombo+1,' of ',ncombos

         combochi2(icombo+1) = 0.0
         comboconetest(icombo+1) = 0
         combosclose(icombo+1) = -9999.
         combozclose(icombo+1) = -9999.
         combotheta(icombo+1) = -9999.
         combophi(icombo+1) = -9999.

         do ipoint=1,ntrackpoints
*     for free hits only, set plusminustest
            if(.not.plusminusfixed(ipoint)) then
               ifree = ifree + 1               
               if(jbit(icombo,ifree).eq.1) then
                  plusminustest(ipoint) = 1.0
*     the first hit in a three-hit cluster is always free: check for three hit clusters here:
                  if(npointsthislayer(trackchambers(ipoint),tracklayers(ipoint))
     $                 .eq.3) then ! initialize the NEXT track point based on pointright
*     the next hit after the first hit in a three hit cluster is always fixed, in this case to the right:
                     copypoint = pointright(trackchambers(ipoint),tracklayers(ipoint))
                     trackwires(ipoint+1) = wires(copypoint)
                     trackpoints(ipoint+1,1) = points(copypoint,1)
                     trackpoints(ipoint+1,2) = points(copypoint,2)
                     tracksigma2s(ipoint+1) = sigma2s(copypoint)
                     trackprojects(ipoint+1,1) = projects(copypoint,1)
                     trackprojects(ipoint+1,2) = projects(copypoint,2)
                     trackdrifts(ipoint+1) = drifts(copypoint)
*     choose drift sign for hit to the right of middle hit so that it points back toward middle hit:
                     plusminustest(ipoint+1) = -1.0
*     we know this has already been set, but let's just set it again to be safe:
                     plusminusfixed(ipoint+1) = .true.
                  endif
               else
                  plusminustest(ipoint) = -1.0
*     the first hit in a three-hit cluster is always free: check for three hit clusters here:
                  if(npointsthislayer(trackchambers(ipoint),tracklayers(ipoint))
     $                 .eq.3) then ! initialise the next track point based on pointleft:
*     the next hit after the first hit in a three hit cluster is always fixed:
                     copypoint = pointleft(trackchambers(ipoint),tracklayers(ipoint))
                     trackwires(ipoint+1) = wires(copypoint)
                     trackpoints(ipoint+1,1) = points(copypoint,1)
                     trackpoints(ipoint+1,2) = points(copypoint,2)
                     tracksigma2s(ipoint+1) = sigma2s(copypoint)
                     trackprojects(ipoint+1,1) = projects(copypoint,1)
                     trackprojects(ipoint+1,2) = projects(copypoint,2)
                     trackdrifts(ipoint+1) = drifts(copypoint)
*     choose drift sign for hit to the left of middle hit so that it points back toward the middle hit:
                     plusminustest(ipoint+1) = 1.0
*     we know it's already been set, but let's set it again to be safe:
                     plusminusfixed(ipoint+1) = .true.
                  endif
               endif
            endif

            rough_wprime = simpletrack(1) * trackprojects(ipoint,1) + 
     $           simpletrack(3) * trackprojects(ipoint,2)
            rough_trkthetaw = atan2(rough_wprime,1.0)

*     initialize hitpos arrays
            if(hfppcorrectdriftforangle.ne.0) then ! assume drift distance represents closest approach distance
c     and correct this point for track angle as measured by the simple track:
               hitpos(ipoint,1) = trackpoints(ipoint,1) + 
     $              plusminustest(ipoint) * trackdrifts(ipoint) / cos(rough_trkthetaw)
               hitpos(ipoint,2) = trackpoints(ipoint,2)

               trackcorrsigma2s(ipoint) = tracksigma2s(ipoint) / ( (cos(rough_trkthetaw))**2 )
            else
               hitpos(ipoint,1) = trackpoints(ipoint,1) + 
     $              plusminustest(ipoint) * trackdrifts(ipoint)
               hitpos(ipoint,2) = trackpoints(ipoint,2)
            endif
         enddo ! end loop over points on the track/plusminustest initialization
*     fit the test track
         if(hfppcorrectdriftforangle.ne.0) then
            call h_fpp_fit3d_ajp(ntrackpoints,hitpos,trackcorrsigma2s,trackprojects,
     $           testtrack)
         else
            call h_fpp_fit3d_ajp(ntrackpoints,hitpos,tracksigma2s,trackprojects,
     $           TestTrack)
         endif
*     get the chi2
         chi2 = TestTrack(5)
*     first check for "ambiguity": is the chi2 of this track exactly 
*     equal to minchi2?
c$$$         if(.not.firsttry.and.chi2.eq.minchi2) then ! AMBIGUITY!!!
c$$$*     choose the better of the two tracks using sclose if we have
c$$$*     an HMS track to use:
c$$$c            write(*,*) 'AMBIGUITY FOUND'
c$$$            if(hsnum_fptrack.gt.0) then
c$$$               hmstrack(1) = hsxp_fp
c$$$               hmstrack(2) = hsx_fp
c$$$               hmstrack(3) = hsyp_fp
c$$$               hmstrack(4) = hsy_fp
c$$$*     translate FPP test track into focal plane coordinates
c$$$               dccoords(1) = testtrack(1)
c$$$               dccoords(2) = testtrack(3)
c$$$               dccoords(3) = 1.0
c$$$*     first slopes:
c$$$               call h_fpp_dc2fp(dcset,.true.,dccoords,fpcoords)
c$$$               fpptrack(1) = fpcoords(1)
c$$$               fpptrack(3) = fpcoords(2)
c$$$*     then position offsets:
c$$$               dccoords(1) = testtrack(2)
c$$$               dccoords(2) = testtrack(4)
c$$$               dccoords(3) = 0.0
c$$$               call h_fpp_dc2fp(dcset,.false.,dccoords,fpcoords)
c$$$               
c$$$               fpptrack(2) = fpcoords(1) 
c$$$     $              - fpcoords(3) * fpptrack(1)
c$$$               fpptrack(4) = fpcoords(2)
c$$$     $              - fpcoords(3) * fpptrack(3)
c$$$
c$$$               call h_fpp_closest(hmstrack,fpptrack,sclos1,zclose)
c$$$*     Now do the same for the (currently) best track:
c$$$               dccoords(1) = besttrack(1)
c$$$               dccoords(2) = besttrack(3)
c$$$               dccoords(3) = 1.0
c$$$               call h_fpp_dc2fp(dcset,.true.,dccoords,fpcoords)
c$$$               fpptrack(1) = fpcoords(1)
c$$$               fpptrack(3) = fpcoords(2)
c$$$               
c$$$               dccoords(1) = besttrack(2)
c$$$               dccoords(2) = besttrack(4)
c$$$               dccoords(3) = 0.0
c$$$               call h_fpp_dc2fp(dcset,.false.,dccoords,fpcoords)
c$$$
c$$$               fpptrack(2) = fpcoords(1) - fpcoords(3) * fpptrack(1)
c$$$               fpptrack(4) = fpcoords(2) - fpcoords(3) * fpptrack(3)
c$$$
c$$$               call h_fpp_closest(hmstrack,fpptrack,sclos2,zclose)
c$$$*     only if sclos1 is smaller than sclos2, replace 
c$$$*     the best track and the best left-right combo with the current
c$$$*     one
c$$$               if(sclos1.lt.sclos2) then
c$$$                  do j=1,ntrackpoints
c$$$                     plusminusbest(j) = plusminustest(j)
c$$$                  enddo
c$$$                  do j=1,5
c$$$                     besttrack(j) = testtrack(j)
c$$$                  enddo
c$$$               endif
c$$$*     otherwise, choose the track with the smallest 
c$$$*     theta relative to the z axis, i.e., closest to normal
c$$$*     incidence: this is arbitrary, but if no HMS track, we don't care anyway
c$$$            else
c$$$               theta1 = acos(1. / sqrt(1. + (testtrack(1) )**2 
c$$$     $              + (testtrack(3) )**2))
c$$$               theta2 = acos(1. / sqrt(1. + (besttrack(1) )**2 
c$$$     $              + (besttrack(3) )**2))
c$$$               if(theta1.lt.theta2) then
c$$$                  do j=1,ntrackpoints
c$$$                     plusminusbest(j) = plusminustest(j)
c$$$                  enddo
c$$$                  do j=1,5
c$$$                     besttrack(j) = testtrack(j)
c$$$                  enddo
c$$$               endif
c$$$            endif ! end if on existence of HMS track 
c$$$         endif ! end test on ambiguity

c$$$c     calculate track quantities relative to reference track for each LR combo:
c$$$         combochi2(icombo+1) = chi2
c$$$c     first, transform to focal plane coordinate system, slopes first:
c$$$         dccoords(1) = testtrack(1) ! dx/dz
c$$$         dccoords(2) = testtrack(3) ! dy/dz
c$$$         dccoords(3) = 1.0          ! dz/dz
c$$$
c$$$         call h_fpp_dc2fp(dcset,.true.,dccoords,fpcoords)
c$$$
c$$$         fpptrack(1) = fpcoords(1) ! dx/dz
c$$$         fpptrack(3) = fpcoords(2) ! dy/dz
c$$$
c$$$         dccoords(1) = testtrack(2) ! x 
c$$$         dccoords(2) = testtrack(4) ! y
c$$$         dccoords(3) = 0.0          ! z
c$$$         
c$$$         call h_fpp_dc2fp(dcset,.false.,dccoords,fpcoords)
c$$$
c$$$c     project back to z=0:
c$$$
c$$$         fpptrack(2) = fpcoords(1) - fpcoords(3)*fpptrack(1)
c$$$         fpptrack(4) = fpcoords(2) - fpcoords(3)*fpptrack(3)
c$$$
c$$$*     now we need to define reference track: if dcset==1, reference track = HMS track. Otherwise, reference track = 
c$$$*     "BEST" FPP1 track
c$$$
c$$$         hmstrack(1) = hsxp_fp
c$$$         hmstrack(2) = hsx_fp
c$$$         hmstrack(3) = hsyp_fp
c$$$         hmstrack(4) = hsy_fp
c$$$         
c$$$         if(dcset.eq.2.and.hfpp_best_track(1).gt.0) then
c$$$            jtrack = hfpp_best_track(1)
c$$$            hmstrack(1) = hfpp_track_dx(1,jtrack)
c$$$            hmstrack(2) = hfpp_track_x(1,jtrack)
c$$$            hmstrack(3) = hfpp_track_dy(1,jtrack)
c$$$            hmstrack(4) = hfpp_track_y(1,jtrack)
c$$$         endif
c$$$
c$$$         call h_fpp_relative_angles(hmstrack(1),hmstrack(3),fpptrack(1),fpptrack(3),theta,phi)
c$$$
c$$$         combotheta(icombo+1) = theta
c$$$         combophi(icombo+1) = phi
c$$$         
c$$$         call h_fpp_closest(hmstrack,fpptrack,sclose,zclose)
c$$$
c$$$         combosclose(icombo+1) = sclose
c$$$         combozclose(icombo+1) = zclose
c$$$
c$$$         icone = 1
c$$$
c$$$         call h_fpp_conetest(hmstrack,dcset,zclose,theta,icone)
c$$$
c$$$         comboconetest(icombo+1) = icone

c$$$         if(ntrackplanes.eq.6.and.ncombos.ge.4.and.ntrackpoints.eq.6) then
c$$$            write(*,*) 'icombo, chi2=',icombo,chi2
c$$$         endif
*     if this is the first combination tried or the chi2 is better than the best chi2,
         if(firsttry.or.chi2.lt.minchi2) then
            bestcombo = icombo
*     set best chi2 to the chi2 of this track
            minchi2 = chi2
*     no longer first try
            firsttry = .false.
*     set plusminusbest for all the points on the track to the current left-right combo
            do j=1,ntrackpoints
               plusminusbest(j) = plusminustest(j)
            enddo
c            write(*,*) 'found combo w/ better chi2 plusminus=',plusminusbest
*     set track parameters of besttrack to the fit results stored in testtrack
            do j=1,5
               besttrack(j) = testtrack(j)
            enddo
c            write(*,*) 'current best track=',besttrack
         endif
      enddo                     ! end loop over possible left-right combos
      
c$$$      if(ncombos.ge.4.and.ntrackplanes.ge.6.and.ntrackpoints.eq.6) then 
c$$$         write(*,*) 'chi2 of four best left-right combos=',fourbestchi2
c$$$         write(*,*) 'four best left-right combos = ',fourbestcombos
c$$$      endif
c     now the question is whether to choose a left-right combo with worse chi2 based on sclose and so on:
c     DO NOT attempt this if the track has ANY multi-hit clusters!!!!!!
c$$$      if(hfppfixleftright.gt.0.and.ntrackpoints.eq.ntrackplanes.and.
c$$$     $     nfree.eq.ntrackpoints) then
c$$$
c$$$         firsttry = .true.
c$$$
c$$$         newbestcombo = -1
c$$$
c$$$         do icombo=0,ncombos-1
c$$$            zmin = hfpp_prune_zclose(2*(dcset-1)+1) - 
c$$$     $           hfpp_prune_zslop(dcset) / tan(combotheta(icombo+1) )
c$$$            zmax = hfpp_prune_zclose(2*(dcset-1)+2) + 
c$$$     $           hfpp_prune_zslop(dcset) / tan(combotheta(icombo+1) )
c$$$
c$$$            if(combochi2(icombo+1)-minchi2.le.hfpp_leftright_chi2cut
c$$$     $           .and.icombo.ne.bestcombo) then
c$$$
c$$$               anyambig = .false.
c$$$
c$$$               do ipoint=1,ntrackpoints
c$$$                  sign = jbit(icombo,ipoint)
c$$$                  bestsign = jbit(bestcombo,ipoint)
c$$$                  if(sign.ne.bestsign.and.trackdrifts(ipoint).ge.
c$$$     $                 5.*sqrt(tracksigma2s(ipoint)) ) then
c$$$                     anyambig = .true.
c$$$                  endif
c$$$               enddo
c$$$
c$$$               if(anyambig) then
c$$$                  if(combosclose(icombo+1).lt.combosclose(bestcombo+1)
c$$$     $                 .and.combozclose(icombo+1).ge.zmin
c$$$     $                 .and.combozclose(icombo+1).le.zmax) then
c$$$                     test = (combosclose(icombo+1)/combosclose(bestcombo+1) )**2
c$$$                     if(test.le.hfpp_leftright_stestmax.and.
c$$$     $                    (firsttry.or.test.lt.mintest) ) then
c$$$                        firsttry = .false.
c$$$                        mintest = test
c$$$                        newbestcombo = icombo
c$$$                     endif
c$$$                  endif
c$$$               endif
c$$$
c$$$            endif
c$$$         enddo
c$$$
c$$$         if(newbestcombo.ge.0) then
c$$$            write(*,*) 'FPP=',dcset
c$$$            write(*,*) 'old best combo chi2 = ',combochi2(bestcombo+1)
c$$$            write(*,*) '(sclose,zclose,theta,phi)=',combosclose(bestcombo+1),
c$$$     $        combozclose(bestcombo+1),combotheta(bestcombo+1),combophi(bestcombo+1)
c$$$
c$$$            write(*,*) 'new best combo chi2 = ',combochi2(newbestcombo+1)
c$$$            write(*,*) '(sclose,zclose,theta,phi)=',combosclose(newbestcombo+1),
c$$$     $        combozclose(newbestcombo+1),combotheta(newbestcombo+1),combophi(newbestcombo+1)
c$$$
c$$$            do ipoint=1,ntrackpoints
c$$$               if(jbit(newbestcombo,ipoint).eq.1) then
c$$$                  plusminusbest(ipoint) = 1.0
c$$$               else 
c$$$                  plusminusbest(ipoint) = -1.0
c$$$               endif
c$$$            enddo
c$$$         endif
c$$$      endif
*     set "final" coordinate based on best left right combination:
*     we still have to re-"initialize" all the "second" hits within three-hit clusters based on 
*     whatever the best left-right combo is:
      do ipoint=1,ntrackpoints
         if(npointsthislayer(trackchambers(ipoint),tracklayers(ipoint))
     $        .eq.3.and..not.plusminusfixed(ipoint)) then ! Do "final" initialization of the next point, which is fixed:
*     reduce number of points in this layer to two (we never actually use 3 hits in the same layer on a track:
            npointsthislayer(trackchambers(ipoint),tracklayers(ipoint))
     $           = 2
            
            if(plusminusbest(ipoint).eq.1.0) then
               copypoint = pointright(trackchambers(ipoint),tracklayers(ipoint))
            else
               copypoint = pointleft(trackchambers(ipoint),tracklayers(ipoint))
            endif

            trackwires(ipoint+1) = wires(copypoint)
            trackpoints(ipoint+1,1) = points(copypoint,1)
            trackpoints(ipoint+1,2) = points(copypoint,2)
            tracksigma2s(ipoint+1) = sigma2s(copypoint)
            trackprojects(ipoint+1,1) = projects(copypoint,1)
            trackprojects(ipoint+1,2) = projects(copypoint,2)
            trackdrifts(ipoint+1) = drifts(copypoint)

         endif

c     do final initialization of hitpos arrays using fitted track angles:
         
         fine_wprime = besttrack(1) * trackprojects(ipoint,1) + 
     $        besttrack(3) * trackprojects(ipoint,2)
         fine_trkthetaw = atan2(fine_wprime,1.0)

         if(hfppcorrectdriftforangle.ne.0) then
            hitpos(ipoint,1) = trackpoints(ipoint,1) + 
     $           plusminusbest(ipoint) * trackdrifts(ipoint) / cos(fine_trkthetaw)
            hitpos(ipoint,2) = trackpoints(ipoint,2)

            trackcorrsigma2s(ipoint) = tracksigma2s(ipoint) / ((cos(fine_trkthetaw))**2)
         else
            hitpos(ipoint,1) = trackpoints(ipoint,1) + 
     $           plusminusbest(ipoint) * trackdrifts(ipoint)
            hitpos(ipoint,2) = trackpoints(ipoint,2)
         endif
      enddo
*     fit the set of points using the BEST left-right combo to the "BEST" track
*     hit position within the cell is now corrected for the FITTED track angles rather than the 
*     rough track angles. In principle we could iterate this procedure to arbitrary precision
*     limited only by the drift distance resolution and the extent to which the approximation that drift distance 
*     equals closest approach distance is valid, but one iteration should be quite sufficient.

      if(hfppcorrectdriftforangle.ne.0) then
         call h_fpp_fit3d_ajp(ntrackpoints,hitpos,trackcorrsigma2s,trackprojects,
     $        besttrack)
      else
         call h_fpp_fit3d_ajp(ntrackpoints,hitpos,tracksigma2s,trackprojects,
     $        BestTrack)
      endif
         
c      write(*,*) 'best track found=',besttrack
c      write(*,*) 'best LR combo=',plusminusbest

*     now we get into the chi2 test and throwing out hits one by one to see if we can get a 
*     better chi2 part of the code:
*     if we deem the chi2 acceptable, then we are done:
      if( besttrack(5).le.hfpp_min_chi2 .and.
     $     besttrack(5).ge.0.0          .and.
     $     besttrack(5).ne.h_fpp_bad_chi2 ) then
*     if we have "more hits than we need" (all planes hit, at least one plane with >1 hit) AND
*     the chi2 of those hits is worse than the "superclean" chi2 test, 
*     strike out the worst of the hits from a plane with two adjacent hits:
         if(ntrackplanes.eq.h_fpp_n_dcinset*h_fpp_n_dclayers.and.
     $        ntrackpoints.gt.ntrackplanes.and.
     $        besttrack(5).gt.hfpp_superclean_chi2) then
            firsttry = .true.

            do ihit=1,ntrackpoints
               ichamber = trackchambers(ihit)
               ilayer = tracklayers(ihit)
               if(npointsthislayer(ichamber,ilayer).gt.1) then
                  
                  x = besttrack(1) * trackpoints(ihit,2) + besttrack(2)
                  y = besttrack(3) * trackpoints(ihit,2) + besttrack(4)
                  u = x * trackprojects(ihit,1) + y * trackprojects(ihit,2)
                  
                  if(hfppcorrectdriftforangle.ne.0) then
                     residual = (u - hitpos(ihit,1))**2/trackcorrsigma2s(ihit)
                  else
                     residual = (u - hitpos(ihit,1))**2/tracksigma2s(ihit)
                  endif

                  if(firsttry.or.residual.gt.maxresidual) then
                     firsttry = .false.
                     maxresidual = residual
                     worstpoint = ihit
                  endif
               endif
            enddo

            npoints = 0

            do ichamber=1,h_fpp_n_dcinset
               do ilayer=1,h_fpp_n_dclayers
                  npointsthislayer(ichamber,ilayer) = 0
               enddo
            enddo
*     now initialize the "points" array from "trackpoints", in order to prepare for re-fitting:
            do ipoint=1,ntrackpoints
               if(ipoint.ne.worstpoint) then
                  npoints = npoints + 1
                  ichamber = trackchambers(ipoint)
                  ilayer = tracklayers(ipoint)
                  npointsthislayer(ichamber,ilayer) = npointsthislayer(ichamber,ilayer) + 1
                  chambers(npoints) = ichamber
                  layers(npoints) = ilayer
                  wires(npoints) = trackwires(ipoint)
                  points(npoints,1) = trackpoints(ipoint,1)
                  points(npoints,2) = trackpoints(ipoint,2)
                  sigma2s(npoints) = tracksigma2s(ipoint)
                  projects(npoints,1) = trackprojects(ipoint,1)
                  projects(npoints,2) = trackprojects(ipoint,2)
                  drifts(npoints) = trackdrifts(ipoint)
               endif
            enddo
            
            if(npoints.ge.hfpp_minsethits) goto 101 ! repeat track fitting with the worst hit thrown out

         else 
            track_good = .true.

            do ihit=1,ntrackpoints
               ichamber = trackchambers(ihit)
               ilayer = tracklayers(ihit)

               ontrack(ichamber,ilayer) = .true.

               wire = trackwires(ihit)
               hfpp_drift_dist(dcset,ichamber,ilayer,wire) = 
     $              trackdrifts(ihit) * plusminusbest(ihit)

               if(hfppcorrectdriftforangle.ne.0) then
                  fine_wprime = trackprojects(ihit,1)*besttrack(1) + 
     $                 trackprojects(ihit,2)*besttrack(3)

                  fine_trkthetaw = atan2(fine_wprime,1.0)
                  
                  hfpp_drift_dist(dcset,ichamber,ilayer,wire) = 
     $                 trackdrifts(ihit) * plusminusbest(ihit) / cos(fine_trkthetaw)
               endif
            enddo
*     Fill output track variables: 
            do j=1,5
               drifttrack(j) = besttrack(j)
            enddo
            drifttrack(6) = float(ntrackpoints)
            drifttrack(7) = float(ntrackplanes)
         endif
*     Otherwise, go back and drop the hit with the worst contribution to the chi2:
      else if(ntrackpoints.gt.hfpp_minsethits.and.ntrackplanes.ge.
     $        nhitsrequired) then
*     here we will drop the hit with the worst contribution to the 
*     chi2, re-determine the number of free vs fixed left-right hits
*     and re-determine the best left-right combo, and rinse, repeat 
*     until we either arrive at a track with an acceptable chi2 or 
*     the number of hits on the track falls below the minimum:
*     on a second pass, it should be impossible to end up with any three-hit clusters:
*     also note that only if we have made it to the point where 5 plane tracks are allowed do we consider this option
*     start with multiple-hit clusters:
         firsttry = .true.
 
         do ipoint=1,ntrackpoints
            x = besttrack(1) * trackpoints(ipoint,2) + besttrack(2)
            y = besttrack(3) * trackpoints(ipoint,2) + besttrack(4)
*     convert to generalized coordinate:
            u = x * trackprojects(ipoint,1) + y * trackprojects(ipoint,2)

            if(hfppcorrectdriftforangle.ne.0) then
               residual = (u - hitpos(ipoint,1))**2/trackcorrsigma2s(ipoint)
            else
               residual = (u - hitpos(ipoint,1))**2/tracksigma2s(ipoint)
            endif

            if(firsttry.or.residual.gt.maxresidual) then
c     first, ask whether this track has ANY multi-hit clusters:
               if(ntrackpoints.gt.ntrackplanes) then ! if so, restrict to multi-hit clusters on this round:
                  ichamber = trackchambers(ipoint)
                  ilayer = tracklayers(ipoint)
                  if(npointsthislayer(ichamber,ilayer).gt.1) then
                     firsttry = .false.
                     maxresidual = residual
                     worstpoint = ipoint
                  endif
               else ! if the number of hits on the track equals the number of planes, throw out the worst hit
c     from any plane
                  firsttry = .false.
                  maxresidual = residual
                  worstpoint = ipoint
               endif
            endif
         enddo
         
c         write(*,*) 'worst point=',worstpoint
c         write(*,*) 'residual^2/sigma^2=',maxresidual

*     reset number of candidate points on the track:
         npoints = 0
        
*     reset number of points in each layer:
         do ichamber=1,h_fpp_n_dcinset
            do ilayer=1,h_fpp_n_dclayers
               npointsthislayer(ichamber,ilayer) = 0
               ontrack(ichamber,ilayer) = .false.
            enddo
         enddo
*     fill the "points" array from the "trackpoints" array to get ready for 
*     attempted fitting:
         do ipoint=1,ntrackpoints
            if(ipoint.ne.worstpoint) then
               npoints = npoints + 1
               ichamber = trackchambers(ipoint)
               ilayer = tracklayers(ipoint)
               npointsthislayer(ichamber,ilayer) = npointsthislayer(ichamber,ilayer) + 1
c               ontrack(ichamber,ilayer) = .true.
               chambers(npoints) = ichamber
               layers(npoints) = ilayer
               wires(npoints) = trackwires(ipoint)
               points(npoints,1) = trackpoints(ipoint,1)
               points(npoints,2) = trackpoints(ipoint,2)
               sigma2s(npoints) = tracksigma2s(ipoint)
               projects(npoints,1) = trackprojects(ipoint,1)
               projects(npoints,2) = trackprojects(ipoint,2)
               drifts(npoints) = trackdrifts(ipoint)
            endif
         enddo

         ntrackpoints = npoints
         ntrackplanes = 0
         do ichamber=1,h_fpp_n_dcinset
            do ilayer=1,h_fpp_n_dclayers
               if(npointsthislayer(ichamber,ilayer).gt.0) then
                  ntrackplanes = ntrackplanes + 1
               endif
            enddo
         enddo

         if(ntrackpoints.ge.hfpp_minsethits.and.
     $        ntrackplanes.ge.hfpp_minsethits.and.
     $        ntrackplanes.ge.nhitsrequired) goto 101 ! repeat track fitting/LR determination
         
         track_good = .false.
      else
         track_good = .false.
      endif

      if(track_good) then
         if(mode.eq.0) then     ! store global tracking results in common blocks
            itrack = hfpp_n_tracks(dcset) + 1
            
            if(itrack.le.h_fpp_max_tracks) then
               hfpp_n_tracks(dcset) = itrack
               
               nlayershit = 0
               
               do ichamber=1,h_fpp_n_dcinset
                  do ilayer=1,h_fpp_n_dclayers
                     if(npointsthislayer(ichamber,ilayer).gt.0) then
                        nlayershit = nlayershit + 1
                        icluster = hitclusters(ichamber,ilayer)
                        
                        if(icluster.gt.0) then
                           hfpp_clusterintrack(dcset,ichamber,ilayer,icluster) = itrack
                        endif
                     endif
                  enddo
               enddo
               
               hfpp_track_nlayers(dcset,itrack) = nlayershit
               
               do j=1,4
                  hfpp_track_fine(dcset,itrack,j) = drifttrack(j)
               enddo
               hfpp_track_chi2(dcset,itrack) = drifttrack(5)
               hfpp_track_nhits(dcset,itrack) = int(drifttrack(6))
               do j=1,6
                  hfpp_track_rough(dcset,itrack,j) = simpletrack(j)
               enddo
               hfpp_track_uniq(dcset,itrack) = .true.
               
               DCcoords(1) = drifttrack(1)
               DCcoords(2) = drifttrack(3)
               DCcoords(3) = 1.0
               
               call h_fpp_dc2fp(dcset,.true.,dccoords,fpcoords)
               hfpp_track_dx(dcset,itrack) = fpcoords(1)
               hfpp_track_dy(dcset,itrack) = fpcoords(2)
               
               dccoords(1) = drifttrack(2)
               dccoords(2) = drifttrack(4)
               dccoords(3) = 0.0
               call h_fpp_dc2fp(dcset,.false.,dccoords,fpcoords)
               
               hfpp_track_x(dcset,itrack) = fpcoords(1)
     $              - fpcoords(3) * hfpp_track_dx(dcset,itrack)
               hfpp_track_y(dcset,itrack) = fpcoords(2)
     $              - fpcoords(3) * hfpp_track_dy(dcset,itrack)
               
*     calculate relative angle between incident and scattered track in the FP:
               call h_fpp_relative_angles(hsxp_fp,hsyp_fp,hfpp_track_dx(dcset,itrack),
     $              hfpp_track_dy(dcset,itrack),theta,phi)
               if(hsnum_fptrack.gt.0) then
                  hfpp_track_theta(dcset,itrack) = theta
                  hfpp_track_phi(dcset,itrack) = phi
               else             ! take theta and phi relative to normal incidence if there is no HMS track
                  hfpp_track_theta(dcset,itrack) = acos(1.0 / 
     $                 sqrt(1.0 + (hfpp_track_dx(dcset,itrack) )**2 + 
     $                 (hfpp_track_dy(dcset,itrack) )**2) )
                  hfpp_track_phi(dcset,itrack) = atan2(hfpp_track_dy(dcset,itrack),
     $                 hfpp_track_dx(dcset,itrack) )
               endif
            
               if(hsnum_fptrack.gt.0) then
                  hmstrack(1) = hsxp_fp
                  hmstrack(2) = hsx_fp
                  hmstrack(3) = hsyp_fp
                  hmstrack(4) = hsy_fp
               else
                  hmstrack(1) = 0.0
                  hmstrack(2) = 0.0
                  hmstrack(3) = 0.0
                  hmstrack(4) = 0.0
               endif
               fpptrack(1) = hfpp_track_dx(dcset,itrack)
               fpptrack(2) = hfpp_track_x(dcset,itrack)
               fpptrack(3) = hfpp_track_dy(dcset,itrack)
               fpptrack(4) = hfpp_track_y(dcset,itrack)

               call h_fpp_closest(hmstrack,fpptrack,sclose,zclose)
            
               hfpp_track_sclose(dcset,itrack) = sclose
               hfpp_track_zclose(dcset,itrack) = zclose
               
               icone = 1

               call h_fpp_conetest(hmstrack,dcset,zclose,theta,icone)

               hfpp_track_conetest(dcset,itrack) = icone
 
               if(dcset.eq.2) then ! figure out theta,phi,sclose,zclose of fpp2 track relative to fpp1 track. 
c     if multiple FPP1 tracks, store relative angles and closest approach to the track in FPP1 for which theta of 
c     the track in FPP2 is minimimum
                  firsttry=.true.
                  bestref = 0
               
                  do jtrack=1,hfpp_n_tracks(1)
                     call h_fpp_relative_angles(hfpp_track_dx(1,jtrack),
     $                    hfpp_track_dy(1,jtrack),hfpp_track_dx(dcset,itrack),
     $                    hfpp_track_dy(dcset,itrack),theta,phi)
                     if(firsttry.or.theta.lt.mintheta) then
                        firsttry=.false.
                        mintheta = theta
                        bestref = jtrack
                     endif
                  enddo
                  
                  hfpp2_best_reference(itrack) = bestref
c     calculate values to store in common block:
                  if(bestref.gt.0) then
                     call h_fpp_relative_angles(hfpp_track_dx(1,bestref),
     $                    hfpp_track_dy(1,bestref),
     $                    hfpp_track_dx(dcset,itrack),
     $                    hfpp_track_dy(dcset,itrack),theta,phi)
                     hfpp_track_theta(dcset+1,itrack) = theta
                     hfpp_track_phi(dcset+1,itrack) = phi
c     calculate closest approach variables relative to "best" reference track:
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
                     
                     icone = 1
                     
                     call h_fpp_conetest(hmstrack,dcset,zclose,theta,icone)
                  
                     hfpp_track_conetest(dcset+1,itrack) = icone
                  endif
               endif   
            endif
c$$$         else                   ! don't fill track common blocks, just mark the hits as used. 
c$$$c     we will fill the common blocks in a different routine:
c$$$c            nlayershit = 0
c$$$            
c$$$            do ichamber=1,h_fpp_n_dcinset
c$$$               do ilayer=1,h_fpp_n_dclayers
c$$$c     only mark the cluster as used if at least one hit from the cluster made it on to the 
c$$$c     good track!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c$$$                  if(ontrack(ichamber,ilayer)) then
c$$$c                     nlayershit = nlayershit + 1
c$$$                     icluster = hitclusters(ichamber,ilayer)
c$$$                     if(icluster.gt.0) then
c$$$                        hfpp_clusterintrack(dcset,ichamber,ilayer,icluster) = 1
c$$$                     endif
c$$$                  endif
c$$$               enddo
c$$$            enddo
         endif
      endif

      return
      end
