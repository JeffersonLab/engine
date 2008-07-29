      subroutine h_redo_track_left_right(track,abort,err)
      
      implicit none
      save

      integer*4 track
      
      logical abort
      character*(*) err

      character*24 here
      parameter(here='h_redo_track_left_right')

      include 'hms_data_structures.cmn'
      include 'hms_geometry.cmn'
      include 'hms_tracking.cmn'

      integer xhits(4),yhits(4),uhits(2),vhits(2)

      integer nxcombos,nycombos,bestcombox,bestcomboy
      integer nxplanes,nyplanes,nuplanes,nvplanes

      real plusminustest(4)
      real plusminusx(4),plusminusy(4),plusminusu(2),plusminusv(2)
      real plusminusbestx(4)
      real plusminusbesty(4)

      real originalchi2

      integer ntest,nfree,i,j

      integer hit,ihit,jhit,icombo
      integer ierr

      integer plane,wire
      real ddist

      real uhit,utrack,ures(2)
      real vhit,vtrack,vres(2)
      real hitpos,trackpos,residual

      real track_coord(hmax_num_dc_planes)

      real chi2,minchi2

      real*8 TT(hnum_fpray_param)
      real*8 AA(hnum_fpray_param,hnum_fpray_param)
      real*8 dray(hnum_fpray_param)

      logical firsttry
      
      external jbit             ! cernlib bit routine
      external jibset
      integer*4 jbit
      integer*4 jibset          ! Declare to help f2c

*     array to remap hplane_coeff to param number
      integer*4 remap(hnum_fpray_param)
      data remap/5,6,3,4/

      abort = .false.
      err = ' '

      if(track.gt.hntracks_fp) return

c     FIRST STEP: SORT ALL THE HITS ON THE TRACK BY PLANE

      nxplanes = 0
      nyplanes = 0
      nuplanes = 0
      nvplanes = 0

      do ihit=2,hntrack_hits(track,1)+1
         hit = hntrack_hits(track,ihit)
c     "hit" is array index in the decoded dc array
         plane = hdc_plane_num(hit)

c     if we have an xplane
         if(plane.eq.1.or.plane.eq.6.or.plane.eq.7.or.plane.eq.12) then
c     increment number of x planes
            nxplanes = nxplanes + 1
c     store hit number (not order in the track array, order in the DC array)
            xhits(nxplanes) = hit
c     store drift distance sign
            plusminusx(nxplanes) = htrack_leftright(track,ihit-1)
         endif

         if(plane.eq.2.or.plane.eq.5.or.plane.eq.8.or.plane.eq.11) then
            nyplanes = nyplanes + 1
            yhits(nyplanes) = hit
            plusminusy(nyplanes) = htrack_leftright(track,ihit-1)
         endif
            
         if(plane.eq.3.or.plane.eq.10) then
            nuplanes = nuplanes + 1
            uhits(nuplanes) = hit
         endif
         
         if(plane.eq.4.or.plane.eq.11) then
            nvplanes = nvplanes + 1
            vhits(nvplanes) = hit
         endif
      enddo
      
c     up to this point, track arrays haven't been modified in any way, so breaking out of the routine 
c     if we have less than three x planes or less than three y planes doesn't hurt us.

      if(nxplanes.ge.3.and.nyplanes.ge.3) then ! we have enough x and y planes to do the left-right independently for both
c     count number of left-right combinations of x and y
         nxcombos = 2**nxplanes
         nycombos = 2**nyplanes
c     count number of planes for the "test" track-->uses only x and y planes to find the best left-right
         ntest = nxplanes + nyplanes
c     get degrees of freedom
         nfree = ntest - hnum_fpray_param
         
c     start with left right combination of the y planes, it is more likely to be wrong:
         
         firsttry = .true.
         
c     initialize "best" combos to -1. This way we only keep the new results if at least one successful track
         bestcombox = -1
         bestcomboy = -1

         originalchi2 = hchi2_fp(track) / float(hnfree_fp(track))

c     loop over all left-right combinations of the y hits
         do icombo=0,nycombos-1
c     use bit value of icombo to set left/right for y planes
            do ihit=1,nyplanes
               if(jbit(icombo,ihit).eq.1) then
                  plusminustest(ihit) = 1.
               else
                  plusminustest(ihit) = -1.
               endif
c     get the array index of the hit:
               hit = yhits(ihit)
c     set wire_coord of the hit:
               hdc_wire_coord(hit) = hdc_wire_center(hit) + 
     $              plusminustest(ihit) * hdc_drift_dis(hit)
            enddo
c     set left/right for x planes to value from h_left_right.f:
            do ihit=1,nxplanes
c     get array index of the hit
               hit = xhits(ihit)
c     set wire coord of the hit:
               hdc_wire_coord(hit) = hdc_wire_center(hit) + 
     $              plusminusx(ihit) * hdc_drift_dis(hit)
            enddo
            
c     now accumulate system of equations for track fitting:
            
            do i=1,hnum_fpray_param
               TT(i) = 0.
               do ihit=1,nyplanes
                  hit = yhits(ihit)
                  plane = hdc_plane_num(hit)
                  TT(i) = TT(i) + hdc_wire_coord(hit)*hplane_coeff(remap(i),plane) / 
     $                 ( (hdc_sigma(plane) )**2 )
               enddo
               
               do ihit=1,nxplanes
                  hit = xhits(ihit)
                  plane = hdc_plane_num(hit)
                  TT(i) = TT(i) + hdc_wire_coord(hit)*hplane_coeff(remap(i),plane) / 
     $                 ( (hdc_sigma(plane) )**2 )
               enddo
            enddo
            
            do i=1,hnum_fpray_param
               do j=1,hnum_fpray_param
                  AA(i,j) = 0.
                  if(j.lt.i) then
                     AA(i,j) = AA(j,i)
                  else
                     do ihit=1,nyplanes
                        hit = yhits(ihit)
                        plane = hdc_plane_num(hit)
                        AA(i,j) = AA(i,j) + hplane_coeff(remap(i),plane) *
     $                       hplane_coeff(remap(j),plane) / 
     $                       ( (hdc_sigma(plane) )**2 )
                     enddo

                     do ihit=1,nxplanes
                        hit = xhits(ihit)
                        plane = hdc_plane_num(hit)
                        AA(i,j) = AA(i,j) + hplane_coeff(remap(i),plane) * 
     $                       hplane_coeff(remap(j),plane) / 
     $                       ( (hdc_sigma(plane) )**2 )
                     enddo
                  endif
               enddo
            enddo

c     get the solution:
            
            call solve_four_by_four(TT,AA,dray,ierr)
            
            if(ierr.ne.0) then  ! problem with fitting:
               dray(1)=10000.
               dray(2)=10000.
               dray(3)=2.
               dray(4)=2.
            else                ! calculate chi2 of the test track:
               chi2 = 0.
c     calculate track position at each plane:
               do plane=1,hdc_num_planes
                  track_coord(plane) = hplane_coeff(remap(1),plane)*dray(1)
     $                 + hplane_coeff(remap(2),plane)*dray(2)
     $                 + hplane_coeff(remap(3),plane)*dray(3)
     $                 + hplane_coeff(remap(4),plane)*dray(4)
               enddo
c     get the residual of each hit on the test track:
               do ihit=1,nyplanes
                  hit = yhits(ihit)
                  plane = hdc_plane_num(hit)
c     this is wire_coord based on the CURRENT left-right combination
                  hitpos = hdc_wire_coord(hit)
c     this was just calculated above
                  trackpos = track_coord(plane)
                  residual = hitpos - trackpos
                  chi2 = chi2 + (residual / hdc_sigma(plane) )**2
               enddo
               
               do ihit=1,nxplanes
                  hit = xhits(ihit)
                  plane = hdc_plane_num(hit)
c     this is wire coord based on the CURRENT left-right combination
                  hitpos = hdc_wire_coord(hit)
c     this was just calculated above
                  trackpos = track_coord(plane)
                  residual = hitpos - trackpos
                  chi2 = chi2 + (residual / hdc_sigma(plane) )**2
               enddo
c     divide by the number of degrees of freedom of the fit:
               chi2 = chi2 / float(nfree)

               if(firsttry.or.chi2.lt.minchi2) then
                  minchi2 = chi2
                  firsttry = .false.
c     this is the loop over left-right combos of the Y planes!
c     so set plusminusbesty
                  do ihit=1,nyplanes
                     plusminusbesty(ihit) = plusminustest(ihit)
                  enddo
c     set bestcombo, now we know we have at least one successful track fit
                  bestcomboy = icombo
               endif ! end test on first try or best chi2
            endif ! end test on matrix inversion error code
         enddo ! end loop over left-right combinations of y planes
         
c     if a left-right combo of the y planes was chosen, set plusminusy to that combination:
         
         if(bestcomboy.ge.0) then
c     set the left right combination of the y hits to the best found above:
            do ihit=1,nyplanes
               plusminusy(ihit) = plusminusbesty(ihit)
            enddo
         else ! restore the left-right combination of the y hits to its original value,
c     which was stored in plusminusy:
            do ihit=1,nyplanes
               hit = yhits(ihit)
               hdc_wire_coord(hit) = hdc_wire_center(hit) + 
     $              plusminusy(ihit) * hdc_drift_dis(hit)
            enddo
         endif
c     now find the best left-right combination of the x planes:
c     re-initialize firsttry:
         firsttry = .true.
c     begin loop over LR combos of the x planes:
         do icombo = 0,nxcombos-1
c     set wire_coord of y planes based either on original combo or best above:
            do ihit=1,nyplanes
               hit = yhits(ihit)
               hdc_wire_coord(hit) = hdc_wire_center(hit) + 
     $              plusminusy(ihit) * hdc_drift_dis(hit)
            enddo
c     set wire_coord of x planes based on current icombo:
            do ihit=1,nxplanes
c     use bit pattern of icombo to set plusminustest:
               if(jbit(icombo,ihit).eq.1) then
                  plusminustest(ihit) = 1.
               else
                  plusminustest(ihit) = -1.
               endif
               hit = xhits(ihit)
               hdc_wire_coord(hit) = hdc_wire_center(hit) + 
     $              plusminustest(ihit) * hdc_drift_dis(hit)
            enddo
            
c     accumulate system of equations for track fitting:
            
            do i=1,hnum_fpray_param
               TT(i) = 0.
               do ihit=1,nyplanes
                  hit = yhits(ihit)
                  plane = hdc_plane_num(hit)
                  TT(i) = TT(i) + hdc_wire_coord(hit)*hplane_coeff(remap(i),plane) 
     $                 / ( (hdc_sigma(plane) )**2 )
               enddo
               
               do ihit=1,nxplanes
                  hit = xhits(ihit)
                  plane = hdc_plane_num(hit)
                  TT(i) = TT(i) + hdc_wire_coord(hit)*hplane_coeff(remap(i),plane)
     $                 / ( (hdc_sigma(plane))**2 )
               enddo
            enddo
            
            do i=1,hnum_fpray_param
               do j=1,hnum_fpray_param
                  AA(i,j) = 0.
                  if(j.lt.i) then
                     AA(i,j) = AA(j,i)
                  else
                     do ihit=1,nyplanes
                        hit = yhits(ihit)
                        plane = hdc_plane_num(hit)
                        AA(i,j) = AA(i,j) + hplane_coeff(remap(i),plane)*
     $                       hplane_coeff(remap(j),plane) /
     $                       ( (hdc_sigma(plane))**2 )
                     enddo
                     
                     do ihit=1,nxplanes
                        hit = xhits(ihit)
                        plane = hdc_plane_num(hit)
                        AA(i,j) = AA(i,j) + hplane_coeff(remap(i),plane)*
     $                       hplane_coeff(remap(j),plane) /
     $                       ( (hdc_sigma(plane))**2 )
                     enddo
                  endif ! end if on j < i
               enddo ! end inner loop 
            enddo ! end outer loop
c     get the solution:
            
            call solve_four_by_four(TT,AA,dray,ierr)
            
            if(ierr.ne.0) then
               dray(1)=10000.
               dray(2)=10000.
               dray(3)=2.
               dray(4)=2.
            else
               chi2 = 0.
c     calculate track position at each plane:
               do plane=1,hdc_num_planes
                  track_coord(plane) = hplane_coeff(remap(1),plane)*dray(1)
     $                 + hplane_coeff(remap(2),plane)*dray(2)
     $                 + hplane_coeff(remap(3),plane)*dray(3)
     $                 + hplane_coeff(remap(4),plane)*dray(4)
               enddo
c     get the residual of each hit on the test track:
               do ihit=1,nyplanes
                  hit = yhits(ihit)
                  plane = hdc_plane_num(hit)
c     this "wire_coord" is based on the current left-right combo
                  hitpos = hdc_wire_coord(hit)
c     trackpos was just calculated from the fit results of the test track above
                  trackpos = track_coord(plane)
                  residual = hitpos - trackpos
                  
                  chi2 = chi2 + (residual/hdc_sigma(plane) )**2
               enddo
               
               do ihit=1,nxplanes
                  hit = xhits(ihit)
                  plane = hdc_plane_num(hit)
c     wire_coord is based on the current left-right combo
                  hitpos = hdc_wire_coord(hit)
c     trackpos was just calculated from the fit results of the test track above
                  trackpos = track_coord(plane)
                  residual = hitpos - trackpos
                  
                  chi2 = chi2 + (residual/hdc_sigma(plane) )**2
               enddo

               chi2 = chi2 / float(nfree)

               if(firsttry.or.chi2.lt.minchi2) then
                  minchi2 = chi2
                  firsttry = .false.
                  
                  bestcombox = icombo
c     we are in the loop over left-right combos of x planes: 
c     store the current left-right combo of x planes in plusminusbestx:
                  do ihit=1,nxplanes
                     plusminusbestx(ihit) = plusminustest(ihit)
                  enddo
               endif            ! end test on first try or best chi2
            endif               ! end test on matrix inversion error code 
         enddo                  ! end loop over left-right combinations of x planes
c     if a left-right combo of the x planes was chosen, set that combination for the x hits:
         if(bestcombox.ge.0) then
c     store plusminusbestx, the left-right combo that minimizes chi2, in plusminusx, the
c     "final" left-right combination of the x hits:
            do ihit=1,nxplanes
               plusminusx(ihit) = plusminusbestx(ihit)
            enddo
         else ! restore hdc_wire_coord of the x hits to the original left right combination,
c     which is stored in plusminusx:
            do ihit=1,nxplanes
               hit = xhits(ihit)
               hdc_wire_coord(hit) = hdc_wire_center(hit) + 
     $              plusminusx(ihit) * hdc_drift_dis(hit)
            enddo
         endif
c     NOW WE HAVE THE BEST LEFT-RIGHT COMBINATION OF THE X AND Y PLANES--FIT THE TEST 
c     TRACK ONE MORE TIME USING THE BEST LEFT-RIGHT COMBINATION AND USE IT TO PICK THE
c     DRIFT SIGN FOR THE U AND V PLANES, THEN FIT THE FINAL TRACK:
c     AT THIS POINT, WE STILL HAVEN'T MESSED WITH HTRACK_LEFTRIGHT, SO WE CAN STILL
c     RECALL THE ORIGINAL LEFT-RIGHT COMBO IF NEEDED 
         if(bestcombox.ge.0.and.bestcomboy.ge.0) then 
            do ihit=1,nxplanes
c     get array index of hit:
               hit = xhits(ihit)
c     set wire_coord of x plane hits based on best combo determined above:
               hdc_wire_coord(hit) = hdc_wire_center(hit) + 
     $              plusminusx(ihit) * hdc_drift_dis(hit)
            enddo
c     set wire_coord of y plane hits based on best combo determined above:
            do ihit=1,nyplanes
               hit = yhits(ihit)
               hdc_wire_coord(hit) = hdc_wire_center(hit) + 
     $              plusminusy(ihit) * hdc_drift_dis(hit)
            enddo
c     now we need to fit the test track again--should we have just remembered it from above?
            ntest = nxplanes + nyplanes
            nfree = ntest - hnum_fpray_param
            
            do i=1,hnum_fpray_param
               TT(i) = 0.
               do ihit=1,nyplanes
                  hit = yhits(ihit)
                  plane = hdc_plane_num(hit)
                  TT(i) = TT(i) + hdc_wire_coord(hit) * hplane_coeff(remap(i),plane)
     $                 / ( (hdc_sigma(plane) )**2 )
               enddo
               
               do ihit=1,nxplanes
                  hit = xhits(ihit)
                  plane = hdc_plane_num(hit)
                  TT(i) = TT(i) + hdc_wire_coord(hit) * hplane_coeff(remap(i),plane)
     $                 / ( (hdc_sigma(plane) )**2 )
               enddo
            enddo
            
            do i=1,hnum_fpray_param
               do j=1,hnum_fpray_param
                  AA(i,j) = 0.
                  if(j.lt.i) then
                     AA(i,j) = AA(j,i)
                  else
                     do ihit=1,nyplanes
                        hit = yhits(ihit)
                        plane = hdc_plane_num(hit)
                        AA(i,j) = AA(i,j) + hplane_coeff(remap(i),plane)*
     $                       hplane_coeff(remap(j),plane) / 
     $                       ( (hdc_sigma(plane) )**2 )
                     enddo
                     
                     do ihit=1,nxplanes
                        hit = xhits(ihit)
                        plane = hdc_plane_num(hit)
                        AA(i,j) = AA(i,j) + hplane_coeff(remap(i),plane)*
     $                       hplane_coeff(remap(j),plane) / 
     $                       ( (hdc_sigma(plane) )**2 )
                     enddo
                  endif ! end test on j<i
               enddo ! end inner loop over j
            enddo ! end outer loop over i
c     get the solution
            call solve_four_by_four(TT,AA,dray,ierr)
c     ierr!=0 shouldn't happen, but if we get this condition for the test track, we need to revert to
c     the original tracking results
            if(ierr.ne.0) then 
               dray(1)=10000.
               dray(2)=10000.
               dray(3)=2.
               dray(4)=2.
            else
c     use the test track to pick the best left-right for the u and v planes:
c     calculate the track position at all planes:
               do plane=1,hdc_num_planes
                  track_coord(plane) = hplane_coeff(remap(1),plane)*dray(1)
     $                 + hplane_coeff(remap(2),plane)*dray(2)
     $                 + hplane_coeff(remap(3),plane)*dray(3)
     $                 + hplane_coeff(remap(4),plane)*dray(4)
               enddo
c     now check which drift sign for the U planes minimizes their
c     residual with the "final" test track
               do ihit=1,nuplanes
                  hit = uhits(ihit)
                  plane = hdc_plane_num(hit)
c     set utrack:
                  utrack = track_coord(plane)
c     set uhit with positive drift:
                  uhit = hdc_wire_center(hit) + hdc_drift_dis(hit)
c     calculate residual, store in ures(1)
                  ures(1) = uhit - utrack
c     set uhit with negative drift:
                  uhit = hdc_wire_center(hit) - hdc_drift_dis(hit)
c     calculate residual, store in ures(2)
                  ures(2) = uhit - utrack
c     pick the drift sign which minimizes the size of the residual:
                  if(abs(ures(2)).lt.abs(ures(1))) then
c     negative drift gives best residual with test track, set plusminus to -1.
                     plusminusu(ihit) = -1.
                  else
c     positive drift gives best residual with test track, set plusminus to +1.
                     plusminusu(ihit) = 1.
                  endif
c     set final wire coordinate for final track fitting:
                  hdc_wire_coord(hit) = hdc_wire_center(hit) + 
     $                 plusminusu(ihit) * hdc_drift_dis(hit)
               enddo
c     check which drift sign in the V planes minimizes their 
c     residuals on the "final" test track:
               do ihit=1,nvplanes
                  hit = vhits(ihit)
                  plane = hdc_plane_num(hit)
c     set vtrack--generalized track coordinates at all planes were calculated above:
                  vtrack = track_coord(plane)
c     set vhit with positive drift
                  vhit = hdc_wire_center(hit) + hdc_drift_dis(hit)
c     store residual in vres(1):
                  vres(1) = vhit - vtrack
c     set vhit with negative drift:
                  vhit = hdc_wire_center(hit) - hdc_drift_dis(hit)
c     store residual in vres(2):
                  vres(2) = vhit - vtrack
c     pick drift sign which minimizes the size of the residual:
                  if(abs(vres(2)).lt.abs(vres(1))) then
c     negative drift gives best residual with test track, set plusminus to -1.
                     plusminusv(ihit) = -1.
                  else
c     positive drift gives best residual with test track, set plusminus to +1.
                     plusminusv(ihit) = 1.
                  endif
c     set final wire coordinate for final track fitting:
                  hdc_wire_coord(hit) = hdc_wire_center(hit) + 
     $                 plusminusv(ihit) * hdc_drift_dis(hit)
               enddo ! end loop over v planes

c     NOW WIRE COORDS HAVE BEEN SET PROPERLY FOR ALL HITS ON THE TRACK. 
c     FIT THE FINAL TRACK WITH ALL HITS!
c     fit the final track and calculate residuals, chi2, etc. 
c     accumulate the system of equations for track fitting:
c     Note: now we are using the array hntrack_hits, which is 
c     okay because all hits on the track have had their final wire_coord's set:
               do i=1,hnum_fpray_param
                  TT(i) = 0.
                  do ihit=2,hntrack_hits(track,1)+1
                     hit = hntrack_hits(track,ihit)
                     plane = hdc_plane_num(hit)
                     TT(i) = TT(i) + hdc_wire_coord(hit)*
     $                    hplane_coeff(remap(i),plane) / 
     $                    ( (hdc_sigma(plane) )**2 )
                  enddo
               enddo
               do i=1,hnum_fpray_param
                  do j=1,hnum_fpray_param
                     AA(i,j) = 0.
                     if(j.lt.i) then
                        AA(i,j) = AA(j,i)
                     else
                        do ihit=2,hntrack_hits(track,1)+1
                           hit = hntrack_hits(track,ihit)
                           plane = hdc_plane_num(hit)
                           AA(i,j) = AA(i,j) + hplane_coeff(remap(i),plane) *
     $                          hplane_coeff(remap(j),plane) / 
     $                          ( (hdc_sigma(plane) )**2 )
                        enddo
                     endif ! end test on j<i
                  enddo ! end inner loop over j
               enddo ! end outer loop over i
c     get the solution
               call solve_four_by_four(TT,AA,dray,ierr)
c     still need to think about what to do if/when we get these errors
               if(ierr.ne.0) then
                  dray(1) = 10000.
                  dray(2) = 10000.
                  dray(3) = 2.
                  dray(4) = 2.
               else
c     calculate chi2
                  chi2 = 0.
                  do plane=1,hdc_num_planes
                     track_coord(plane) = 0.
                     do j=1,hnum_fpray_param
                        track_coord(plane) = track_coord(plane) + 
     $                       hplane_coeff(remap(j),plane)*dray(j)
                     enddo
                  enddo

                  do ihit=2,hntrack_hits(track,1)+1
                     hit = hntrack_hits(track,ihit)
                     plane = hdc_plane_num(hit)
c     don't mess with common blocks until we test whether chi2 improved with the new 
c     LR combo
                     residual = hdc_wire_coord(hit) - track_coord(plane)
                     chi2  = chi2 + (residual/hdc_sigma(plane))**2
                  enddo
                  
                  nfree = hntrack_hits(track,1) - hnum_fpray_param
                  
                  chi2 = chi2 / float(nfree)

                  if(chi2.lt.originalchi2) then ! set the common block variables associated with this track:
c     set hdc_track_coord-->generalized coordinate of the track at each plane
                     do plane=1,hdc_num_planes
                        hdc_track_coord(track,plane) = track_coord(plane)
                     enddo
c     for each hit, set hdc_single_residual--> track_coord - hit_coord
c     also for each hit, store the final drift sign in htrack_leftright
c     which will be needed if we want to do a t0 determination later:
                     do ihit=2,hntrack_hits(track,1)+1
                        hit = hntrack_hits(track,ihit)
                        plane = hdc_plane_num(hit)
                        hdc_single_residual(track,plane) = hdc_wire_coord(hit) - 
     $                       hdc_track_coord(track,plane)
                        if(hdc_wire_coord(hit).lt.hdc_wire_center(hit)) then
                           htrack_leftright(track,ihit-1) = -1.
                        else
                           htrack_leftright(track,ihit-1) = 1.
                        endif
                     enddo
c     store track fit results and chi2:
                     hx_fp(track) = dray(1)
                     hy_fp(track) = dray(2)
                     hz_fp(track) = 0.
                     hxp_fp(track) = dray(3)
                     hyp_fp(track) = dray(4)
c     convention for hchi2_fp is not to divide by ndf:
                     hchi2_fp(track) = chi2 * float(nfree)
                  else          ! reset hdc_wire_coord to the values obtained using the original left-right combinations:
                     do ihit=2,hntrack_hits(track,1)+1
                        hit = hntrack_hits(track,ihit)
                        hdc_wire_coord(hit) = hdc_wire_center(hit) +  
     $                       htrack_leftright(track,ihit-1) * hdc_drift_dis(hit)
                     enddo
                  endif
               endif ! end test on matrix inversion error code for FINAL track
            endif ! end test on matrix inversion error code for final TEST track
         endif ! end test on found a good combo for x planes and y planes
      endif ! end if on nxplanes >= 3 and nyplanes >= 3-->otherwise we can't do the LR this way. keep what we had before
      
      return 
      end
