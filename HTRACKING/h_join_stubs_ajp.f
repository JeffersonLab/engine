      subroutine h_join_stubs_ajp(abort,err)

      implicit none
      save

      character*16 here
      parameter(here='h_join_stubs_ajp')

      include 'hms_data_structures.cmn'
      include 'hms_tracking.cmn'
      include 'hms_track_histid.cmn'
      include 'hms_geometry.cmn'

      logical abort
      character*(*) err
      
      integer isp,jsp,npass,npossible,ngoodpossible
      integer possible_good_tracks(hntracks_max),igoodtrack

c     second array index of possible tracks
c     possible_track_space_points(track,1) = space point in first chamber
c     possible_track_space_points(track,2) = space point in second chamber
c     possible_track_space_points(track,3) = which stub link criterion failed

      integer possible_track_space_points(hntracks_max,3)

      integer possible_track_hits(hntracks_max,hntrackhits_max+1)

      real*4 possible_track_leftright(hntracks_max,hntrackhits_max)

      integer ierr

      real*4 dxstub,dystub,dxpstub
      real*4 chi2,minchi2

      integer ifail
      integer itrack,sp1,sp2,nhit1,nhit2,ihit,hit,ntest
      integer nxplanes,nyplanes,nuplanes,nvplanes
      integer nxcombos !(max of 16 new combos to try) because we are only testing 4 planes
      integer nycombos

      integer plane,wire
      real*4 ddist

      real*4 xbest,ybest,xpbest,ypbest

      real*4 plusminusx(4),plusminusy(4),plusminusbestx(4),plusminusbesty(4)
      real*4 uhit,utrack,uresplus,uresminus,vhit,vtrack,vresplus,vresminus
      integer xhits(4),yhits(4),uhits(2),vhits(2)
      integer icombo,bestcombox,bestcomboy
      
      integer remember1,remember2

      logical firsttry

      integer*4 h_chamnum
      external h_chamnum

      external jbit             ! cernlib bit routine
      external jibset
      integer*4 jbit
      integer*4 jibset          ! Declare to help f2c

*     array to remap hplane_coeff to param number
      integer*4 remap(hnum_fpray_param)
      data remap/5,6,3,4/

      abort=.false.
      err=' '

c     attempt to find tracks where none existed before: 
c     approach is to figure out if a combination of 
c     two stubs, one from each chamber, passes two of three 
c     stub linking criteria, and then, for the other 
c     cut which failed, see if we can get the stubs to agree by trying a different 
c     left-right combination of the hits (and, in the case of y, allowing a nonzero 
c     dy/dz)

      npass = 0
      npossible = 0

      remember1 = hbypass_redo_leftright
      remember2 = hbypass_track_t0

      hbypass_redo_leftright = 1
      hbypass_track_t0 = 1

c     build all possible combinations of one space point from each chamber, 
c     that pass at least two out of three stub linking criteria:
      
      do isp=1,hnspace_points_tot-1
         do jsp=isp+1,hnspace_points_tot
            if(h_chamnum(isp).ne.h_chamnum(jsp)) then
               dxstub = hbeststub(isp,1) - hbeststub(jsp,1)
               dystub = hbeststub(isp,2) - hbeststub(jsp,2)
               dxpstub = hbeststub(isp,3) - hbeststub(jsp,3)

               npass = 0
               ifail = 0

               if(abs(dxstub).le.hxt_track_criterion) then
                  npass = npass + 1
               else
                  ifail = 1
               endif
            
               if(abs(dystub).le.hyt_track_criterion) then
                  npass = npass + 1
               else
                  ifail = 2
               endif
            
               if(abs(dxstub).le.hxpt_track_criterion) then
                  npass = npass + 1
               else 
                  ifail = 3
               endif

               if(npass.eq.2) then
                  npossible = npossible + 1 
                  if(npossible.gt.hntracks_max_fp) then
                     npossible = 0
c                     write(*,*) 'found too many possible tracks'
                     goto 666
                  endif

                  if(h_chamnum(isp).eq.1) then
                     possible_track_space_points(npossible,1) = isp

                     hx_sp1(npossible) = hbeststub(isp,1)
                     hy_sp1(npossible) = hbeststub(isp,2)
                     hxp_sp1(npossible) = hbeststub(isp,3)
                     hchi2_sp1(npossible) = h_stub_chi2perdf(isp)

                     possible_track_space_points(npossible,2) = jsp

                     hx_sp2(npossible) = hbeststub(jsp,1)
                     hy_sp2(npossible) = hbeststub(jsp,2)
                     hxp_sp2(npossible) = hbeststub(jsp,3)
                     hchi2_sp2(npossible) = h_stub_chi2perdf(jsp)

                  else
                     possible_track_space_points(npossible,1) = jsp
                     
                     hx_sp1(npossible) = hbeststub(jsp,1)
                     hy_sp1(npossible) = hbeststub(jsp,2)
                     hxp_sp1(npossible) = hbeststub(jsp,3)
                     hchi2_sp1(npossible) = h_stub_chi2perdf(jsp)

                     possible_track_space_points(npossible,2) = isp

                     hx_sp2(npossible) = hbeststub(isp,1)
                     hy_sp2(npossible) = hbeststub(isp,2)
                     hxp_sp2(npossible) = hbeststub(isp,3)
                     hchi2_sp2(npossible) = h_stub_chi2perdf(isp)

                  endif
                  possible_track_space_points(npossible,3) = ifail
               endif
            endif
         enddo
      enddo

      if(npossible.gt.0) then 

c     loop over all the possible track combinations:

         ngoodpossible = 0

         do itrack=1,npossible

            nxplanes = 0
            nyplanes = 0
            nuplanes = 0
            nvplanes = 0

            sp1 = possible_track_space_points(itrack,1)
            sp2 = possible_track_space_points(itrack,2)
            ifail = possible_track_space_points(itrack,3)

            nhit1 = hspace_point_hits(sp1,1)
            nhit2 = hspace_point_hits(sp2,1)

            possible_track_hits(itrack,1) = nhit1+nhit2

c     count up all the hits on the track and count the number of x planes and y planes

            do ihit = 1,nhit1
               hit = hspace_point_hits(sp1,ihit+2)
               possible_track_hits(itrack,ihit+1) = hit
c     set left-right combination of these hits based on memory from h_left_right:
               possible_track_leftright(itrack,ihit) = 
     $              hspace_point_leftright(sp1,ihit)

               if(hdc_plane_num(hit).eq.1.or.hdc_plane_num(hit).eq.6) 
     $              then              
                  nxplanes = nxplanes + 1
                  xhits(nxplanes) = ihit
               endif
               if(hdc_plane_num(hit).eq.2.or.hdc_plane_num(hit).eq.5) 
     $              then 
                  nyplanes = nyplanes + 1
                  yhits(nyplanes) = ihit
               endif
               if(hdc_plane_num(hit) .eq. 3) then
                  nuplanes = nuplanes + 1
                  uhits(nuplanes) = ihit
               endif
               if(hdc_plane_num(hit) .eq. 4) then
                  nvplanes = nvplanes + 1
                  vhits(nvplanes) = ihit
               endif
            enddo
            do ihit=1,nhit2
               hit = hspace_point_hits(sp2,ihit+2)
               possible_track_hits(itrack,ihit+1+nhit1) = hit
c     set left-right combination of these hits based on memory from h_left_right:
               possible_track_leftright(itrack,ihit+nhit1) = 
     $              hspace_point_leftright(sp2,ihit)
               if(hdc_plane_num(hit).eq.7.or.hdc_plane_num(hit).eq.12)
     $              then
                  nxplanes = nxplanes + 1
                  xhits(nxplanes) = ihit + nhit1
               endif
               if(hdc_plane_num(hit).eq.8.or.hdc_plane_num(hit).eq.11)
     $              then
                  nyplanes = nyplanes + 1
                  yhits(nyplanes) = ihit + nhit1
               endif
               if(hdc_plane_num(hit).eq.9) then
                  nuplanes = nuplanes + 1
                  uhits(nuplanes) = ihit + nhit1
               endif
               if(hdc_plane_num(hit).eq.10) then
                  nvplanes = nvplanes + 1
                  vhits(nvplanes) = ihit + nhit1
               endif
            enddo
            
            if(nxplanes.ge.3.and.nyplanes.ge.3) then ! try and re-fit the left-right combination of the x and y hits
                  
c$$$               if(ifail.eq.1.or.ifail.eq.3)
c$$$     $              then
c$$$                  ncombos = 2**nxplanes
c$$$               else if(ifail.eq.2) then
c$$$                  ncombos = 2**nyplanes
c$$$               endif

               nxcombos = 2**nxplanes
               nycombos = 2**nyplanes

               firsttry = .true.
                  
               bestcombox = -1
               bestcomboy = -1

               do icombo=0,nxcombos-1
c     set the left-right of each xhit
                  hit=1         ! counter for x or y plane hits
                  ntest = nxplanes + nyplanes ! counter for all hits on test track
                  hntracks_fp = 1 ! temporarily set number of tracks to 1
                  
                  hntrack_hits(1,1) = ntest
                  
                  do ihit=1,nxplanes
                     hntrack_hits(1,ihit+1) = possible_track_hits(itrack,xhits(ihit)+1)
c                     if(ifail.eq.1.or.ifail.eq.3) then
                     if(jbit(icombo,hit).eq.1) then
                        plusminusx(hit) = 1.
                        htrack_leftright(1,ihit) = 1.
                     else 
                        plusminusx(hit) = -1.
                        htrack_leftright(1,ihit) = -1.
                     endif
                     hit = hit + 1
c$$$  else
c$$$  htrack_leftright(1,ihit) = possible_track_leftright(itrack,xhits(ihit))
c$$$  endif
                  enddo

                  do ihit=1,nyplanes
                     hntrack_hits(1,ihit+1+nxplanes) = possible_track_hits(itrack,yhits(ihit)+1)
c$$$  if(ifail.eq.2) then
c$$$  if(jbit(icombo,hit).eq.1) then
c$$$  plusminus(hit) = 1.
c$$$  htrack_leftright(1,ihit+nxplanes) = 1.
c$$$  else
c$$$  plusminus(hit) = -1.
c$$$  htrack_leftright(1,ihit+nxplanes) = -1.
c$$$  endif
c$$$  hit = hit + 1
c$$$  else
                     htrack_leftright(1,ihit+nxplanes) = possible_track_leftright(itrack,yhits(ihit))
c     endif
                  enddo
                     
c     done adding all x and y hits to the test track
                  if(ntest.gt.4) then
c     ******************* FIT THE TEST TRACK: *********************
                     call H_TRACK_FIT(ABORT,err,ierr)
                     if(ABORT) then
                        call G_add_path(here,err)
                        return
                     endif
                     
*     Check for internal error in H_TRACK_FIT 
                     if(ierr.ne.0) then
c     line_err=' '
c     call CSETDI(ierr,line_err,1,5)
                        err='ERROR IN H_TRACK_FIT' !// line_err
                        call G_add_path(here,err)
                        call G_LOG_MESSAGE(err)
                     endif
c     ******************** DONE FITTING THE TEST TRACK ***********************
                     chi2 = hchi2_fp(1) / float(ntest-4)
                     
                     if(firsttry.or.chi2.lt.minchi2) then
                        firsttry=.false.
                        minchi2 = chi2
                        do hit=1,nxplanes
                           plusminusbestx(hit) = plusminusx(hit)
                        enddo
                        bestcombox = icombo
                        
c$$$                        xbest = hx_fp(1)
c$$$                        ybest = hy_fp(1)
c$$$                        xpbest = hxp_fp(1)
c$$$                        ypbest = hyp_fp(1)

                     endif
                  endif
               enddo            ! end loop over x plane left-right combinations

c     SET LEFT-RIGHT OF X PLANES FROM THE COMBINATION GIVING THE BEST CHI2 FOR THE Y PLANE FITTING

               if(bestcombox.ge.0) then
                  do hit=1,nxplanes
                     possible_track_leftright(itrack,xhits(hit)) = 
     $                    plusminusbestx(hit)
                  enddo
               endif

               firsttry = .true.

               do icombo=0,nycombos-1
                  hit = 1
                  ntest = nxplanes + nyplanes
                  
                  hntracks_fp = 1 ! temporarily set number of tracks to 1
                  
                  hntrack_hits(1,1) = ntest

                  do ihit=1,nxplanes
                     hntrack_hits(1,ihit+1) = possible_track_hits(itrack,xhits(ihit)+1)
c                     if(ifail.eq.1.or.ifail.eq.3) then
c$$$                     if(jbit(icombo,hit).eq.1) then
c$$$                        plusminusx(hit) = 1.
c$$$                        htrack_leftright(1,ihit) = 1.
c$$$                     else 
c$$$                        plusminusx(hit) = -1.
c$$$                        htrack_leftright(1,ihit) = -1.
c$$$                     endif
c$$$                     hit = hit + 1
c$$$  else
                     htrack_leftright(1,ihit) = possible_track_leftright(itrack,xhits(ihit))
c$$$  endif
                  enddo

                  do ihit=1,nyplanes
                     hntrack_hits(1,ihit+1+nxplanes) = possible_track_hits(itrack,yhits(ihit)+1)
c$$$  if(ifail.eq.2) then
                     if(jbit(icombo,hit).eq.1) then
                        plusminusy(hit) = 1.
                        htrack_leftright(1,ihit+nxplanes) = 1.
                     else
                        plusminusy(hit) = -1.
                        htrack_leftright(1,ihit+nxplanes) = -1.
                     endif
                     hit = hit + 1
c$$$  else
c                     htrack_leftright(1,ihit+nxplanes) = possible_track_leftright(itrack,yhits(ihit))
c     endif
                  enddo

c     done adding all x and y hits to the test track
                  if(ntest.gt.4) then
c     ******************* FIT THE TEST TRACK: *********************
                     call H_TRACK_FIT(ABORT,err,ierr)
                     if(ABORT) then
                        call G_add_path(here,err)
                        return
                     endif
                     
*     Check for internal error in H_TRACK_FIT 
                     if(ierr.ne.0) then
c     line_err=' '
c     call CSETDI(ierr,line_err,1,5)
                        err='ERROR IN H_TRACK_FIT' !// line_err
                        call G_add_path(here,err)
                        call G_LOG_MESSAGE(err)
                     endif
c     ******************** DONE FITTING THE TEST TRACK ***********************
                     chi2 = hchi2_fp(1) / float(ntest-4)
                     
                     if(firsttry.or.chi2.lt.minchi2) then
                        firsttry=.false.
                        minchi2 = chi2
                        do hit=1,nxplanes
                           plusminusbesty(hit) = plusminusy(hit)
                        enddo
                        bestcomboy = icombo
                        
c$$$                        xbest = hx_fp(1)
c$$$                        ybest = hy_fp(1)
c$$$  xpbest = hxp_fp(1)
c$$$  ypbest = hyp_fp(1)
                        
                     endif
                  endif

               enddo ! end loop over y plane left right combinations

               if(bestcomboy.ge.0) then
                  do hit=1,nyplanes
                     possible_track_leftright(itrack,yhits(hit)) = 
     $                    plusminusbesty(hit)
                  enddo
               endif

               if(bestcombox.ge.0.and.bestcomboy.ge.0) then ! set left-right of x hits, pick left-right of u and v hits:
                  ngoodpossible = ngoodpossible + 1
                  
                  possible_good_tracks(ngoodpossible) = itrack
                  
c     BEST LEFT RIGHT OF X AND Y PLANES HAVE BEEN SET: NOW FIT THE TRACK ONE MORE TIME AND USE IT TO PICK THE BEST 
c     LEFT RIGHT FOR THE U AND V PLANES:

                  hntracks_fp = 1
                  ntest = nxplanes + nyplanes
                  hntrack_hits(1,1) = ntest
                  
                  do ihit=1,nxplanes
                     hntrack_hits(1,ihit+1) = possible_track_hits(itrack,xhits(ihit)+1)
                     htrack_leftright(1,ihit) = possible_track_leftright(itrack,xhits(ihit))
                  enddo
                  do ihit=1,nyplanes
                     hntrack_hits(1,ihit+1) = possible_track_hits(itrack,yhits(ihit)+1)
                     htrack_leftright(1,ihit) = possible_track_leftright(itrack,yhits(ihit))
                  enddo
c     ******************* FIT THE TEST TRACK: *********************
                  call H_TRACK_FIT(ABORT,err,ierr)
                  if(ABORT) then
                     call G_add_path(here,err)
                     return
                  endif
                  
*     Check for internal error in H_TRACK_FIT 
                  if(ierr.ne.0) then
c     line_err=' '
c     call CSETDI(ierr,line_err,1,5)
                     err='ERROR IN H_TRACK_FIT' !// line_err
                     call G_add_path(here,err)
                     call G_LOG_MESSAGE(err)
                  endif
c     ******************** DONE FITTING THE TEST TRACK ***********************
c     THIS TIME, JUST GET BEST TRACK FIT PARAMETERS:
           
                  xbest = hx_fp(1)
                  ybest = hy_fp(1)
                  xpbest = hxp_fp(1)
                  ypbest = hyp_fp(1)
                  
c$$$  if(ifail.eq.1.or.ifail.eq.3) then
c$$$                     do ihit=1,nxplanes
c$$$                        possible_track_leftright(itrack,xhits(ihit)) = 
c$$$     $                       plusminusbest(ihit)
c$$$                     enddo
c$$$                  else if(ifail.eq.2) then
c$$$                     do ihit=1,nyplanes
c$$$                        possible_track_leftright(itrack,yhits(ihit)) = 
c$$$     $                       plusminusbest(ihit)
c$$$                     enddo
c$$$                  endif
c     LOOP OVER U HITS AND V HITS ON THIS TRACK, AND PICK THE DRIFT DISTANCE SIGN THAT MINIMIZES THE 
c     IN-PLANE DISTANCE BETWEEN THE TRACK AND THE HIT:
                  do ihit=1,nuplanes
                     hit = possible_track_hits(itrack,uhits(ihit)+1)
                     uhit = hdc_wire_center(hit) - hdc_drift_dis(hit)
                     
                     utrack = hplane_coeff(remap(1),hdc_plane_num(hit))*xbest +
     $                    hplane_coeff(remap(2),hdc_plane_num(hit))*ybest + 
     $                    hplane_coeff(remap(3),hdc_plane_num(hit))*xpbest + 
     $                    hplane_coeff(remap(4),hdc_plane_num(hit))*ypbest 
                     
                     uresminus = uhit - utrack
                     
                     uhit = hdc_wire_center(hit) + hdc_drift_dis(hit)
                     
                     uresplus = uhit - utrack
                    
                     if(abs(uresminus).lt.abs(uresplus)) then
                        possible_track_leftright(itrack,uhits(ihit)) = -1.
                     else
                        possible_track_leftright(itrack,uhits(ihit)) = 1.
                     endif
                  enddo

                  do ihit=1,nvplanes
                     hit = possible_track_hits(itrack,vhits(ihit)+1)
                     vhit = hdc_wire_center(hit) - hdc_drift_dis(hit)
                     
                     vtrack = hplane_coeff(remap(1),hdc_plane_num(hit))*xbest +
     $                    hplane_coeff(remap(2),hdc_plane_num(hit))*ybest + 
     $                    hplane_coeff(remap(3),hdc_plane_num(hit))*xpbest + 
     $                    hplane_coeff(remap(4),hdc_plane_num(hit))*ypbest 
                     vresminus = vhit - vtrack
                     vhit = hdc_wire_center(hit) + hdc_drift_dis(hit)
                     vresplus = vhit - vtrack
                     if(abs(vresminus).lt.abs(vresplus)) then
                        possible_track_leftright(itrack,vhits(ihit)) = -1.
                     else 
                        possible_track_leftright(itrack,vhits(ihit)) = 1.
                     endif
                  enddo
               endif
            endif
         enddo                  ! end loop over possible tracks
         
c     loop over possible tracks again, but this time filling up the master tracking arrays
c     for the final track fitting
         hntracks_fp = ngoodpossible
         
c$$$  write(*,*) 'found ',ngoodpossible,
c$$$  $        ' possible tracks from space points '//
c$$$  $        'passing 2/3 stub tests'
         do itrack=1,ngoodpossible
            
            igoodtrack = possible_good_tracks(itrack)
            
            hntrack_hits(itrack,1) = possible_track_hits(igoodtrack,1)
            do ihit=2,hntrack_hits(itrack,1)+1
               hntrack_hits(itrack,ihit) = possible_track_hits(igoodtrack,ihit)
               htrack_leftright(itrack,ihit-1) = possible_track_leftright(igoodtrack,ihit-1)
            enddo
         enddo
      endif

 666  continue

      hbypass_redo_leftright = remember1
      hbypass_track_t0 = remember2
 
      return
      end
