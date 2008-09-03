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

c     eliminated some of the code in this routine. Because a lot of what's done here is 
c     duplicated in h_redo_track_left_right.f, we can simply build the hit lists for 
c     tracks here and use h_redo_track_left_right.f to find the best left-right combo.

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
            
               if(abs(dxpstub).le.hxpt_track_criterion) then
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

c     make a track out of every space point combo
c     passing 2 of 3 criteria:

         hntracks_fp = npossible

         do itrack=1,npossible

            sp1 = possible_track_space_points(itrack,1)
            sp2 = possible_track_space_points(itrack,2)
            ifail = possible_track_space_points(itrack,3)
            
            nhit1 = hspace_point_hits(sp1,1)
            nhit2 = hspace_point_hits(sp2,1)
            
            hntrack_hits(itrack,1) = nhit1+nhit2

c     record all the hits on this track along with the drift sign 
c     determined in h_left_right:
            
            do ihit=1,nhit1
               hntrack_hits(itrack,ihit+1) = hspace_point_hits(sp1,ihit+2)
               htrack_leftright(itrack,ihit) = hspace_point_leftright(sp1,ihit)
            enddo
          
            do ihit=1,nhit2
               hntrack_hits(itrack,ihit+1+nhit1) = hspace_point_hits(sp2,ihit+2)
               htrack_leftright(itrack,ihit) = hspace_point_leftright(sp2,ihit)
            enddo

c     the rest will be taken care of in h_track_fit!
            
         enddo
      endif

 666  continue

      return
      end
