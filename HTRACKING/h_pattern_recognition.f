      subroutine H_PATTERN_RECOGNITION(ABORT,err)
*--------------------------------------------------------
*-
*-   Purpose and Methods :  Finds HMS Space points 
*-
*-      Required Input BANKS     HMS_DECODED_DC
*-
*-      Output BANKS             HMS_FOCAL_PLANE
*-                               HMS_DECODED_DC hit coordinates
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
*-   Created 30-AUG-1993   D. F. Geesaman
*-   Modified 19-JAN-1994  DFG    Include standard error form
* $Log: h_pattern_recognition.f,v $
* Revision 1.14  2003/04/01 13:49:27  jones
* Modifications to tracking codes.
* Mainly fix problems at high rates. (M. E. Christy)
*
* Revision 1.13  1996/11/05 21:51:08  saw
* (JRA) Initialize hdc_sing_drifttime elements to -100
*
* Revision 1.12  1996/04/30 12:45:36  saw
* (JRA) Histogram the card id.
*
* Revision 1.11  1996/01/16 21:53:24  cdaq
* (JRA) Add code for easy space points
*
* Revision 1.10  1995/08/31 14:49:32  cdaq
* (JRA) Fix typo
*
* Revision 1.9  1995/05/22  19:39:15  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.8  1994/10/11  19:01:38  cdaq
* (DJM) Move hdc_sing_wcoord filling into h_left_right
*
* Revision 1.7  1994/09/19  20:31:39  cdaq
* (DJM) add some histogrammable wire positions and fine positions for each plane
*
* Revision 1.6  1994/08/31  19:39:43  cdaq
* (DJM) Stuff drift time and distance into histogrammable and testable
*       registered variables.
*
* Revision 1.5  1994/08/22  19:54:11  cdaq
* (DJA) Correct sign errors in wire velocity correction
*
* Revision 1.4  1994/08/16  13:08:50  cdaq
* (DJA) Add wire velocity correction
*
* Revision 1.3  1994/06/30  02:27:48  cdaq
* (DFG) Place a limit on total nubmer of hits in each chamber
*       Add filter to get minimum drift time in each plane
*
* Revision 1.2  1994/02/21  03:17:53  cdaq
* (SAW) Removed reference to 3rd chamber in hnspace_points
*
* Revision 1.1  1994/02/19  06:15:47  cdaq
* Initial revision
*
*-
*
*     This routine finds the space points in each chamber using wire center
*     locations.
*
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*21 here
      parameter (here= 'H_PATTERN_RECOGNITION')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 'hms_data_structures.cmn'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      INCLUDE 'hms_tracking.cmn'
      INCLUDE 'hms_geometry.cmn'
*
*     local variables
      integer*4 hit_number(hmax_chamber_hits)
      integer*4 space_point_hits(hmax_space_points,hmax_hits_per_point+2)
      integer*4 pln, isp, ihit, hit
      integer*4 i,j,k,yy,yyprime
      integer*4 yplane,yprimeplane
      integer*4 ich, ip
      logical easy_space_point
*
      real*4 space_points(hmax_space_points,2)
      real*4 xdist,ydist
      real*4 time_corr
      real*4 h_drift_dist_calc
      external h_drift_dist_calc

*
*     temporary initialization
      ABORT= .FALSE.
      err=' '
*
*   
      do pln=1,12
        hdc_sing_drifttime(pln)=-100
      enddo

      ihit = 0
      hnspace_points_tot = 0
      do ich=1,hdc_num_chambers
        easy_space_point = .false.
        hnspace_points(ich)=0
        hncham_hits(ich)=0
         
        do i = 1,hmax_space_points  !!  Initialize - MEC  !!   
         space_points(i,1) = 0
         space_points(i,2) = 0
        enddo

*
*     For this loop to work, hdc_planes_per_chamber must be
*     the number of planes per chamber.  (And all chambers must have the
*     same number of planes.)
*
        do ip=(ich-1)*hdc_planes_per_chamber+1,ich*hdc_planes_per_chamber
          hncham_hits(ich)=hncham_hits(ich)+hdc_hits_per_plane(ip)
        enddo
        yplane=2+(ich-1)*hdc_planes_per_chamber
        yprimeplane=5+(ich-1)*hdc_planes_per_chamber
        if(hncham_hits(ich).ge.hmin_hit(ich) .and.
     $       hncham_hits(ich).lt.hmax_pr_hits(ich))  then
          do i=ihit+1,ihit+hncham_hits(ich)
            hit_number(i)=i
            if(hdc_plane_num(i).eq.yplane) yy=i
            if(hdc_plane_num(i).eq.yprimeplane) yyprime=i
          enddo
          if((hdc_hits_per_plane(yplane).eq.1) .and.
     &         (hdc_hits_per_plane(yprimeplane).eq.1).and.
     &         ((hdc_wire_center(yy)-hdc_wire_center(yyprime))**2.lt.
     &         (hspace_point_criterion(ich))) .and.
     &         (hncham_hits(ich).le.6)) then
            call h_find_easy_space_point(hncham_hits(ich),hit_number(ihit+1),
     &           hdc_wire_center(ihit+1),hdc_plane_num(ihit+1),
     &           hspace_point_criterion(ich),hmax_space_points,yy-ihit,
     &           yyprime-ihit,easy_space_point,hnspace_points(ich),
     &           space_points,space_point_hits)
            if (.not.easy_space_point) call find_space_points(hncham_hits(ich)
     $           ,hit_number(ihit+1),hdc_wire_center(ihit+1)
     $           ,hdc_plane_num(ihit+1),hspace_point_criterion(ich),hxsp(1)
     $           ,hysp(1),hmax_space_points,hnspace_points(ich), space_points,
     $           space_point_hits)
          else
            call find_space_points(hncham_hits(ich),hit_number(ihit+1),
     &           hdc_wire_center(ihit+1),
     &           hdc_plane_num(ihit+1),hspace_point_criterion(ich),
     &           hxsp(1),hysp(1),hmax_space_points,
     &           hnspace_points(ich), space_points, space_point_hits)
          endif
*
          if (hnspace_points(ich).gt.0) then
*    If two hits in same plane, choose one with minimum drift time

             if ( h_remove_sppt_if_one_y_plane .eq. 1) then
             call h_sp_destroy(ABORT,err,hnspace_points(ich),
     &           space_point_hits,space_points,ich)
             endif
c
             call h_sp_multiwire(ABORT,err,hnspace_points(ich),
     &           space_point_hits,space_points)
c
            call h_choose_single_hit(ABORT,err,hnspace_points(ich),
     &           space_point_hits)
* Select on minimum number of combinations and hits
            call select_space_points(hmax_space_points,hnspace_points(ich),
     &           space_points,space_point_hits,hmin_hit(ich),hmin_combos(ich),
     $           easy_space_point)
          endif


          do i=1,hnspace_points(ich)
            k=hnspace_points_tot+i
            hspace_points(k,1)=space_points(i,1)
            hspace_points(k,2)=space_points(i,2)
            hspace_point_hits(k,1)=space_point_hits(i,1)
            hspace_point_hits(k,2)=space_point_hits(i,2)
            do j=1,space_point_hits(i,1)
              hspace_point_hits(k,j+2)=space_point_hits(i,j+2)
            enddo
          enddo
        endif      
        hnspace_points_tot = hnspace_points_tot+ hnspace_points(ich)
        ihit = ihit + hncham_hits(ich)
      enddo
*
* Now we know rough hit positions in the chambers so we can make
* wire velocity drift time corrections for each hit in the space point
*
* Assume all wires for a plane are read out on the same side (l/r or t/b).
* If the wire is closer to horizontal, read out left/right.  If nearer
* vertical, assume top/bottom.  (Note, this is not always true for the
* SOS u and v planes.  They have 1 card each on the side, but the overall
* time offset per card will cancel much of the error caused by this.  The
* alternative is to check by card, rather than by plane and this is harder.
*
      if(hnspace_points_tot.gt.0) then
        do isp=1,hnspace_points_tot
          xdist = hspace_points(isp,1)
          ydist = hspace_points(isp,2)
          do ihit=1,hspace_point_hits(isp,1)
            hit = hspace_point_hits(isp,ihit+2)
            pln = hdc_plane_num(hit)
            if (hdc_readout_x(pln)) then !readout from side
              time_corr = ydist*hdc_readout_corr(pln)/hdc_wire_velocity
            else                        !readout from top/bottom
              time_corr = xdist*hdc_readout_corr(pln)/hdc_wire_velocity
            endif
            
            hdc_drift_time(hit)=hdc_drift_time(hit) - hdc_central_time(pln)
     &           + hdc_drifttime_sign(pln)*time_corr
            hdc_drift_dis(hit) = h_drift_dist_calc
     &           (pln,hdc_wire_num(hit),hdc_drift_time(hit))
*
* djm 8/25/94
* Stuff drift time and distance into registered variables for histogramming and tests.
* In the case of two separated hits per plane, the last one will be histogrammed.
            hdc_sing_drifttime(pln) = hdc_drift_time(hit)
            hdc_sing_driftdis(pln) = hdc_drift_dis(hit)
            hdc_sing_cardid(pln) =
     &           hdc_card_no(hdc_wire_num(hit),hdc_plane_num(hit))
          enddo
        enddo
      endif
*     
*     Histogram hdc_DECODED_DC
      call h_fill_dc_dec_hist(ABORT,err)

*     write out results if debugflagpr is set
      if(hdebugflagpr.ne.0) then
        call h_print_pr
      endif
*     
      return
      end
