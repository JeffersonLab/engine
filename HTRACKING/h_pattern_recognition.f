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
* $Log$
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
      character*50 here
      parameter (here= 'H_PATTERN_RECOGNITION')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 'hms_data_structures.cmn'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
*
*
      include "hms_tracking.cmn"
      include "hms_geometry.cmn"
*     local variables
      integer*4 hit_number(hmax_chamber_hits)
      real*4 space_points(hmax_space_points,2)
      integer*4 space_point_hits(hmax_space_points,hmax_hits_per_point+2)
      integer*4 i,j,k
*
      integer*4 plane, wire, isp, ihit, hit
      real*4 x_pos, x_drifttime_corr, y_pos, y_drifttime_corr
      real*4 h_drift_dist_calc
      external h_drift_dist_calc
*
*     temporary initialization
      ABORT= .FALSE.
      err=':success'
*
*   
*     Chamber 1
      hnspace_points(1)=0
      hncham_hits(1)=HDC_HITS_PER_PLANE(1)+HDC_HITS_PER_PLANE(2)
     &          +HDC_HITS_PER_PLANE(3)+HDC_HITS_PER_PLANE(4)
     &          +HDC_HITS_PER_PLANE(5)+HDC_HITS_PER_PLANE(6)
      if(hncham_hits(1).gt.2 .and. hncham_hits(1).lt. hmax_pr_hits(1))  then
          do i=1,hncham_hits(1)
             hit_number(i)=i
          enddo
          call find_space_points(hncham_hits(1),hit_number(1),
     &        HDC_WIRE_CENTER(1),
     &        HDC_PLANE_NUM(1),hspace_point_criterion(1),
     &        hxsp(1),hysp(1),hmax_space_points,
     &        hnspace_points(1), space_points, space_point_hits)
*    
*    If two hits in same plane, choose one with minimum drift time
         call h_choose_single_hit(ABORT,err,hnspace_points(1),
     &        space_point_hits)
*     select on minimum number of combinations and hits
         call select_space_points(hmax_space_points,
     &        hnspace_points(1), space_points, space_point_hits,
     &        hmin_hit(1),hmin_combos(1))
          do i=1,hnspace_points(1)
             hspace_points(i,1)=space_points(i,1)
             hspace_points(i,2)=space_points(i,2)
             hspace_point_hits(i,1)=space_point_hits(i,1)
             hspace_point_hits(i,2)=space_point_hits(i,2)
             do j=1,space_point_hits(i,1)
               hspace_point_hits(i,j+2)=space_point_hits(i,j+2)
             enddo
          enddo
      endif      
*
*     chamber 2
      hnspace_points(2)=0
      hncham_hits(2)=HDC_HITS_PER_PLANE(7)+HDC_HITS_PER_PLANE(8)
     &     +HDC_HITS_PER_PLANE(9)+HDC_HITS_PER_PLANE(10)
     &     +HDC_HITS_PER_PLANE(11)+HDC_HITS_PER_PLANE(12)
      if(hncham_hits(2).gt.2  .and. hncham_hits(2).lt. hmax_pr_hits(2))  then
        do i=hncham_hits(1)+1,hncham_hits(1)+hncham_hits(2)
*     type *,hncham_hits(1),hncham_hits(2),i
          hit_number(i)=i
        enddo
        call find_space_points(hncham_hits(2),hit_number(hncham_hits(1)+1),
     &       HDC_WIRE_CENTER(hncham_hits(1)+1),
     &       HDC_PLANE_NUM(hncham_hits(1)+1),hspace_point_criterion(2),
     &       hxsp(1),hysp(1),hmax_space_points,
     &       hnspace_points(2), space_points, space_point_hits)
*    
*    If two hits in same plane, choose one with minimum drift time
        call h_choose_single_hit(ABORT,err,hnspace_points(2),
     &       space_point_hits)
*     select on minimum number of combinations and hits
        call select_space_points(hmax_space_points,
     &       hnspace_points(2), space_points, space_point_hits,
     &       hmin_hit(2),hmin_combos(2))
        do i=1,hnspace_points(2)
          k=hnspace_points(1)+i
          hspace_points(k,1)=space_points(i,1)
          hspace_points(k,2)=space_points(i,2)
          hspace_point_hits(k,1)=space_point_hits(i,1)
          hspace_point_hits(k,2)=space_point_hits(i,2)
          do j=1,space_point_hits(i,1)
            hspace_point_hits(k,j+2)=space_point_hits(i,j+2)
          enddo
        enddo
      endif      

      do plane=1,hdc_num_planes
        hdc_sing_wcenter(plane)=-100.
      enddo

*     calculate total numbe of space points
      hnspace_points_tot=hnspace_points(1)+hnspace_points(2)
*
*     Now we know rough hit positions in the chambers so we can
*     Make wire velocity drift time corrections for each hit in the space
*     point
      if(h_wire_vel_correction.ne.0 .and. hnspace_points_tot.gt.0) then
        do isp=1,hnspace_points_tot
*     write(hluno,*)' ** space point',isp
          x_pos = hspace_points(isp,1)
          y_pos = hspace_points(isp,2)
          x_drifttime_corr = hdc_x_central_time + y_pos/hdc_wire_velocity
          y_drifttime_corr = hdc_y_central_time + x_pos/hdc_wire_velocity
*     write(hluno,*)x_pos,x_drifttime_corr,y_pos,y_drifttime_corr
          do ihit=1,hspace_point_hits(isp,1)
            hit = hspace_point_hits(isp,ihit+2)
            plane = HDC_PLANE_NUM(hit)
            wire = HDC_WIRE_NUM(hit)
            if(plane.eq.2 .or. plane.eq.5 .or. 
     &           plane.eq.8 .or. plane.eq.11) then ! Y or Y'  plane
              HDC_DRIFT_TIME(hit)=HDC_DRIFT_TIME(hit) - y_drifttime_corr
            else                        ! X,U,V,X' plane
              HDC_DRIFT_TIME(hit)=HDC_DRIFT_TIME(hit) - x_drifttime_corr
            endif
            HDC_DRIFT_DIS(hit) = h_drift_dist_calc
     &           (plane,wire,HDC_DRIFT_TIME(hit))
*     write(hluno,*)ihit,hit,HDC_DRIFT_TIME(hit),HDC_DRIFT_DIS(hit)

* djm 8/25/94
* Stuff drift time and distance into registered variables for histogramming and tests.
* In the case of two separated hits per plane, the last one will be histogrammed.

            hdc_sing_drifttime(plane) = HDC_DRIFT_TIME(hit)
            hdc_sing_driftdis(plane) = HDC_DRIFT_DIS(hit)
* djm 9/16/94
* add some wire positions for each plane 
            hdc_sing_wcenter(plane) = HDC_WIRE_CENTER(hit)

          enddo
        enddo
      endif
*     
*     Histogram HDC_DECODED_DC
      call h_fill_dc_dec_hist(ABORT,err)
*     
*     write out results if debugflagpr is set
      if(hdebugflagpr.ne.0) then
        call h_print_pr
      endif
*     
      return
      end
