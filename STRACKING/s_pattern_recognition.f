      subroutine S_PATTERN_RECOGNITION(ABORT,err)
*--------------------------------------------------------
*-
*-   Purpose and Methods :  Finds SOS Space points 
*-
*-      Required Input BANKS     SOS_DECODED_DC
*-
*-      Output BANKS             SOS_FOCAL_PLANE
*-                               SOS_DECODED_DC hit coordinates
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
*-   Created 30-AUG-1993   D. F. Geesaman
*-   Modified 19-JAN-1994  DFG    Include standard error form
* $Log$
* Revision 1.2  1994/11/22 21:48:45  cdaq
* (SPB) Recopied from hms file and modified names for SOS
* (SAW) Improved some code hardwired for 3 chambers.  NOTE: the wire velocity
*       correction stuff at the end is HMS specific.  This needs to
*       be worked on.
*
* Revision 1.1  1994/02/21  16:15:19  cdaq
* Initial revision
*
*
*     This routine finds the space points in each chamber using wire center
*     locations.
*
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*50 here
      parameter (here= 'S_PATTERN_RECOGNITION')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 'gen_data_structures.cmn'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
*
*
      include 'sos_tracking.cmn'
      include 'sos_geometry.cmn'
*     local variables
      integer*4 hit_number(smax_chamber_hits)
      real*4 space_points(smax_space_points,2)
      integer*4 space_point_hits(smax_space_points,smax_hits_per_point+2)
      integer*4 i,j,k
*
      integer*4 plane, wire, isp, ihit, hit
      integer*4 ich, ip
      real*4 x_pos, x_drifttime_corr, y_pos, y_drifttime_corr
      real*4 s_drift_dist_calc
      external s_drift_dist_calc

*
*     temporary initialization
      ABORT= .FALSE.
      err=':success'
*
*   
      ihit = 0
      do ich=1,SDC_NUM_CHAMBERS
        snspace_points(ich)=0        
        sncham_hits(ich)=0
*
*     For this loop to work, SDC_NUM_PLANES/SDC_NUM_CHAMBERS must be
*     the number of planes per chamber.  (And all chambers must have the
*     same number of planes.)
*
        do ip=(ich-1)*(SDC_NUM_PLANES/SDC_NUM_CHAMBERS)+1,
     $       ich*(SDC_NUM_PLANES/SDC_NUM_CHAMBERS)
          sncham_hits(ich)=sncham_hits(ich)+SDC_HITS_PER_PLANE(ip)
        enddo
        if(sncham_hits(ich).gt.2 .and. sncham_hits(ich).lt.
     $       smax_pr_hits(ich))  then
          do i=ihit+1,ihit+sncham_hits(ich)
            hit_number(i)=i
          enddo
          call find_space_points(sncham_hits(ich),hit_number(ihit+1),
     &         SDC_WIRE_CENTER(ihit+1),
     &         SDC_PLANE_NUM(ihit+1),sspace_point_criterion(ich),
     &         sxsp(1),sysp(1),smax_space_points,
     &         snspace_points(ich), space_points, space_point_hits)
*    
*    If two hits in same plane, choose one with minimum drift time
          call s_choose_single_hit(ABORT,err,snspace_points(ich),
     &         space_point_hits)
*    
*    
*     select on minimum number of combinations and hits
          call select_space_points(smax_space_points,
     &         snspace_points(ich), space_points, space_point_hits,
     &         smin_hit(ich),smin_combos(ich))
          do i=1,snspace_points(ich)
            k=ihit+i
            sspace_points(k,1)=space_points(i,1)
            sspace_points(k,2)=space_points(i,2)
            sspace_point_hits(k,1)=space_point_hits(i,1)
            sspace_point_hits(k,2)=space_point_hits(i,2)
            do j=1,space_point_hits(i,1)
              sspace_point_hits(k,j+2)=space_point_hits(i,j+2)
            enddo
          enddo
        endif
        ihit = ihit + sncham_hits(ich)
      enddo

      do plane=1,SDC_NUM_PLANES
        sdc_sing_wcenter(plane)=-100.
      enddo
      
*     calculate total number of space points
      snspace_points_tot=snspace_points(1)+snspace_points(2)+snspace_points(3)


*
*     Now we know rough hit positions in the chambers so we can
*     Make wire velocity drift time corrections for each hit in the space
*     point
      if(s_wire_vel_correction.ne.0 .and. snspace_points_tot.gt.0) then
        do isp=1,snspace_points_tot
*     write(sluno,*)' ** space point',isp
          x_pos = sspace_points(isp,1)
          y_pos = sspace_points(isp,2)
          x_drifttime_corr = sdc_x_central_time + y_pos/sdc_wire_velocity
          y_drifttime_corr = sdc_y_central_time + x_pos/sdc_wire_velocity
*     write(sluno,*)x_pos,x_drifttime_corr,y_pos,y_drifttime_corr
          do ihit=1,sspace_point_hits(isp,1)
            hit = sspace_point_hits(isp,ihit+2)
            plane = SDC_PLANE_NUM(hit)
            wire = SDC_WIRE_NUM(hit)
            if(plane.eq.2 .or. plane.eq.5 .or. 
     &           plane.eq.8 .or. plane.eq.11 .or.  ! Y or Y'  plane
     &            plane.eq.14 .or. plane.eq.17) then
              SDC_DRIFT_TIME(hit)=SDC_DRIFT_TIME(hit) - y_drifttime_corr
            else                        ! X,U,V,X' plane
              SDC_DRIFT_TIME(hit)=SDC_DRIFT_TIME(hit) - x_drifttime_corr
            endif
            SDC_DRIFT_DIS(hit) = s_drift_dist_calc
     &           (plane,wire,SDC_DRIFT_TIME(hit))
*     write(sluno,*)ihit,hit,SDC_DRIFT_TIME(hit),SDC_DRIFT_DIS(hit)

* djm 8/25/94
* Stuff drift time and distance into registered variables for histogramming and tests.
* In the case of two separated hits per plane, the last one will be histogrammed.

            sdc_sing_drifttime(plane) = SDC_DRIFT_TIME(hit)
            sdc_sing_driftdis(plane) = SDC_DRIFT_DIS(hit)
* djm 9/16/94
* add some wire positions for each plane 
            sdc_sing_wcenter(plane) = SDC_WIRE_CENTER(hit)

          enddo
        enddo
      endif
*     
*     Histogram SDC_DECODED_DC
      call s_fill_dc_dec_hist(ABORT,err)


*     write out results if debugflagpr is set
      if(sdebugflagpr.ne.0) then
        call s_print_pr
      endif
*
      return
      end
