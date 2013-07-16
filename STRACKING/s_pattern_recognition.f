      subroutine s_pattern_recognition(ABORT,err)
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
* $Log: s_pattern_recognition.f,v $
* Revision 1.10  1996/09/05 20:09:36  saw
* (JRA) Cosmetic
*
* Revision 1.9  1996/04/30 17:34:56  saw
* (JRA) Histogram the card id.
*
* Revision 1.8  1996/01/17 19:01:21  cdaq
* (JRA) Add code for easy space points
*
* Revision 1.7  1995/10/10 16:13:47  cdaq
* (JRA) Remove sdc_sing_wcenter, cosmetics.
*
* Revision 1.6  1995/07/20 18:58:50  cdaq
* (SAW) Declare sind and cosd for f2c compatibility
*
* Revision 1.5  1995/05/22  19:45:43  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.4  1995/04/06  19:36:53  cdaq
* (SAW) Hopefully improve wire velocity correction for SOS chambers
*
* Revision 1.3  1994/12/06  15:33:06  cdaq
* (SAW) First pass at wire velocity correction for Brookhaven chambers
*
* Revision 1.2  1994/11/22  21:48:45  cdaq
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
      character*21 here
      parameter (here= 's_pattern_recognition')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 'sos_data_structures.cmn'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      INCLUDE 'sos_tracking.cmn'
      INCLUDE 'sos_geometry.cmn'
*
*     local variables
      integer*4 hit_number(smax_chamber_hits)
      integer*4 space_point_hits(smax_space_points,smax_hits_per_point+2)
      integer*4 pln, isp, ihit, hit
      integer*4 i,j,k,xx,xxprime
      integer*4 xplane,xprimeplane
      integer*4 ich, ip
      logical easy_space_point
*
      real*4 space_points(smax_space_points,2)
      real*4 xdist,ydist
      real*4 time_corr
      real*4 s_drift_dist_calc
      external s_drift_dist_calc

*
*     temporary initialization
      ABORT= .FALSE.
      err=' '
*
*   
      ihit = 0
      snspace_points_tot = 0
      do ich=1,sdc_num_chambers
        easy_space_point = .false.
        snspace_points(ich)=0        
        sncham_hits(ich)=0
*
*     For this loop to work, sdc_planes_per_chamber must be
*     the number of planes per chamber.  (And all chambers must have the
*     same number of planes.)
*
        do ip=(ich-1)*sdc_planes_per_chamber+1,ich*sdc_planes_per_chamber
          sncham_hits(ich)=sncham_hits(ich)+sdc_hits_per_plane(ip)
        enddo
        xplane=3+(ich-1)*sdc_planes_per_chamber
        xprimeplane=4+(ich-1)*sdc_planes_per_chamber
        if(sncham_hits(ich).ge.smin_hit(ich) .and.
     $     sncham_hits(ich).lt.smax_pr_hits(ich))  then
          do i=ihit+1,ihit+sncham_hits(ich)
            hit_number(i)=i
            if(sdc_plane_num(i).eq.xplane) xx=i
            if(sdc_plane_num(i).eq.xprimeplane) xxprime=i
          enddo
          if((sdc_hits_per_plane(xplane).eq.1) .and.
     &       (sdc_hits_per_plane(xprimeplane).eq.1).and.
     &       ((sdc_wire_center(xx)-sdc_wire_center(xxprime))**2.lt.
     &       (sspace_point_criterion(ich))) .and.
     &       (sncham_hits(ich).le.6)) then
            call s_find_easy_space_point(sncham_hits(ich),hit_number(ihit+1),
     &        sdc_wire_center(ihit+1),sdc_plane_num(ihit+1),
     &        sspace_point_criterion(ich),smax_space_points,xx-ihit,
     &        xxprime-ihit,easy_space_point,snspace_points(ich),
     &        space_points,space_point_hits)
            if (.not.easy_space_point) call find_space_points(sncham_hits(ich),
     &         hit_number(ihit+1),sdc_wire_center(ihit+1),
     &         sdc_plane_num(ihit+1),sspace_point_criterion(ich),
     &         sxsp(1),sysp(1),smax_space_points,
     &         snspace_points(ich), space_points, space_point_hits)
          else
          call find_space_points(sncham_hits(ich),hit_number(ihit+1),
     &         sdc_wire_center(ihit+1),
     &         sdc_plane_num(ihit+1),sspace_point_criterion(ich),
     &         sxsp(1),sysp(1),smax_space_points,
     &         snspace_points(ich), space_points, space_point_hits)
          endif
*    
          if (snspace_points(ich).gt.0) then
*    If two hits in same plane, choose one with minimum drift time
          call s_choose_single_hit(ABORT,err,snspace_points(ich),
     &         space_point_hits)
* Select on minimum number of combinations and hits
            call select_space_points(smax_space_points,snspace_points(ich),
     &         space_points,space_point_hits,smin_hit(ich),smin_combos(ich),
     $         easy_space_point)
          endif


          do i=1,snspace_points(ich)
            k=snspace_points_tot+i
            sspace_points(k,1)=space_points(i,1)
            sspace_points(k,2)=space_points(i,2)
            sspace_point_hits(k,1)=space_point_hits(i,1)
            sspace_point_hits(k,2)=space_point_hits(i,2)
            do j=1,space_point_hits(i,1)
              sspace_point_hits(k,j+2)=space_point_hits(i,j+2)
            enddo
          enddo
        endif
        snspace_points_tot = snspace_points_tot+ snspace_points(ich)
        ihit = ihit + sncham_hits(ich)
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
      if(snspace_points_tot.gt.0) then
        do isp=1,snspace_points_tot
          xdist = sspace_points(isp,1)
          ydist = sspace_points(isp,2)
            do ihit=1,sspace_point_hits(isp,1)
              hit = sspace_point_hits(isp,ihit+2)
              pln = sdc_plane_num(hit)
            if (sdc_readout_x(pln)) then    !readout from side
              time_corr = ydist*sdc_readout_corr(pln)/sdc_wire_velocity
            else                           !readout from top/bottom
              time_corr = xdist*sdc_readout_corr(pln)/sdc_wire_velocity
            endif

            sdc_drift_time(hit)=sdc_drift_time(hit) - sdc_central_time(pln)
     &            + sdc_drifttime_sign(pln)*time_corr
            sdc_drift_dis(hit) = s_drift_dist_calc
     &            (pln,sdc_wire_num(hit),sdc_drift_time(hit))
*
* djm 8/25/94
* Stuff drift time and distance into registered variables for histogramming and tests.
* In the case of two separated hits per plane, the last one will be histogrammed.
              sdc_sing_drifttime(pln) = sdc_drift_time(hit)
              sdc_sing_driftdis(pln) = sdc_drift_dis(hit)
              sdc_sing_cardid(pln) =
     &             sdc_card_no(sdc_wire_num(hit),sdc_plane_num(hit))
            enddo
          enddo
        endif
*     
*     Histogram sdc_DECODED_DC
      call s_fill_dc_dec_hist(ABORT,err)

*     write out results if debugflagpr is set
      if(sdebugflagpr.ne.0) then
        call s_print_pr
      endif
*
      return
      end
