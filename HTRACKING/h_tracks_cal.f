*=======================================================================
      subroutine h_tracks_cal(abort,errmsg)
*=======================================================================
*-
*-      Purpose: Associates clusters with detector tracks which are inside
*-               the calorimeter fiducial volume. A track and a cluster
*-               are considered as matched if the distance in X projection
*-               between these two is less than half the block width.
*-
*-      Input Banks: HMS_CLUSTERS_CAL, HMS_FOCAL_PLANE,HMS_GEOMETRY_CAL
*-
*-      Output Bank: HMS_TRACK_CAL
*-
*-      Created: 15 Mar 1994      Tsolak A. Amatuni
*-      Modified 25 Mar 1994      DFG
*-                                Change name of print routine
*-      Modified 9 Apr 1998       Added a switch to turn on the fiducial
*-                                cut.  The default for this is now no cut.
*-                                K.G. Vansyoc
* $Log$
* Revision 1.9  2003/03/21 22:21:51  jones
* Modified and rearrange routines to calibrate the HMS calorimeter (V. Tadevosyan)
*
* Revision 1.8  1999/02/23 18:52:07  csa
* (JRA) Clean up logical structure, remove hdebugcalcpeds stuff
*
* Revision 1.7  1998/12/17 22:02:40  saw
* Support extra set of tubes on HMS shower counter
*
* Revision 1.6  1997/02/13 14:12:36  saw
* (JRA) Correct error in position of top edge of fiducial cut.
*
* Revision 1.5  1996/01/16 22:00:40  cdaq
* (JRA) Add hdebugcalcpeds flag
*
* Revision 1.4  1995/08/30 17:34:24  cdaq
* (JRA) Use off-track blocks to accumulate pedestal data
*
* Revision 1.3  1995/05/22  19:39:31  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.2  1994/10/11  19:24:54  cdaq
* (SAW) Formatting changes
*
* Revision 1.1  1994/04/13  17:33:38  cdaq
* Initial revision
*
*-----------------------------------------------------------------------
*
*
      implicit none
      save
*
      logical abort
      character*(*) errmsg
      character*12 here
      parameter (here='H_TRACKS_CAL')
*
      integer*4 nt      !Track number
      integer*4 nc      !Cluster number
      real*4 xf         !X position of track on calorimeter front surface
      real*4 xb         !X position of track on calorimeter back  surface
      real*4 yf         !Y position of track on calorimeter front surface
      real*4 yb         !Y position of track on calorimeter back  surface
      real*4 dz_f       !Distance along Z axis between focal plane and
      real*4 dz_b       !calorimeter front(f) and back(b) surfaces
      real*4 delta_x    !Distance between track & cluster in X projection
      logical*4 track_in_fv

      include 'hms_data_structures.cmn'
      include 'hms_calorimeter.cmn'
      include 'hms_tracking.cmn'

      hntracks_cal=0
      if(hntracks_fp.le.0) go to 100   !Return
*
* Compute impact point coordinates on the calorimeter front and back surfaces
*
      do nt=1,hntracks_fp
        dz_f=hcal_zmin-hz_fp(nt)
        dz_b=hcal_zmax-hz_fp(nt)

        xf=hx_fp(nt)+hxp_fp(nt)*dz_f
        xb=hx_fp(nt)+hxp_fp(nt)*dz_b

        yf=hy_fp(nt)+hyp_fp(nt)*dz_f
        yb=hy_fp(nt)+hyp_fp(nt)*dz_b

        htrack_xc(nt)        = xf
        htrack_yc(nt)        = yf

	track_in_fv = (xf.le.hcal_fv_xmax  .and.  xf.ge.hcal_fv_xmin  .and.
     &                 xb.le.hcal_fv_xmax  .and.  xb.ge.hcal_fv_xmin  .and.
     &                 yf.le.hcal_fv_ymax  .and.  yf.ge.hcal_fv_ymin  .and.
     &                 yb.le.hcal_fv_ymax  .and.  yb.ge.hcal_fv_ymin)

* Initialize hcluster_track(nt)
        if(hcal_fv_test.eq.0) then         !not using fv test
          hcluster_track(nt)=-1
        else	                           !using fv test
	  if (track_in_fv) then
            hcluster_track(nt)=0   !Track is inside the fiducial volume
          else
            hcluster_track(nt)=-1  !Track is outside the fiducial volume
          endif
        endif
*
*----------If inside fv (or no test), Search for a cluster matching this track
*
        if( (hcal_fv_test.ne.0.and.track_in_fv) .or. hcal_fv_test.eq.0) then

          if(hnclusters_cal.gt.0) then
            do nc=1,hnclusters_cal
              delta_x=abs(xf-hcluster_xc(nc))
              if(delta_x.le.(0.5*hcal_block_xsize + hcal_slop)) then
                hcluster_track(nt)=nc   !Track matches cluster #nc
                hntracks_cal      =hntracks_cal+1
              endif                     !End ... if matched
            enddo                       !End loop over clusters
          endif                         !End ... if number of clusters > 0
        endif                           
      enddo                             !End loop over detector tracks

  100 continue

      if(hdbg_tracks_cal.gt.0) call h_prt_cal_tracks

c     Collect data for HMS calorimeter calibration.
      if(hdbg_tracks_cal.lt.0) call h_cal_calib(0)

      return
      end
