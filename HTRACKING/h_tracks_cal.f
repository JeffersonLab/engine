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
* $Log$
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
      integer*4 hit,blk
      real*4 xblk

      integer*4 nt      !Track number
      integer*4 nc      !Cluster number
      real*4 xf         !X position of track on calorimeter front surface
      real*4 xb         !X position of track on calorimeter back  surface
      real*4 yf         !Y position of track on calorimeter front surface
      real*4 yb         !Y position of track on calorimeter back  surface
      real*4 dz_f       !Distance along Z axis between focal plane and
      real*4 dz_b       !calorimeter front(f) and back(b) surfaces
      real*4 delta_x    !Distance between track & cluster in X projection
*
      include 'hms_data_structures.cmn'
      include 'hms_calorimeter.cmn'
      include 'hms_tracking.cmn'
*
*
      hntracks_cal=0
      if(hntracks_fp.le.0) go to 100   !Return
*
*-----Compute impact point coordinates on the 
*-----calorimeter front and back surfaces
      do nt=1,hntracks_fp
        dz_f=hcal_zmin-hz_fp(nt)
        dz_b=hcal_zmax-hz_fp(nt)
*
        xf=hx_fp(nt)+hxp_fp(nt)*dz_f
        xb=hx_fp(nt)+hxp_fp(nt)*dz_b
*
        yf=hy_fp(nt)+hyp_fp(nt)*dz_f
        yb=hy_fp(nt)+hyp_fp(nt)*dz_b
*
        hcluster_track(nt)   =-1        !Track is outside the fiducial volume
        htrack_xc(nt)        = xf
        htrack_yc(nt)        = yf
*
*-------Is the track inside the fiducial volume?
        if(xf.le.hcal_fv_xmax  .and.  xf.ge.hcal_fv_xmin  .and.
     &       xb.le.hcal_fv_xmax  .and.  xb.ge.hcal_fv_xmin  .and.
     &       yf.le.hcal_fv_ymax  .and.  yf.ge.hcal_fv_ymin  .and.
     &       yb.le.hcal_fv_ymax  .and.  yb.ge.hcal_fv_ymin) then
*
          hcluster_track(nt)=0          !Track is inside the fiducial volume
          if(hnclusters_cal.gt.0) then
*
*------------Search for a cluster matching this track
            do nc=1,hnclusters_cal
              delta_x=abs(xf-hcluster_xc(nc))
              if(delta_x.le.(0.5*hcal_block_xsize + hcal_slop)) then
                hcluster_track(nt)=nc   !Track matches cluster #nc
                hntracks_cal      =hntracks_cal+1
              endif                     !End ... if matched
            enddo                       !End loop over clusters
          endif                         !End ... if inside fiducial volume
        endif                           !End ... if number of clusters > 0
      enddo                             !End loop over detector tracks
*

* (SAW Dec 10, 1998) Why do we use data events rather than ped events to 
* get the pedestals.  Anyway, The following does nothing with the tubes on
* the negative ends.
      if (hdebugcalcpeds.ne.0) then
        if(hntracks_fp.eq.1) then       !use blocks not on track to find pedestal
          do hit=1,hcal_tot_hits
            blk=hcal_row(hit)+hmax_cal_rows*(hcal_column(hit)-1)
            xblk=hcal_block_xc(blk)
            if (abs(xf-xblk).ge.20. .and. abs(xb-xblk).ge.20.) then !blk not hit
              if (hcal_zero_num(blk).le.2000) then !avoid overflow in sum**2
                hcal_zero_sum(blk)=hcal_zero_sum(blk)+hcal_adc_pos(hit)
                hcal_zero_sum2(blk)=hcal_zero_sum2(blk)
     $               +hcal_adc_pos(hit)*hcal_adc_pos(hit)
                hcal_zero_num(blk)=hcal_zero_num(blk)+1
              endif
            endif
          enddo
        endif
      endif

  100 continue
      if(hdbg_tracks_cal.gt.0) call h_prt_cal_tracks
*
      return
      end
