*=======================================================================
      subroutine s_tracks_cal(abort,errmsg)
*=======================================================================
*-
*-      Purpose: Associates clusters with detector tracks which are inside
*-               the calorimeter fiducial volume. A track and a cluster
*-               are considered as matched if the distance in X projection
*-               between these two is less than half the block width.
*-
*-      Input Banks: SOS_CLUSTERS_CAL, SOS_FOCAL_PLANE,SOS_GEOMETRY_CAL
*-
*-      Output Bank: SOS_TRACK_CAL
*-
*-      Created: 15 Mar 1994      Tsolak A. Amatuni
*-      Modified 25 Mar 1994      DFG
*-                                Change name of print routine
* $Log$
* Revision 1.4  1995/08/31 20:45:28  cdaq
* (JRA) Use off-track blocks to accumulate pedestal data
*
* Revision 1.3  1995/05/22  19:46:01  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.2  1994/11/23  14:24:46  cdaq
* * (SPB) Recopied from hms file and modified names for SOS
*
* Revision 1.1  1994/04/13  16:16:04  cdaq
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
      parameter (here='S_TRACKS_CAL')
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
      include 'sos_data_structures.cmn'
      include 'sos_calorimeter.cmn'
*
*
      sntracks_cal=0
      if(sntracks_fp.le.0) go to 100   !Return
*
*-----Compute impact point coordinates on the 
*-----calorimeter front and back surfaces
      do nt=1,sntracks_fp
        dz_f=scal_zmin-sz_fp(nt)
        dz_b=scal_zmax-sz_fp(nt)
*
        xf=sx_fp(nt)+sxp_fp(nt)*dz_f
        xb=sx_fp(nt)+sxp_fp(nt)*dz_b
*
        yf=sy_fp(nt)+syp_fp(nt)*dz_f
        yb=sy_fp(nt)+syp_fp(nt)*dz_b
*
        scluster_track(nt)   =-1        !Track is outside the fiducial volume
        strack_xc(nt)        = xf
        strack_yc(nt)        = yf
*
*--------Is the track inside the fiducial volume?
        if(xf.le.scal_fv_xmax  .and.  xf.ge.scal_fv_xmin  .and.
     &       xb.le.scal_fv_xmax  .and.  xf.ge.scal_fv_xmin  .and.
     &       yf.le.scal_fv_ymax  .and.  yf.ge.scal_fv_ymin  .and.
     &       yb.le.scal_fv_ymax  .and.  yb.ge.scal_fv_ymin) then
*
          scluster_track(nt)=0          !Track is inside the fiducial volume
          if(snclusters_cal.gt.0) then
*
*--------------Search for a cluster matching this track
            do nc=1,snclusters_cal
              delta_x=abs(xf-scluster_xc(nc))
              if(delta_x.le.(0.5*scal_block_xsize + scal_slop)) then
                scluster_track(nt)=nc   !Track matches cluster #nc
                sntracks_cal      =sntracks_cal+1
              endif                     !End ... if matched
            enddo                       !End loop over clusters
          endif                         !End ... if inside fiducial volume
        endif                           !End ... if number of clusters > 0
      enddo                             !End loop over detector tracks
*

      if(sntracks_fp.eq.1) then   !use blocks not on track to find pedestal
        do hit=1,scal_tot_hits
          blk=scal_row(hit)+smax_cal_rows*(scal_column(hit)-1)
          xblk=scal_block_xc(blk)
          if (abs(xf-xblk).ge.20. .and. abs(xb-xblk).ge.20.) then !blk not hit
           if (scal_zero_num(blk).le.2000) then !avoid overflow in sum**2
              scal_zero_sum(blk)=scal_zero_sum(blk)+scal_adc(hit)
              scal_zero_sum2(blk)=scal_zero_sum2(blk)+scal_adc(hit)*scal_adc(hit)
              scal_zero_num(blk)=scal_zero_num(blk)+1
            endif
          endif
        enddo
      endif

  100 continue
      if(sdbg_tracks_cal.gt.0) call s_prt_cal_tracks
*
      return
      end
