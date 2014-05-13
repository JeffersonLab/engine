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
*-      Modified 9 Apr 1998       Added a switch to turn on the fiducial
*-                                cut.  The default for this is now no cut.
*-                                K.G. Vansyoc
* $Log: s_tracks_cal.f,v $
* Revision 1.9  2003/04/03 00:45:01  jones
* Update to calorimeter calibration (V. Tadevosyan)
*
* Revision 1.8  1999/02/23 19:01:45  csa
* (JRA) Clean up logical structure, remove sdebugcalcpeds stuff
*
* Revision 1.7  1999/01/29 17:34:59  saw
* Add variables for second tubes on shower counter
*
* Revision 1.6  1997/02/13 14:13:29  saw
* (JRA) Correct error in position of top edge of fiducial cut.
*
* Revision 1.5  1996/01/17 18:54:41  cdaq
* (JRA) Add sdebugcalcpeds flag
*
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

      integer*4 t_nt, t_nc
      real*4 t_minx, temp_x

      include 'sos_data_structures.cmn'
      include 'sos_calorimeter.cmn'
      include 'sos_tracking.cmn'

      sntracks_cal=0
      if(sntracks_fp.le.0) go to 100   !Return
*
* Compute impact point coordinates on the calorimeter front and back surfaces
*
      do nt=1,sntracks_fp
        dz_f=scal_zmin-sz_fp(nt)
        dz_b=scal_zmax-sz_fp(nt)

        xf=sx_fp(nt)+sxp_fp(nt)*dz_f
        xb=sx_fp(nt)+sxp_fp(nt)*dz_b

        yf=sy_fp(nt)+syp_fp(nt)*dz_f
        yb=sy_fp(nt)+syp_fp(nt)*dz_b

        strack_xc(nt)        = xf
        strack_yc(nt)        = yf

! TH - Note these limits are calculated in s_init_cal.f:
!    scal_fv_xmin = scal_xmin+5
!    scal_fv_xmax = scal_xmax-5
!    
!    scal_xmin = scal_4ta_top(1) = -55.0-5.64 (this is from PARAM/scal.pos)
!    scal_xmax = scal_4ta_top(scal_4ta_nr) + scal_block_size 
!  where scal_4ta_nr is the number of shower counter blocks per layer 
!  (nr=11). Note that X,Y position is the same for each block.
!
!    scal_block_size = scal_4ta_top(2) - scal_4ta_top(1)

! TH Temporarily hardcode elastic region for calorimeter as fiducial region
!    in which track is allowed: XCAL=[-33,30], YCAL=[-18,18]
!
!        scal_fv_xmax=30
!        scal_fv_xmin=-33
!        scal_fv_ymax=18
!        scal_fv_ymin=-18


        track_in_fv = (xf.le.scal_fv_xmax  .and.  xf.ge.scal_fv_xmin  .and.
     &                 xb.le.scal_fv_xmax  .and.  xb.ge.scal_fv_xmin  .and.
     &                 yf.le.scal_fv_ymax  .and.  yf.ge.scal_fv_ymin  .and.
     &                 yb.le.scal_fv_ymax  .and.  yb.ge.scal_fv_ymin)


! TH - Note that scal_fv_test is set in PARAM/scal.param

* Initialize scluster_track(nt)
        if(scal_fv_test.eq.0) then         !not using fv test
          scluster_track(nt)=-1
        else                               !using fv test
          if (track_in_fv) then
            scluster_track(nt)=0   !Track is inside the fiducial volume
          else
            scluster_track(nt)=-1  !Track is outside the fiducial volume
          endif
        endif

*
*----------If inside fv (or no test), search for a cluster matching this track
*
        if((scal_fv_test.ne.0.and.track_in_fv) .or. scal_fv_test.eq.0) then

          if(snclusters_cal.gt.0) then
             t_minx = 99999
             t_nt = 1
             t_nc = 1
            do nc=1,snclusters_cal
              delta_x=abs(xf-scluster_xc(nc))
              if(delta_x.le.(0.5*scal_block_xsize + scal_slop)) then

!! TH - Check the deviation distance for each track for each cluster. If 
!!      distance smaller assign to t_minx. Eventually want to associate 
!!      the track with the smallest deviation to the cluster. Increment 
!!      tracks for calorimeter though whenever condition above is passed

                 temp_x = delta_x
                 if(temp_x.lt.t_minx) then
                    t_minx = temp_x
                    t_nt = nt
                    t_nc = nc
                 endif
                 sntracks_cal      =sntracks_cal+1
              endif                     !End ... if matched
            enddo                       !End loop over clusters

            scluster_track(t_nt)=t_nc   !Track matches cluster #nc with min deviation
          endif                         !End ... if number of clusters > 0
        endif                           !End ... if inside fiducial volume
      enddo                             !End loop over detector tracks

  100 continue
      if(sdbg_tracks_cal.gt.0) call s_prt_cal_tracks

c     Collect data for SOS calorimeter calibration.
      if(sdbg_tracks_cal.lt.0) call s_cal_calib(0)

      return
      end
