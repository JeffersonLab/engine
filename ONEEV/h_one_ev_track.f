      subroutine h_one_ev_track
*
* $Log: h_one_ev_track.f,v $
* Revision 1.1  1996/01/17 16:39:41  cdaq
* Initial revision
*

      implicit none

      include 'hms_data_structures.cmn'
      include 'hms_tracking.cmn'
      include 'hms_geometry.cmn'
      include 'hms_calorimeter.cmn'
      include 'gen_event_info.cmn'
      include 'hms_one_ev.par'
      include 'gen_one_ev_gctrak.cmn'
      include 'gen_one_ev_gckine.cmn'
      include 'gen_one_ev_gcvolu.cmn'

      real track_x, track_y, track_z    ! a point on the track
      real track_x_slope, track_y_slope ! slope of the track
      real x, y, z                      ! coordinates
      real z_distance                   ! z distance to end of hut
      integer track                     ! index variables
*
* Take care of creating the reconstructed tracks
*
      vect(4) =  0.0
      vect(5) =  0.0
      vect(6) =  1.
      vect(7) =  1.
*      ipart = 3            !electron to make track red
*      ipart = 13           !neutron to make track black
*      ipart = 5            !muon to make track green
      ipart = 1            !photon to make track blue 
      tofg  = 1e-5
      itra  = 1
*      amass = 0.511e-3
*      amass = 0.93957
      sleng = 200.
      step  = 200.

      do track = 1, HNTRACKS_FP
        ipart = 1                       !photon to make track blue
        if (track.eq.HSNUM_FPTRACK) then
          ipart = 3                     !electron to make track red
        endif
        track_x       = -hx_fp(track)   ! x position on track
        track_y       = hy_fp(track)    ! y position on track
        track_z       = 0               ! z position on track
        track_x_slope = -hxp_fp(track)  ! track slope in x
        track_y_slope = hyp_fp(track)   ! track slope in y
        
        z		= -HHUT_HEIGHT / 2. ! bottom of hut
        z_distance	= track_z - z   ! distance from point to floor
        x		= track_x - z_distance * sin(track_x_slope) ! x loci
        y		= track_y - z_distance * sin(track_y_slope) ! y loci
        vect(1) = x
        vect(2) = y
        vect(3) = z
        call gsxyz
        z		= HHUT_HEIGHT / 2. ! bottom of hut
        z_distance	= z - track_z   ! distance from point to roof
        x		= track_x + z_distance * sin(track_x_slope) ! x loci
        y		= track_y + z_distance * sin(track_y_slope) ! y loci
        vect(1) = x
        vect(2) = y
        vect(3) = z
        call gsxyz
        call gdxyz (track)
*     call gdpart (itra,01,0.5)  !this will number the tracks
        itra = itra+1
*     ipart = ipart+1   !this changes the color for each track
      enddo
      end
