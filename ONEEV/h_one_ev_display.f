	subroutine h_one_ev_display
*
* This routine will store digitized hits for use in the one event display for
* Hall C
*
* August, 1994, Pat Welch, Oregon State University, tpw@physics.orst.edu
*
* $Log$
* Revision 1.1  1995/03/14 21:26:49  cdaq
* Initial revision
*

      implicit none

      include 'gen_data_structures.cmn'
      include 'hms_tracking.cmn'
      include 'hms_geometry.cmn'
      include 'hms_calorimeter.cmn'
      include 'gen_event_info.cmn'
      include 'hms_one_ev.par'
      include 'gctrak.inc'
      include 'gckine.inc'
      include 'gcvolu.inc'

      integer ihit                      ! hit number
      character*4 lnames(0:10)		! volume names
      integer lnums(0:10)               ! volume numbers or copies
      real xd(3), xm(3)			! coordinates
      real track_x, track_y, track_z    ! a point on the track
      real track_x_slope, track_y_slope ! slope of the track
      integer error_code                ! error return code
      real x, y, z                      ! coordinates
      real z_distance                   ! z distance to end of hut
      integer track,chamhit,scinhit,showhit ! index variables
      integer wirenum                   ! indicates GEANT wirenumber

*
* Clear any previous drawing
*
      call iclrwk (0, 0)
      call gtrigi
* Open the drawing and draw the detector stack
      call gdopen(3)
*
* Take care of creating the reconstructed tracks
*
      vect(4) =  0.0
      vect(5) =  0.0
      vect(6) =  1.
      vect(7) =  1.
      ipart = 3
      tofg  = 1e-5
      itra  = 1
      amass = 0.511e-3
      sleng = 200.
      step  = 200.

      if (HNTRACKS_FP .GT. 0) then
        do track = 1, HNTRACKS_FP
          track_x       = -hx_fp(track)	! x position on track
          track_y       = hy_fp(track)	! y position on track
          track_z       = 0             ! z position on track
          track_x_slope = -hxp_fp(track) ! track slope in x
          track_y_slope = hyp_fp(track)	! track slope in y

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
        enddo
      endif
*
* Now take care of defining the hits so they can be displayed
*
      xd(1) = 0.			! find the center of the detector
      xd(2) = 0.			! find the center of the detector
      xd(3) = 0.			! find the center of the detector
* Start with the wire chambers
      if (HDC_TOT_HITS .GT. 0) then
        lnames(0) = 'HHUT'
        lnums(0)  = 1
        do chamhit = 1, HDC_TOT_HITS
          if (HDC_PLANE_NUM(chamhit) .EQ. 1) then
            nlevel = 0			! initial value for # of levels
            lnames(1) = 'WCHA'		! level one
            lnums(1)  = 1               ! copy one, higher chamber
            lnames(2) = 'WCXP'		! X plane
            lnums(2)  = 1               ! copy one
            lnames(3) = 'WCWX'		! X wires
            wirenum = nint(hdc_nrwire(HDC_PLANE_NUM(chamhit))) + 1
     &           - HDC_WIRE_NUM(chamhit)
            if (HDC_WIRE_COUNTING(HDC_PLANE_NUM(chamhit)) .EQ. 1)
     &           wirenum = HDC_WIRE_NUM(chamhit)
            lnums(3)  = wirenum		! wire number
            call glvolu (4, lnames, lnums, error_code)
            call gdtom (xd, xm, 1)      ! transform from detector to MARS
            call gsahit (1, 4, 1, lnums(1), xm, ihit) ! store the hit
          elseif (HDC_PLANE_NUM(chamhit) .EQ. 2) then
            nlevel = 0			! initial value for # of levels
            lnames(1) = 'WCHA'		! level one
            lnums(1)  = 1               ! copy one, higher chamber
            lnames(2) = 'WCYP'		! Y plane
            lnums(2)  = 1               ! copy one
            lnames(3) = 'WCWY'		! Y wires
            wirenum = HDC_WIRE_NUM(chamhit)
            if (HDC_WIRE_COUNTING(HDC_PLANE_NUM(chamhit)) .EQ. 1)
     &           wirenum = nint(hdc_nrwire(HDC_PLANE_NUM(chamhit)))+1
     $           -wirenum
            lnums(3)  = wirenum		! wire number
            call glvolu (4, lnames, lnums, error_code)
            call gdtom (xd, xm, 1)      ! transform from detector to MARS
            call gsahit (1, 5, 1, lnums(1), xm, ihit) ! store the hit
          elseif (HDC_PLANE_NUM(chamhit) .EQ. 3) then
            nlevel = 0			! initial value for # of levels
            lnames(1) = 'WCHA'		! level one
            lnums(1)  = 1               ! copy one, higher chamber
            lnames(2) = 'WCUP'		! U plane
            lnums(2)  = 1               ! copy one
            lnames(3) = 'WCWU'		! U wires
            wirenum = nint(hdc_nrwire(HDC_PLANE_NUM(chamhit))) + 1
     &           - HDC_WIRE_NUM(chamhit)
            if (HDC_WIRE_COUNTING(HDC_PLANE_NUM(chamhit)) .EQ. 1)
     &           wirenum = HDC_WIRE_NUM(chamhit)
            lnums(3)  = wirenum		! wire number
            call glvolu (4, lnames, lnums, error_code)
            call gdtom (xd, xm, 1)      ! transform from detector to MARS
            call gsahit (1, 6, 1, lnums(1), xm, ihit) ! store the hit
          elseif (HDC_PLANE_NUM(chamhit) .EQ. 4) then
            nlevel = 0			! initial value for # of levels
            lnames(1) = 'WCHA'		! level one
            lnums(1)  = 1               ! copy one, higher chamber
            lnames(2) = 'WCVP'		! V plane
            lnums(2)  = 1               ! copy one
            lnames(3) = 'WCWV'		! V wires
            wirenum = nint(hdc_nrwire(HDC_PLANE_NUM(chamhit))) + 1
     &           - HDC_WIRE_NUM(chamhit)
            if (HDC_WIRE_COUNTING(HDC_PLANE_NUM(chamhit)) .EQ. 1)
     &           wirenum = HDC_WIRE_NUM(chamhit)
            lnums(3)  = wirenum		! wire number
            call glvolu (4, lnames, lnums, error_code)
            call gdtom (xd, xm, 1)      ! transform from detector to MARS
            call gsahit (1, 7, 1, lnums(1), xm, ihit) ! store the hit
          elseif (HDC_PLANE_NUM(chamhit) .EQ. 5) then
            nlevel = 0			! initial value for # of levels
            lnames(1) = 'WCHA'		! level one
            lnums(1)  = 1               ! copy one, higher chamber
            lnames(2) = 'WCYP'		! Y plane
            lnums(2)  = 2               ! copy one
            lnames(3) = 'WCWY'		! Y wires
            wirenum = HDC_WIRE_NUM(chamhit)
            if (HDC_WIRE_COUNTING(HDC_PLANE_NUM(chamhit)) .EQ. 1)
     &           wirenum = nint(hdc_nrwire(HDC_PLANE_NUM(chamhit)))+1
     $           -wirenum
            lnums(3)  = wirenum		! wire number
            call glvolu (4, lnames, lnums, error_code)
            call gdtom (xd, xm, 1)      ! transform from detector to MARS
            call gsahit (1, 5, 1, lnums(1), xm, ihit) ! store the hit
          elseif (HDC_PLANE_NUM(chamhit) .EQ. 6) then
            nlevel = 0			! initial value for # of levels
            lnames(1) = 'WCHA'		! level one
            lnums(1)  = 1               ! copy one, higher chamber
            lnames(2) = 'WCXP'		! X plane
            lnums(2)  = 2               ! copy one
            lnames(3) = 'WCWX'		! X wires
            wirenum = nint(hdc_nrwire(HDC_PLANE_NUM(chamhit))) + 1
     &           - HDC_WIRE_NUM(chamhit)
            if (HDC_WIRE_COUNTING(HDC_PLANE_NUM(chamhit)) .EQ. 1)
     &           wirenum = HDC_WIRE_NUM(chamhit)
            lnums(3)  = wirenum		! wire number
            call glvolu (4, lnames, lnums, error_code)
            call gdtom (xd, xm, 1)      ! transform from detector to MARS
            call gsahit (1, 4, 1, lnums(1), xm, ihit) ! store the hit
          elseif (HDC_PLANE_NUM(chamhit) .EQ. 7) then
            nlevel = 0			! initial value for # of levels
            lnames(1) = 'WCHA'		! level one
            lnums(1)  = 2               ! copy two, lower chamber
            lnames(2) = 'WCXP'		! X plane
            lnums(2)  = 1               ! copy one
            lnames(3) = 'WCWX'		! X wires
            wirenum = nint(hdc_nrwire(HDC_PLANE_NUM(chamhit))) + 1
     &           - HDC_WIRE_NUM(chamhit)
            if (HDC_WIRE_COUNTING(HDC_PLANE_NUM(chamhit)) .EQ. 1)
     &           wirenum = HDC_WIRE_NUM(chamhit)
            lnums(3)  = wirenum		! wire number
            call glvolu (4, lnames, lnums, error_code)
            call gdtom (xd, xm, 1)      ! transform from detector to MARS
            call gsahit (1, 4, 1, lnums(1), xm, ihit) ! store the hit
          elseif (HDC_PLANE_NUM(chamhit) .EQ. 8) then
            nlevel = 0			! initial value for # of levels
            lnames(1) = 'WCHA'		! level one
            lnums(1)  = 2               ! copy two, lower chamber
            lnames(2) = 'WCYP'		! Y plane
            lnums(2)  = 1               ! copy one
            lnames(3) = 'WCWY'		! Y wires
            wirenum = HDC_WIRE_NUM(chamhit)
            if (HDC_WIRE_COUNTING(HDC_PLANE_NUM(chamhit)) .EQ. 1)
     &           wirenum = nint(hdc_nrwire(HDC_PLANE_NUM(chamhit)))+1
     $           -wirenum
            lnums(3)  = wirenum		! wire number
            call glvolu (4, lnames, lnums, error_code)
            call gdtom (xd, xm, 1)      ! transform from detector to MARS
            call gsahit (1, 5, 1, lnums(1), xm, ihit) ! store the hit
          elseif (HDC_PLANE_NUM(chamhit) .EQ. 9) then
            nlevel = 0			! initial value for # of levels
            lnames(1) = 'WCHA'		! level one
            lnums(1)  = 2               ! copy two, lower chamber
            lnames(2) = 'WCUP'		! U plane
            lnums(2)  = 1               ! copy one
            lnames(3) = 'WCWU'		! U wires
            wirenum = nint(hdc_nrwire(HDC_PLANE_NUM(chamhit))) + 1
     &           - HDC_WIRE_NUM(chamhit)
            if (HDC_WIRE_COUNTING(HDC_PLANE_NUM(chamhit)) .EQ. 1)
     &           wirenum = HDC_WIRE_NUM(chamhit)
            lnums(3)  = wirenum		! wire number
            call glvolu (4, lnames, lnums, error_code)
            call gdtom (xd, xm, 1)      ! transform from detector to MARS
            call gsahit (1, 6, 1, lnums(1), xm, ihit) ! store the hit
          elseif (HDC_PLANE_NUM(chamhit) .EQ. 10) then
            nlevel = 0			! initial value for # of levels
            lnames(1) = 'WCHA'		! level one
            lnums(1)  = 2               ! copy two, lower chamber
            lnames(2) = 'WCVP'		! V plane
            lnums(2)  = 1               ! copy one
            lnames(3) = 'WCWV'		! V wires
            wirenum = nint(hdc_nrwire(HDC_PLANE_NUM(chamhit))) + 1
     &           - HDC_WIRE_NUM(chamhit)
            if (HDC_WIRE_COUNTING(HDC_PLANE_NUM(chamhit)) .EQ. 1)
     &           wirenum = HDC_WIRE_NUM(chamhit)
            lnums(3)  = wirenum		! wire number
            call glvolu (4, lnames, lnums, error_code)
            call gdtom (xd, xm, 1)      ! transform from detector to MARS
            call gsahit (1, 7, 1, lnums(1), xm, ihit) ! store the hit
          elseif (HDC_PLANE_NUM(chamhit) .EQ. 11) then
            nlevel = 0			! initial value for # of levels
            lnames(1) = 'WCHA'		! level one
            lnums(1)  = 2               ! copy two, lower chamber
            lnames(2) = 'WCYP'		! Y plane
            lnums(2)  = 2               ! copy one
            lnames(3) = 'WCWY'		! Y wires
            wirenum = HDC_WIRE_NUM(chamhit)
            if (HDC_WIRE_COUNTING(HDC_PLANE_NUM(chamhit)) .EQ. 1)
     &           wirenum = nint(hdc_nrwire(HDC_PLANE_NUM(chamhit)))+1
     $           -wirenum
            lnums(3)  = wirenum		! wire number
            call glvolu (4, lnames, lnums, error_code)
            call gdtom (xd, xm, 1)      ! transform from detector to MARS
            call gsahit (1, 5, 1, lnums(1), xm, ihit) ! store the hit
          elseif (HDC_PLANE_NUM(chamhit) .EQ. 12) then
            nlevel = 0			! initial value for # of levels
            lnames(1) = 'WCHA'		! level one
            lnums(1)  = 2               ! copy two, lower chamber
            lnames(2) = 'WCXP'		! X plane
            lnums(2)  = 2               ! copy one
            lnames(3) = 'WCWX'		! X wires
            wirenum = nint(hdc_nrwire(HDC_PLANE_NUM(chamhit))) + 1
     &           - HDC_WIRE_NUM(chamhit)
            if (HDC_WIRE_COUNTING(HDC_PLANE_NUM(chamhit)) .EQ. 1)
     &           wirenum = HDC_WIRE_NUM(chamhit)
            lnums(3)  = wirenum		! wire number
            call glvolu (4, lnames, lnums, error_code)
            call gdtom (xd, xm, 1)      ! transform from detector to MARS
            call gsahit (1, 4, 1, lnums(1), xm, ihit) ! store the hit
          endif
        enddo
      endif

* Take a look at the hodoscopes
      if (HSCIN_TOT_HITS .GT. 0) then
        lnames(0) = 'HHUT'              ! relative to the hut
        lnums(0)  = 1                   ! copy 1
        do scinhit = 1, HSCIN_TOT_HITS
*
* First the lower X
*
          if (HSCIN_PLANE_NUM(scinhit) .EQ. 1) then
            nlevel = 0			! initial value for # of levels
            lnames(1) = 'HODO'		! level one
            lnums(1)  = 1               ! copy one, lower hodo
            lnames(2) = 'HODX'		! X strips
            lnums(2)  = 1               ! copy one
            lnames(3) = 'HOXS'		! X strips
            lnums(3)  = HSCIN_COUNTER_NUM(scinhit) ! X strip number
            call glvolu (4, lnames, lnums, error_code)
            call gdtom (xd, xm, 1)      ! transform from detector to MARS
            call gsahit (1, 2, 1, lnums(1), xm, ihit) ! store the hit
*     
* now the upper X
*
          elseif (HSCIN_PLANE_NUM(scinhit) .EQ. 3) then
            nlevel = 0			! initial value for # of levels
            lnames(1) = 'HODO'		! level one
            lnums(1)  = 2               ! copy two, upper hodo
            lnames(2) = 'HODX'		! X strips
            lnums(2)  = 1               ! copy one
            lnames(3) = 'HOXS'		! X strips
            lnums(3)  = HSCIN_COUNTER_NUM(scinhit) ! X strip number
            call glvolu (4, lnames, lnums, error_code)
            call gdtom (xd, xm, 1)      ! transform from detector 
            call gsahit (1, 2, 1, lnums(1), xm, ihit) ! store the hit
*
* now the lower Y
*
          elseif (HSCIN_PLANE_NUM(scinhit) .EQ. 2) then
            nlevel = 0			! initial value for # of levels
            lnums(1)  = 1               ! copy one, lower hodo
            lnames(2) = 'HODY'		! Y strips
            lnums(2)  = 1               ! copy one
            lnames(3) = 'HOYS'		! Y strips
            lnums(3)  = HSCIN_COUNTER_NUM(scinhit) ! Y strip number
            call glvolu (4, lnames, lnums, error_code)
            call gdtom (xd, xm, 1)      ! transform from detector to MARS
            call gsahit (1, 3, 1, lnums(1), xm, ihit) ! store the hit
*     
* now the upper Y
*
          elseif (HSCIN_PLANE_NUM(scinhit) .EQ. 4) then
            nlevel = 0                  ! initial value for # of levels
            lnums(1)  = 2               ! copy two, upper hodo
            lnames(2) = 'HODY'          ! Y strips
            lnums(2)  = 1               ! copy one
            lnames(3) = 'HOYS'          ! Y strips
            lnums(3)  = HSCIN_COUNTER_NUM(scinhit) ! Y strip number
            call glvolu (4, lnames, lnums, error_code)
            call gdtom (xd, xm, 1)      ! transform from detector to MARS
            call gsahit (1, 3, 1, lnums(1), xm, ihit) ! store the hit
          endif
        enddo
      endif
*     
* Now take care of the shower detector
*
      if (HCAL_NUM_HITS .GE. 0) then
        lnames(1) = 'SHOW'              ! shower detector
        lnums(1)  = 1                   ! copy 1
        lnames(2) = 'LAYE'              ! x subdivisions
        lnames(3) = 'BLOC'              ! z subdivisions
        do showhit = 1, HCAL_NUM_HITS
          nlevel = 0
          lnums(2)  = HCAL_COLS(showhit) ! x subdivision number
          lnums(3)  = HCAL_ROWS(showhit) ! which block
          call glvolu(4, lnames, lnums, error_code)
          call gdtom (xd, xm, 1)        ! transform from det to MARS
          call gsahit (1, 1, 1, lnums(2), xm, ihit) ! store the hit
        enddo
      endif
*
* A decent 3D view in reserve, this one takes up the whole screen,
* so swap it with the other two,projected ones if you want it.
*     call gdraw ('HHUT', 45., 115., 90., 5.0, 5.0, 0.06, 0.06)
*	call gdxyz (0)                          ! draw the tracks
*	call gdhits ('*   ', '*   ', 0, 850, 0.5)
*
* Side view
*
      call gdraw ('HHUT', 90., 90., 90., 14.75, 2.9, 0.045, 0.08)
* change to next line if you only want to see the chambers
*     call gdraw ('HHUT', 90., 90., 90., 13.75, 9.9, 0.140, 0.09)
      call gdxyz (0)                    ! draw the tracks
      call gdcol (4)
      call gdhits ('*   ', '*   ', 0, 850, 0.1)
      call gdcol (0)
*
* Other side view
*
      call gdraw ('HHUT', 90., 0., 270., 4.4, 2.9, 0.045, 0.08)
* change to next line if you only want to see the chambers
*     call gdraw ('HHUT', 90., 0., 270., 3.4, 9.9, 0.140, 0.09)
      call gdxyz (0)                    ! draw the tracks
      call gdcol (4)
      call gdhits ('*   ', '*   ', 0, 850, 0.1)
      call gdcol (0)

* close the picture and show it
      call gdclos(3)
      call gdshow(3)
* clear the viewbank
      call gdelet(3)
* and clear the hit banks
      call gtrigc

      end

