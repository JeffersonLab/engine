      subroutine s_one_ev_display
*
* This routine will store digitized hits for use in the one event display for
* Hall C
*
* August, 1994, Pat Welch, Oregon State University, tpw@physics.orst.edu
*
* Modified from HMS version, h_one_ev_display, on March 1995 by
* Derek van Westrum (vanwestr@cebaf.gov)
*
* $Log$
* Revision 1.1  1995/07/31 15:25:20  cdaq
* Initial revision
*

      implicit none

      include 'sos_data_structures.cmn'
      include 'sos_tracking.cmn'
      include 'sos_geometry.cmn'
      include 'sos_calorimeter.cmn'
      include 'gen_event_info.cmn'
      include 'sos_one_ev.par'
      include 'gctrak.inc'
      include 'gckine.inc'
      include 'gcvolu.inc'

      integer ihit                      ! hit number
*      character*4 lnames(0:10)		! volume names
      character*4 lnames(0:3)		! volume names
*      integer lnums(0:10)               ! volume numbers or copies
      integer lnums(0:3)               ! volume numbers or copies
      real xd(3), xm(3)			! coordinates
      real track_x, track_y, track_z    ! a point on the track
      real track_x_slope, track_y_slope ! slope of the track
      integer error_code                ! error return code
      real x, y, z                      ! coordinates
      real z_distance                   ! z distance to end of hut
      integer track,chamhit,scinhit,showhit ! index variables
      integer wirenum                   ! indicates GEANT wirenumber

      character*5 wire          !define names and indicies to loop over...
      character*5 scinname 
      integer iscin
      character*4 blockname
      character*5 layername
      character*1 blockletter
      integer blockidet
      integer ilayer
      integer irow

      character*1 viewopt

c      real par(10)			! geometry parameters
c      real x_wire                       ! x position of indvid. wires
*      integer WIREMEDIA
*      parameter (WIREMEDIA = 3)
c      integer ivolu

*
* Clear any previous drawing
*
* Reset the detector hit indicators...
      call s_one_ev_det_reset
*
      call iclrwk (0, 0)
      call gtrigi
*
* define some colors for the various wires, and turn shading on 
*
      call iscr(1,1,.5,.5,.5)        !make the detectors grey
      call iscr(1,15,1.,0.7,0.2)    !define an "orange"
      call iscr(1,13,0.,.65,0.)      !define a "dark green"
      call gdopt ('SHAD','ON')
*
* Take care of creating the reconstructed tracks
*
      vect(4) =  0.0
      vect(5) =  0.0
      vect(6) =  1.
      vect(7) =  1.
      ipart = 3            !electron to make track red
*      ipart = 13          !neutron to make track black
      tofg  = 1e-5
      itra  = 1
      amass = 0.511e-3
*      amass = 0.93957
      sleng = 200.
      step  = 200.

      if (SNTRACKS_FP .GT. 0) then
        do track = 1, SNTRACKS_FP
          track_x       = -sx_fp(track)	! x position on track
          track_y       = sy_fp(track)	! y position on track
          track_z       = 0             ! z position on track
          track_x_slope = -sxp_fp(track) ! track slope in x
          track_y_slope = syp_fp(track)	! track slope in y

          z		= -SHUT_HEIGHT / 2. ! bottom of hut
          z_distance	= track_z - z   ! distance from point to floor
          x		= track_x - z_distance * sin(track_x_slope) ! x loci
          y		= track_y - z_distance * sin(track_y_slope) ! y loci
          vect(1) = x
          vect(2) = y
 1	  vect(3) = z
          call gsxyz
          z		= SHUT_HEIGHT / 2. ! bottom of hut
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
* Now loop over all the detector elements "lighting" each one if it has been hit 
*
      xd(1) = 0.			! find the center of the detector
      xd(2) = 0.			! find the center of the detector
      xd(3) = 0.			! find the center of the detector
* Start with the wire chambers
* See the file "displaynumbering.help" for a description of the numbering of the
* various detector elements
*
      if (SDC_TOT_HITS .GT. 0) then
        lnames(0) = 'SHUT'
        lnums(0)  = 1
        do chamhit = 1, SDC_TOT_HITS
**************************************************************************************
*UUU
****
          if (SDC_PLANE_NUM(chamhit) .EQ. 1) then
            nlevel = 0			! initial value for # of levels
            lnames(1) = 'WCHA'		! level one
            lnums(1)  = 1               ! copy one, higher chamber
            lnames(2) = 'WAAU'		! U plane
            lnums(2)  = 1               ! copy one
            wirenum = nint(sdc_nrwire(SDC_PLANE_NUM(chamhit))) + 1
     &           - SDC_WIRE_NUM(chamhit)
            if (SDC_WIRE_COUNTING(SDC_PLANE_NUM(chamhit)) .EQ. 1)
     &           wirenum = SDC_WIRE_NUM(chamhit)
            if (wirenum .le. 24) write(wire,'(a,a,a,a)') 'AUA',char(64 + wirenum)
            if (wirenum .gt. 24) write(wire,'(a,a,a,a)') 'AUB',char(64 - 24 + wirenum)
            lnames(3) = wire		! U wires
            lnums(3)  = wirenum		! wire number
            call glvolu (4, lnames, lnums, error_code)
            call gdtom (xd, xm, 1)      ! transform from detector to MARS
            call gsatt (wire,'SEEN',1)
            call gsatt (wire,'COLO',15)
*            call gsahit (1, 4, 1, lnums(1), xm, ihit) ! store the hit
**************************************************************************************
*UUU
****
          elseif (SDC_PLANE_NUM(chamhit) .EQ. 2) then
            nlevel = 0			! initial value for # of levels
            lnames(1) = 'WCHA'		! level one
            lnums(1)  = 1               ! copy one, higher chamber
            lnames(2) = 'WABU'		! U plane
            lnums(2)  = 1               ! copy one
            wirenum = nint(sdc_nrwire(SDC_PLANE_NUM(chamhit))) + 1
     &           - SDC_WIRE_NUM(chamhit)
            if (SDC_WIRE_COUNTING(SDC_PLANE_NUM(chamhit)) .EQ. 1)
     &           wirenum = SDC_WIRE_NUM(chamhit)
            if (wirenum .le. 24) write(wire,'(a,a,a,a)') 'AUC',char(64 + wirenum)
            if (wirenum .gt. 24) write(wire,'(a,a,a,a)') 'AUD',char(64 - 24 + wirenum)
            lnames(3) = wire		! U wires
            lnums(3)  = wirenum		! wire number
            call glvolu (4, lnames, lnums, error_code)
            call gdtom (xd, xm, 1)      ! transform from detector to MARS
            call gsatt (wire,'SEEN',1)
            call gsatt (wire,'COLO',5)
*            call gsahit (1, 4, 1, lnums(1), xm, ihit) ! store the hit
**************************************************************************************
*VVV
****
          elseif (SDC_PLANE_NUM(chamhit) .EQ. 3) then
            nlevel = 0			! initial value for # of levels
            lnames(1) = 'WCHA'		! level one
            lnums(1)  = 1               ! copy one, higher chamber
            lnames(2) = 'WAAX'		! X plane
            lnums(2)  = 1               ! copy one
	    wirenum = nint(SDC_nrwire(SDC_PLANE_NUM(chamhit))) + 1
     &           - SDC_WIRE_NUM(chamhit)
            if (SDC_WIRE_COUNTING(SDC_PLANE_NUM(chamhit)) .EQ. 1)
     &           wirenum = SDC_WIRE_NUM(chamhit)
            if (wirenum .le. 16) write(wire,'(a,a,a,a)') 'AXA',char(64 + wirenum)
            if ((wirenum .gt. 16) .and. (wirenum .le. 32))  write(wire,'(a,a,a,a)') 
     $           'AXB',char(64 - 16 + wirenum)
            if ((wirenum .gt. 32) .and. (wirenum .le. 48))  write(wire,'(a,a,a,a)') 
     $           'AXC',char(64 - 32 + wirenum)
            if ((wirenum .gt. 48) .and. (wirenum .le. 64))  write(wire,'(a,a,a,a)') 
     $           'AXD',char(64 - 48 + wirenum)
	    lnames(3) = wire
            lnums(3)  = wirenum		! wire number
            call glvolu (4, lnames, lnums, error_code)
            call gdtom (xd, xm, 1)      ! transform from detector to MARS
            call gsatt (wire,'SEEN',1)
            call gsatt (wire,'COLO',13)
*           call gsahit (1, 6, 1, lnums(1), xm, ihit) ! store the hit
**************************************************************************************
*VVV
****
         elseif (SDC_PLANE_NUM(chamhit) .EQ. 4) then
           nlevel = 0			! initial value for # of levels
           lnames(1) = 'WCHA'		! level one
           lnums(1)  = 1                ! copy one, higher chamber
           lnames(2) = 'WABX'		! X plane
           lnums(2)  = 1                ! copy one
           wirenum = nint(SDC_nrwire(SDC_PLANE_NUM(chamhit))) + 1
     &          - SDC_WIRE_NUM(chamhit)
           if (SDC_WIRE_COUNTING(SDC_PLANE_NUM(chamhit)) .EQ. 1)
     &          wirenum = SDC_WIRE_NUM(chamhit)
           if (wirenum .le. 16) write(wire,'(a,a,a,a)') 'AXE',char(64 +
     $          wirenum)
           if ((wirenum .gt. 16) .and. (wirenum .le. 32))  write(wire
     $          ,'(a,a,a,a)')'AXF',char(64 - 16 + wirenum)
           if ((wirenum .gt. 32) .and. (wirenum .le. 48))  write(wire
     $          ,'(a,a,a,a)')'AXG',char(64 - 32 + wirenum)
           if ((wirenum .gt. 48) .and. (wirenum .le. 64))  write(wire
     $          ,'(a,a,a,a)')'AXH',char(64 - 48 + wirenum)
           lnames(3) = wire
           lnums(3)  = wirenum		! wire number
           call glvolu (4, lnames, lnums, error_code)
           call gdtom (xd, xm, 1)       ! transform from detector to MARS
           call gsatt (wire,'SEEN',1)
           call gsatt (wire,'COLO',3)
*     call gsahit (1, 6, 1, lnums(1), xm, ihit) ! store the hit
**************************************************************************************
*XXX
****
         elseif (SDC_PLANE_NUM(chamhit) .EQ. 5) then
           nlevel = 0			! initial value for # of levels
           lnames(1) = 'WCHA'		! level one
           lnums(1)  = 1                ! copy one, higher chamber
           lnames(2) = 'WAAV'		! V plane
           lnums(2)  = 1                ! copy one
           wirenum = nint(SDC_nrwire(SDC_PLANE_NUM(chamhit))) + 1
     &          - SDC_WIRE_NUM(chamhit)
           if (SDC_WIRE_COUNTING(SDC_PLANE_NUM(chamhit)) .EQ. 1)
     &          wirenum = SDC_WIRE_NUM(chamhit)
           if (wirenum .le. 24) write(wire,'(a,a,a,a)') 'AVA',char(64 +
     $          wirenum)
           if (wirenum .gt. 24) write(wire,'(a,a,a,a)') 'AVB',char(64 - 24
     $          + wirenum)
           lnames(3) = wire		! V wires
           lnums(3)  = wirenum		! wire number
           call glvolu (4, lnames, lnums, error_code)
           call gdtom (xd, xm, 1)       ! transform from detector to MARS
           call gsatt (wire,'SEEN',1)
           call gsatt (wire,'COLO',4)
*            call gsahit (1, 5, 1, lnums(1), xm, ihit) ! store the hit
*            call gsahit (1, 90, 1, lnums(1), xm, ihit) ! store the hit
**************************************************************************************
*XXX
****
         elseif (SDC_PLANE_NUM(chamhit) .EQ. 6) then
           nlevel = 0			! initial value for # of levels
           lnames(1) = 'WCHA'		! level one
           lnums(1)  = 1                ! copy one, higher chamber
           lnames(2) = 'WABV'		! V plane
           lnums(2)  = 1                ! copy one
           wirenum = nint(SDC_nrwire(SDC_PLANE_NUM(chamhit))) + 1
     &          - SDC_WIRE_NUM(chamhit)
           if (SDC_WIRE_COUNTING(SDC_PLANE_NUM(chamhit)) .EQ. 1)
     &          wirenum = SDC_WIRE_NUM(chamhit)
           if (wirenum .le. 24) write(wire,'(a,a,a,a)') 'AVC',char(64 +
     $          wirenum)
           if (wirenum .gt. 24) write(wire,'(a,a,a,a)') 'AVD',char(64 - 24
     $          + wirenum)
           lnames(3) = wire		! V wires
           lnums(3)  = wirenum		! wire number
           call glvolu (4, lnames, lnums, error_code)
           call gdtom (xd, xm, 1)       ! transform from detector to MARS
           call gsatt (wire,'SEEN',1)
           call gsatt (wire,'COLO',7)
*     call gsahit (1, 5, 1, lnums(1), xm, ihit) ! store the hit
*            call gsahit (1, 90, 1, lnums(1), xm, ihit) ! store the hit
**************************************************************************************
**************************************************************************************
*UUU
****
         elseif (SDC_PLANE_NUM(chamhit) .EQ. 7) then
           nlevel = 0			! initial value for # of levels
           lnames(1) = 'WCHB'		! level one
           lnums(1)  = 1                ! copy one, higher chamber
           lnames(2) = 'WBAU'		! U plane
           lnums(2)  = 1                ! copy one
           wirenum = nint(sdc_nrwire(SDC_PLANE_NUM(chamhit))) + 1
     &          - SDC_WIRE_NUM(chamhit)
           if (SDC_WIRE_COUNTING(SDC_PLANE_NUM(chamhit)) .EQ. 1)
     &          wirenum = SDC_WIRE_NUM(chamhit)
           if (wirenum .le. 24) write(wire,'(a,a,a,a)') 'BUA',char(64 +
     $          wirenum)
           if (wirenum .gt. 24) write(wire,'(a,a,a,a)') 'BUB',char(64 - 24
     $          + wirenum)
           lnames(3) = wire		! U wires
           lnums(3)  = wirenum		! wire number
           call glvolu (4, lnames, lnums, error_code)
           call gdtom (xd, xm, 1)       ! transform from detector to MARS
           call gsatt (wire,'SEEN',1)
           call gsatt (wire,'COLO',15)
*            call gsahit (1, 4, 1, lnums(1), xm, ihit) ! store the hit
**************************************************************************************
*UUU
****
         elseif (SDC_PLANE_NUM(chamhit) .EQ. 8) then
           nlevel = 0			! initial value for # of levels
           lnames(1) = 'WCHB'		! level one
           lnums(1)  = 1                ! copy one, higher chamber
           lnames(2) = 'WBBU'		! U plane
           lnums(2)  = 1                ! copy one
           wirenum = nint(sdc_nrwire(SDC_PLANE_NUM(chamhit))) + 1
     &          - SDC_WIRE_NUM(chamhit)
           if (SDC_WIRE_COUNTING(SDC_PLANE_NUM(chamhit)) .EQ. 1)
     &          wirenum = SDC_WIRE_NUM(chamhit)
           if (wirenum .le. 24) write(wire,'(a,a,a,a)') 'BUC',char(64 +
     $          wirenum)
           if (wirenum .gt. 24) write(wire,'(a,a,a,a)') 'BUD',char(64 - 24
     $          + wirenum)
           lnames(3) = wire		! U wires
           lnums(3)  = wirenum		! wire number
           call glvolu (4, lnames, lnums, error_code)
           call gdtom (xd, xm, 1)       ! transform from detector to MARS
           call gsatt (wire,'SEEN',1)
           call gsatt (wire,'COLO',5)
*            call gsahit (1, 4, 1, lnums(1), xm, ihit) ! store the hit
**************************************************************************************
*VVV
****
         elseif (SDC_PLANE_NUM(chamhit) .EQ. 9) then
           nlevel = 0			! initial value for # of levels
           lnames(1) = 'WCHB'		! level one
           lnums(1)  = 1                ! copy one, higher chamber
           lnames(2) = 'WBAX'		! X plane
           lnums(2)  = 1                ! copy one
           wirenum = nint(SDC_nrwire(SDC_PLANE_NUM(chamhit))) + 1
     &          - SDC_WIRE_NUM(chamhit)
           if (SDC_WIRE_COUNTING(SDC_PLANE_NUM(chamhit)) .EQ. 1)
     &          wirenum = SDC_WIRE_NUM(chamhit)
           if (wirenum .le. 16) write(wire,'(a,a,a,a)') 'BXA',char(64 +
     $          wirenum)
           if ((wirenum .gt. 16) .and. (wirenum .le. 32))  write(wire
     $          ,'(a,a,a,a)')'BXB',char(64 - 16 + wirenum)
           if ((wirenum .gt. 32) .and. (wirenum .le. 48))  write(wire
     $          ,'(a,a,a,a)')'BXC',char(64 - 32 + wirenum)
           if ((wirenum .gt. 48) .and. (wirenum .le. 64))  write(wire
     $          ,'(a,a,a,a)')'BXD',char(64 - 48 + wirenum)
           lnames(3) = wire
           lnums(3)  = wirenum		! wire number
           call glvolu (4, lnames, lnums, error_code)
           call gdtom (xd, xm, 1)       ! transform from detector to MARS
           call gsatt (wire,'SEEN',1)
           call gsatt (wire,'COLO',13)
*           call gsahit (1, 6, 1, lnums(1), xm, ihit) ! store the hit
**************************************************************************************
*VVV
****
         elseif (SDC_PLANE_NUM(chamhit) .EQ. 10) then
           nlevel = 0			! initial value for # of levels
           lnames(1) = 'WCHB'		! level one
           lnums(1)  = 1                ! copy one, higher chamber
           lnames(2) = 'WBBX'		! X plane
           lnums(2)  = 1                ! copy one
           wirenum = nint(SDC_nrwire(SDC_PLANE_NUM(chamhit))) + 1
     &          - SDC_WIRE_NUM(chamhit)
           if (SDC_WIRE_COUNTING(SDC_PLANE_NUM(chamhit)) .EQ. 1)
     &          wirenum = SDC_WIRE_NUM(chamhit)
           if (wirenum .le. 16) write(wire,'(a,a,a,a)') 'BXE',char(64 +
     $          wirenum)
           if ((wirenum .gt. 16) .and. (wirenum .le. 32))  write(wire
     $          ,'(a,a,a,a)')'BXF',char(64 - 16 + wirenum)
           if ((wirenum .gt. 32) .and. (wirenum .le. 48))  write(wire
     $          ,'(a,a,a,a)')'BXG',char(64 - 32 + wirenum)
           if ((wirenum .gt. 48) .and. (wirenum .le. 64))  write(wire
     $          ,'(a,a,a,a)')'BXH',char(64 - 48 + wirenum)
           lnames(3) = wire
           lnums(3)  = wirenum		! wire number
           call glvolu (4, lnames, lnums, error_code)
           call gdtom (xd, xm, 1)       ! transform from detector to MARS
           call gsatt (wire,'SEEN',1)
           call gsatt (wire,'COLO',3)
*           call gsahit (1, 6, 1, lnums(1), xm, ihit) ! store the hit
**************************************************************************************
*XXX
****
         elseif (SDC_PLANE_NUM(chamhit) .EQ. 11) then
           nlevel = 0			! initial value for # of levels
           lnames(1) = 'WCHB'		! level one
           lnums(1)  = 1                ! copy one, higher chamber
           lnames(2) = 'WBAV'		! V plane
           lnums(2)  = 1                ! copy one
           wirenum = nint(SDC_nrwire(SDC_PLANE_NUM(chamhit))) + 1
     &          - SDC_WIRE_NUM(chamhit)
           if (SDC_WIRE_COUNTING(SDC_PLANE_NUM(chamhit)) .EQ. 1)
     &          wirenum = SDC_WIRE_NUM(chamhit)
           if (wirenum .le. 24) write(wire,'(a,a,a,a)') 'BVA',char(64 +
     $          wirenum)
           if (wirenum .gt. 24) write(wire,'(a,a,a,a)') 'BVB',char(64 - 24
     $          + wirenum)
           lnames(3) = wire		! V wires
           lnums(3)  = wirenum		! wire number
           call glvolu (4, lnames, lnums, error_code)
           call gdtom (xd, xm, 1)       ! transform from detector to MARS
           call gsatt (wire,'SEEN',1)
           call gsatt (wire,'COLO',4)
*            call gsahit (1, 5, 1, lnums(1), xm, ihit) ! store the hit
*            call gsahit (1, 90, 1, lnums(1), xm, ihit) ! store the hit
**************************************************************************************
*XXX
****
         elseif (SDC_PLANE_NUM(chamhit) .EQ. 12) then
           nlevel = 0			! initial value for # of levels
           lnames(1) = 'WCHB'		! level one
           lnums(1)  = 1                ! copy one, higher chamber
           lnames(2) = 'WBBV'		! V plane
           lnums(2)  = 1                ! copy one
           wirenum = nint(SDC_nrwire(SDC_PLANE_NUM(chamhit))) + 1
     &          - SDC_WIRE_NUM(chamhit)
           if (SDC_WIRE_COUNTING(SDC_PLANE_NUM(chamhit)) .EQ. 1)
     &          wirenum = SDC_WIRE_NUM(chamhit)
           if (wirenum .le. 24) write(wire,'(a,a,a,a)') 'BVC',char(64 +
     $          wirenum)
           if (wirenum .gt. 24) write(wire,'(a,a,a,a)') 'BVD',char(64 - 24
     $          + wirenum)
           lnames(3) = wire		! V wires
           lnums(3)  = wirenum		! wire number
           call glvolu (4, lnames, lnums, error_code)
           call gdtom (xd, xm, 1)       ! transform from detector to MARS
           call gsatt (wire,'SEEN',1)
           call gsatt (wire,'COLO',7)
*            call gsahit (1, 5, 1, lnums(1), xm, ihit) ! store the hit
*            call gsahit (1, 90, 1, lnums(1), xm, ihit) ! store the hit
**************************************************************************************
         endif
       enddo
      endif

* Take a look at the hodoscopes
* See the file "displaynumbering.help" for a description of the numbering of the
* various detector elements
*
      if (SSCIN_TOT_HITS .GT. 0) then
        lnames(0) = 'SHUT'              ! relative to the hut
        lnums(0)  = 1                   ! copy 1
        do scinhit = 1, SSCIN_TOT_HITS
*
* First the lower X
*
          if (SSCIN_PLANE_NUM(scinhit) .EQ. 1) then
            nlevel = 0			! initial value for # of levels
            lnames(1) = 'HOD1'		! level one
            lnums(1)  = 1               ! copy one, lower hodo
            lnames(2) = 'HDX1'		! X strips
            lnums(2)  = 1               ! copy one
	    write (scinname,'(a,a)') 'H1X',char(64 + SSCIN_COUNTER_NUM(scinhit))
            lnames(3) = scinname		! X strips
            lnums(3)  = SSCIN_COUNTER_NUM(scinhit) ! X strip number
            call glvolu (4, lnames, lnums, error_code)
            call gdtom (xd, xm, 1)      ! transform from detector to MARS
*            call gsahit (1, 2, 1, lnums(1), xm, ihit) ! store the hit
	    call gsatt (scinname,'COLO',4)   !change the color of the it element
            call gsatt (scinname,'FILL',5)
            call gsatt (scinname,'LWID',1)
*     
* now the upper X
*
          elseif (SSCIN_PLANE_NUM(scinhit) .EQ. 3) then
            nlevel = 0			! initial value for # of levels
            lnames(1) = 'HOD2'		! level one
            lnums(1)  = 1               ! copy two, upper hodo
            lnames(2) = 'HDX2'		! X strips
            lnums(2)  = 1               ! copy one
	    write (scinname,'(a,a)') 'H2X',char(64 + SSCIN_COUNTER_NUM(scinhit))
            lnames(3) = scinname		! X strips
            lnums(3)  = SSCIN_COUNTER_NUM(scinhit) ! X strip number
            call glvolu (4, lnames, lnums, error_code)
            call gdtom (xd, xm, 1)      ! transform from detector to MARS
*            call gsahit (1, 2, 1, lnums(1), xm, ihit) ! store the hit
	    call gsatt (scinname,'COLO',4)   !change the color of the it element
            call gsatt (scinname,'FILL',5)
            call gsatt (scinname,'LWID',1)
*
* now the lower Y
*
          elseif (SSCIN_PLANE_NUM(scinhit) .EQ. 2) then
            nlevel = 0			! initial value for # of levels
	    lnames(1) = 'HOD1'
            lnums(1)  = 1               ! copy one, lower hodo
            lnames(2) = 'HDY1'		! Y strips
            lnums(2)  = 1               ! copy one
	    write (scinname,'(a,a)') 'H1Y',char(64 + SSCIN_COUNTER_NUM(scinhit))
            lnames(3) = scinname		! Y strips
            lnums(3)  = SSCIN_COUNTER_NUM(scinhit) ! Y strip number
            call glvolu (4, lnames, lnums, error_code)
            call gdtom (xd, xm, 1)      ! transform from detector to MARS
*            call gsahit (1, 3, 1, lnums(1), xm, ihit) ! store the hit
	    call gsatt (scinname,'COLO',4)   !change the color of the it element
            call gsatt (scinname,'FILL',5)
            call gsatt (scinname,'LWID',1)
*     
* now the upper Y
*
          elseif (SSCIN_PLANE_NUM(scinhit) .EQ. 4) then
            nlevel = 0                  ! initial value for # of levels
	    lnames(1) = 'HOD2'
            lnums(1)  = 1               ! copy two, upper hodo
            lnames(2) = 'HDY2'          ! Y strips
            lnums(2)  = 1               ! copy one
	    write (scinname,'(a,a)') 'H2Y',char(64 + SSCIN_COUNTER_NUM(scinhit))
            lnames(3) = scinname          ! Y strips
            lnums(3)  = SSCIN_COUNTER_NUM(scinhit) ! Y strip number
            call glvolu (4, lnames, lnums, error_code)
            call gdtom (xd, xm, 1)      ! transform from detector to MARS
*            call gsahit (1, 3, 1, lnums(1), xm, ihit) ! store the hit
	    call gsatt (scinname,'COLO',4)   !change the color of the it element
            call gsatt (scinname,'FILL',5)
            call gsatt (scinname,'LWID',1)
          endif
        enddo
      endif
* 
* Now take care of the shower detector
* See the file "displaynumbering.help" for a description of the numbering of the
* various detector elements
*
      lnames(0) = 'SHUT'
      lnums(0) = 1
      if (SCAL_NUM_HITS .GE. 0) then
        do showhit = 1, SCAL_NUM_HITS
          nlevel = 0
          lnames(1) = 'SHOW'            ! shower detector
          lnums(1)  = 4                 ! copy 1
          write(layername,'(a,i1)') 'LAY',SCAL_COLS(showhit)
*      print*,'layername = ',layername
          lnames(2) = layername         ! x subdivisions
*            lnums(2)  = SCAL_COLS(showhit) ! x subdivision number
          lnums(2) = 11
*            lnums(3)  = SCAL_ROWS(showhit) ! which block
          lnums(3)  = 1
*            blockidet = int(43 + smax_cal_rows*(SCAL_COLS(showhit) -1) + SCAL_ROWS(showhit))
          write (blockname,'(a,i1,a)') 'BL',SCAL_COLS(showhit),
     $         char(64 + scal_rows(showhit))
*      print*, 'blockname = ',blockname
          lnames(3) = blockname         ! z subdivisions
*      print*,'lnames = ',lnames,'   lnums = ',lnums
          call glvolu(4, lnames, lnums, error_code)
*      print*,'error code = ',error_code
          call gdtom (xd, xm, 1)        ! transform from det to MARS
*      print*,'blockidet = ',blockidet
*      print*,'xm = ',xm
*      print*,'ihit = ',ihit
*            call gsahit (1 ,blockidet, 1, lnums(2), xm, ihit) ! store the hit
          call gsatt (blockname,'COLO',4) !change the color of the it element
          call gsatt (blockname,'FILL',5)
          call gsatt (blockname,'LWID',2)
        enddo
      endif
*******************************************************************************
*******************************************************************************
***   Now the display part...
*******************************************************************************
*******************************************************************************

      viewopt = 'a'
      do while (viewopt .ne. 'p')
        
 100    print*, 'enter a for 3D view with wc blow up'
        print*, 'enter b for top and side views'
        print*, 'enter c for a head on view of the wc''s'
        print*, 'enter p to go back to the CTP prompt.'
        read*, viewopt
        if ((viewopt .ne. 'a') .and. (viewopt .ne. 'b') .and. (viewopt
     $       .ne. 'c').and. (viewopt .ne. 'p')) then
          print *, 'Invalid option.  Please type a, b, c, or p.'
          goto 100
        endif
        call ixclrwi
*     Do the 3d View
*     
        if (viewopt .eq. 'a') then
          call gdopen (8)
          call gdrawt (5.,2.,'PERSPECTIVE VIEW',.5,0.,2,0)
          call gdraw ('SHUT', 45., 115., 90., 3.0, 9.0, 0.06, 0.06)
          call gdxyz (0)		! draw the tracks
          call gdhits ('*   ', '*   ', 0, 850, 0.1)
          call gdclos (8)
*     
*     blow up the wire chambers, and make the hodoscopes invisible
*     
          call gdopen (9)
          call gsatt ('HDX1','SEEN',0)
          call gsatt ('HDY1','SEEN',0)
          do iscin=1,LOWER_HODO_X_PADDLES
            write(scinname,'(a,a)') 'H1X',char(64 + iscin)
            call gsatt (scinname,'SEEN',0)
          enddo
          do iscin=1,LOWER_HODO_Y_PADDLES
            write(scinname,'(a,a)') 'H1Y',char(64 + iscin)
            call gsatt (scinname,'SEEN',0)
          enddo
          call gdraw ('SHUT', 45., 115., 90., 14.0, 6.1, 0.15, 0.15)
          call gdxyz (0)		! draw the tracks
          call gdhits ('*   ', '*   ', 0, 850, 0.3)
          call gdclos (9)
          call gsatt ('HDX1','SEEN',1)
          call gsatt ('HDY1','SEEN',1)
          call gdclos (9)
*     Now make them visible again for the next pass...
          do iscin=1,LOWER_HODO_X_PADDLES
            write(scinname,'(a,a)') 'H1X',char(64 + iscin)
            call gsatt (scinname,'SEEN',1)
          enddo
          do iscin=1,LOWER_HODO_Y_PADDLES
            write(scinname,'(a,a)') 'H1Y',char(64 + iscin)
            call gsatt (scinname,'SEEN',1)
          enddo
          call gdshow (9)
          call gdshow (8)
          call gdshow (9)
          call gdelet (8)
          call gdelet (9)
***   
        elseif (viewopt .eq. 'c') then
*     
*     Head On view
*     
          call gdopen (5)
*f    irst, get all the background junk out of the picture...
          call gsatt ('HDX1','SEEN',0)
          call gsatt ('HDX2','SEEN',0)
          call gsatt ('HDY1','SEEN',0)
          call gsatt ('HDY2','SEEN',0)
          call gsatt ('SHOW','SEEN',0)
          do iscin=1,LOWER_HODO_X_PADDLES
            write(scinname,'(a,a)') 'H1X',char(64 + iscin)
            call gsatt (scinname,'SEEN',0)
          enddo
          do iscin=1,LOWER_HODO_Y_PADDLES
            write(scinname,'(a,a)') 'H1Y',char(64 + iscin)
            call gsatt (scinname,'SEEN',0)
          enddo
          do iscin=1,UPPER_HODO_X_PADDLES
            write(scinname,'(a,a)') 'H2X',char(64 + iscin)
            call gsatt (scinname,'SEEN',0)
          enddo
          do iscin=1,UPPER_HODO_Y_PADDLES
            write(scinname,'(a,a)') 'H2Y',char(64 + iscin)
            call gsatt (scinname,'SEEN',0)
          enddo
          do ilayer =1,SMAX_CAL_COLUMNS
            write(layername,'(a,i1)') 'LAY',ilayer
            call gsatt (layername,'SEEN',0)
            do irow = 1,SMAX_CAL_ROWS
              write(blockname,'(a,i1,a)') 'BL',ilayer,char(64 + irow)
              call gsatt (blockname,'SEEN',0)
            enddo
          enddo         
          call gdhits ('*   ', '*   ', 0, 850, 0.3)
          call gdrawt (3.,2.,'HEAD ON VIEW',.5,0.,2,0)
          call gdraw ('SHUT', 0., 0., 90., 10.0, 8.5,0.25,0.25)
          call gdxyz (0)		! draw the tracks
          call gdclos (5)
          call gdshow (5)
          call gdshow (5)

*     It's already been stored, so now make everything visible again for 
*     the next pass
          call gsatt ('HDX1','SEEN',1)
          call gsatt ('HDY1','SEEN',1)
          call gsatt ('HDX2','SEEN',1)
          call gsatt ('HDY2','SEEN',1)
          do iscin=1,LOWER_HODO_X_PADDLES
            write(scinname,'(a,a)') 'H1X',char(64 + iscin)
            call gsatt (scinname,'SEEN',1)
          enddo
          do iscin=1,LOWER_HODO_Y_PADDLES
            write(scinname,'(a,a)') 'H1Y',char(64 + iscin)
            call gsatt (scinname,'SEEN',1)
          enddo
          do iscin=1,UPPER_HODO_X_PADDLES
            write(scinname,'(a,a)') 'H2X',char(64 + iscin)
            call gsatt (scinname,'SEEN',1)
          enddo
          do iscin=1,UPPER_HODO_Y_PADDLES
            write(scinname,'(a,a)') 'H2Y',char(64 + iscin)
            call gsatt (scinname,'SEEN',1)
          enddo
          do ilayer =1,SMAX_CAL_COLUMNS
            write(layername,'(a,i1)') 'LAY',ilayer
            call gsatt (layername,'SEEN',1)
            do irow = 1,SMAX_CAL_ROWS
              write(blockname,'(a,i1,a)') 'BL',ilayer,char(64 + irow)
              call gsatt (blockname,'SEEN',1)
            enddo
          enddo
          call gdelet (5)
*     
        elseif (viewopt .eq. 'b') then  !draw the two side views
*     
*     Top view
*     
*     
          call gdopen (7)
          call gsatt ('HDX1','SEEN',0)
          call gsatt ('HDX2','SEEN',0)
          call gdrawt (4.4,2.,'TOP VIEW',.5,0.,2,0)
          call gdraw ('SHUT', 270., 0., 90., 4.4,5., 0.045, 0.045)
          call gdxyz (0)                ! draw the tracks
          call gdhits ('*   ', '*   ', 0, 850, 0.1)
          call gdhits ('HOD1', 'HDY1', 0, 850, 0.1)
          call gdhits ('HOD2', 'HDY2', 0, 850, 0.1)
          call gdclos (7)
*     
*     
*     Other side view
*     
          call gdopen (6)
          call gsatt ('HDY1','SEEN',0)
          call gsatt ('HDY2','SEEN',0)
          call gdrawt (14.75,2.,'SIDE VIEW',.5,0.,2,0)
          call gdraw ('SHUT', 90., 90., 90., 14.75,5.,0.045, 0.045)
          call gdxyz (0)                ! draw the tracks
          call gdhits ('*   ', '*   ', 0, 850, 0.1)
          call gdclos (6)
          call gdshow (7)
          call gdshow (6)
          call gdshow (7)
          call gdelet (6)
          call gdelet (7)
*     
        endif
*     
      enddo
*****************************************************************************
*     Clear the hit banks
      call gtrigc

      end
