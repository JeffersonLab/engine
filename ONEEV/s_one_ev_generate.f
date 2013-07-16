      subroutine s_one_ev_generate
*
* $Log: s_one_ev_generate.f,v $
* Revision 1.2  1996/09/04 20:06:35  saw
* (SAW) hdc_nrwire already integer, don't nint it.
*
* Revision 1.1  1996/01/17 16:37:21  cdaq
* Initial revision
*

      implicit none

      include 'sos_data_structures.cmn'
      include 'sos_tracking.cmn'
      include 'sos_geometry.cmn'
      include 'sos_calorimeter.cmn'
      include 'gen_event_info.cmn'
      include 'gen_run_info.cmn'
      include 'sos_one_ev.par'
      include 'gen_one_ev_gctrak.cmn'
      include 'gen_one_ev_gckine.cmn'
      include 'gen_one_ev_gcvolu.cmn'

      character*4 lnames(0:3)		! volume names
      integer lnums(0:3)                ! volume numbers or copies
      real xd(3), xm(3)			! coordinates
      integer error_code                ! error return code
      integer chamhit,scinhit,showhit   ! index variables
      integer wirenum                   ! indicates GEANT wirenumber

      character*5 wire          !define names and indicies to loop over...
      character*5 scinname 
      character*4 blockname
      character*5 layername

*
* Reset the detector hit indicators...
      call s_one_ev_det_reset
*
* Clear any previous drawing
*
      call iclrwk (0, 0)
      call gtrigc
      call gtrigi
*
* define some colors for the various wires, and turn shading on 
*
      call iscr(1,1,.5,.5,.5)        !make the detectors grey
      call iscr(1,15,1.,0.7,0.2)    !define an "orange"
      call iscr(1,13,0.,.65,0.)      !define a "dark green"
      call iscr(1,14,0.,0.,1.)       !define a dark blue
      call gdopt ('SHAD','ON')

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
            wirenum = (sdc_nrwire(SDC_PLANE_NUM(chamhit))) + 1
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
            call gsatt (wire,'COLO',13)
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
            wirenum = (sdc_nrwire(SDC_PLANE_NUM(chamhit))) + 1
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
            call gsatt (wire,'COLO',13)
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
	    wirenum = (SDC_nrwire(SDC_PLANE_NUM(chamhit))) + 1
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
           wirenum = (SDC_nrwire(SDC_PLANE_NUM(chamhit))) + 1
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
           call gsatt (wire,'COLO',13)
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
           wirenum = (SDC_nrwire(SDC_PLANE_NUM(chamhit))) + 1
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
           call gsatt (wire,'COLO',13)
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
           wirenum = (SDC_nrwire(SDC_PLANE_NUM(chamhit))) + 1
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
           call gsatt (wire,'COLO',13)
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
           wirenum = (sdc_nrwire(SDC_PLANE_NUM(chamhit))) + 1
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
           wirenum = (sdc_nrwire(SDC_PLANE_NUM(chamhit))) + 1
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
           call gsatt (wire,'COLO',15)
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
           wirenum = (SDC_nrwire(SDC_PLANE_NUM(chamhit))) + 1
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
           call gsatt (wire,'COLO',15)
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
           wirenum = (SDC_nrwire(SDC_PLANE_NUM(chamhit))) + 1
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
           call gsatt (wire,'COLO',15)
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
           wirenum = (SDC_nrwire(SDC_PLANE_NUM(chamhit))) + 1
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
           call gsatt (wire,'COLO',15)
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
           wirenum = (SDC_nrwire(SDC_PLANE_NUM(chamhit))) + 1
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
           call gsatt (wire,'COLO',15)
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
	    call gsatt (scinname,'COLO',14)   !change the color of the it element
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
	    call gsatt (scinname,'COLO',14)   !change the color of the it element
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
	    call gsatt (scinname,'COLO',14)    !change the color of the it element
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
	    call gsatt (scinname,'COLO',14)   !change the color of the it element
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
          lnames(2) = layername         ! x subdivisions
          lnums(2) = 11
          lnums(3)  = 1
          write (blockname,'(a,i1,a)') 'BL',SCAL_COLS(showhit),
     $         char(64 + scal_rows(showhit))
          lnames(3) = blockname         ! z subdivisions
          call glvolu(4, lnames, lnums, error_code)
          call gdtom (xd, xm, 1)        ! transform from det to MARS
          call gsatt (blockname,'COLO',14) !change the color of the it element
          call gsatt (blockname,'FILL',5)
          call gsatt (blockname,'LWID',2)
        enddo
      endif
      end
