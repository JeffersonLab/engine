      subroutine s_one_ev_geometry
*
* This routine will get the detector position and size information from CTP,
* then use this information for defining the different GEANT geometry structures
*
* August, 1994, Pat Welch, Oregon State University, tpw@physics.orst.edu
*
* Note: Subdivided volumes won't work for doing coordinate transforms.  Or
*	at least I didn't see a method around them.  So I have defined all
*	the subvolumes explicitly. (TPW)
*
* Modified from HMS version, h_one_ev_geometry, March 1995 by
* Derek van Westrum (vanwestr@cebaf.gov)
*
* $Log: s_one_ev_geometry.f,v $
* Revision 1.5  1996/11/22 15:37:10  saw
* (SAW) Don't let U&V wires extend beyond chamber.  Some code cleanup.
*
* Revision 1.4  1996/04/30 14:10:39  saw
* (DVW) Code update
*
* Revision 1.3  1996/01/17 16:37:48  cdaq
* (DVW) Tweak hodoscale
*
* Revision 1.2  1995/10/06 18:24:18  cdaq
* (DVW) Changed to ctp geometry variables and eliminated call to s_one_ev.par.
*
* Revision 1.1  1995/07/31 15:23:38  cdaq
* Initial revision
* h_one_ev_geometry.f,v $

      implicit none

      include 'sos_data_structures.cmn'
      include 'sos_geometry.cmn'
      include 'sos_calorimeter.cmn'
      include 'sos_scin_parms.cmn'

      real*4    SHUT_WIDTH,SHUT_HEIGHT
      parameter (SHUT_WIDTH = 100.)     ! full width of the det. hut
      parameter (SHUT_HEIGHT = 800.)    ! full height of the det. hut
      integer SHUTMEDIA                 ! non-sensitive tracking media
      integer DETMEDIA                  ! sensitive tracking media
      parameter (SHUTMEDIA = 1, DETMEDIA = 2)
      real*4 hodoscale
      parameter (hodoscale = 2.)
      real*4 wcscale
*      parameter (wcscale = 5.)
      parameter (wcscale = 1.)
      real*4 xwirelength                   
      real*4 ywirelength                   
      real*4 uwirelength
      real*4 vwirelength
      parameter(xwirelength = 40.0)
      parameter(ywirelength = 66.0)
      parameter(uwirelength = 80)   != xwirelength/sin(60 degrees)
      parameter(vwirelength = 80)   != xwirelength/sin(60 degrees)


      character*5 scinname 
      character*5 layername
      character*5 planename
      character*5 plane
      character*5 wire
      character*5 blockname
      integer isector
      integer iplane
      integer iwire
      integer ichamber
      integer ilayer
      integer irow

      integer ivolu			! internal volume number
      real par(10)			! geometry parameters
      real x, y, z			! offset position for placement of dets
      integer i                         ! index variable

      real wspace                       ! Wire spacing temp variable
      real xtemp,ytemp                  ! Temporary variables for
      real xplus,yplus                  ! display correct wire lengths.
      real xminus,yminus
  
      real*4 raddeg
      parameter (raddeg = 3.14159265/180.)

* First define two general media that everything is made of
* one is insensitive, and the other is sensitive

      call gstmed (SHUTMEDIA, 'air', 15, 0, 0,0.,20.,1.,0.5,1.,1.,0,0)
      call gstmed (DETMEDIA, 'det', 15, 1, 0,0.,20.,1.,0.5,1.,1.,0,0)

* Now define the mother volume that the detectors sit in
      par(1) = SHUT_WIDTH / 2.          ! half width in x of mother volume
      par(2) = SHUT_WIDTH / 2.          ! half width in y of mother volume
      par(3) = SHUT_HEIGHT / 2.         ! half height in z of mother volume
      call g_ugsvolu ('SHUT', 'BOX ', SHUTMEDIA, par, 3, ivolu)
      call gsatt ('SHUT', 'SEEN', 0)	! can't see the hut


* Now define the wire chambers as a collection of planes
* First the U and V planes.
*
      par(1) = ywirelength/2.
      par(2) = xwirelength/2.
      par(3) = wcscale * (sdc_zpos(2) - sdc_zpos(1))/ 2. ! half width of chamber planes
      do ichamber = 1,2
         do iplane = 1,2
           write(planename,'(a,a,a,a)') "W",char(64+ichamber),char(64+iplane),"U"
           call g_ugsvolu (planename, 'BOX ', DETMEDIA, par, 3, ivolu)
           write(planename,'(a,a,a,a)') "W",char(64+ichamber),char(64+iplane),"V"
           call g_ugsvolu (planename, 'BOX ', DETMEDIA, par, 3, ivolu)
         enddo
       enddo
*
* Now do the X planes. 
*
      par(1) = ywirelength/2.
      par(2) = xwirelength/2.
      par(3) = wcscale * (sdc_zpos(2) - sdc_zpos(1))/ 2. ! half width of chamber planes
      do ichamber = 1,2
         do iplane = 1,2
           write(planename,'(a,a,a,a)') "W",char(64+ichamber),char(64+iplane),"X"
           call g_ugsvolu (planename, 'BOX ', DETMEDIA, par, 3, ivolu)
         enddo
       enddo

! make a volume for 6 planes.  The size here should be cool. DVW 18 jul 95
      par(1) = ywirelength/2.
      par(2) = xwirelength/2.
      par(3) = wcscale * (6./5. * (sdc_zpos(6) - sdc_zpos(1))) / 2.
      call g_ugsvolu ('WCHA', 'BOX ', DETMEDIA, par, 3, ivolu) ! Wire chamber
      call g_ugsvolu ('WCHB', 'BOX ', DETMEDIA, par, 3, ivolu) ! Wire chamber

* Now place the planes within the wire chamber.  Start with U
      z = - wcscale * (5. / 2.) * (sdc_zpos(2) -sdc_zpos(1))
      call gspos ('WAAU', 1, 'WCHA', 0., 0., z, 0, 'ONLY') ! U plane
      z = - wcscale * (3. / 2.) * (sdc_zpos(2) -sdc_zpos(1))
      call gspos ('WABU', 1, 'WCHA', 0., 0., z, 0, 'ONLY') ! U plane
      z = - wcscale * (5. / 2.) * (sdc_zpos(2) -sdc_zpos(1))
      call gspos ('WBAU', 1, 'WCHB', 0., 0., z, 0, 'ONLY') ! U plane
      z = - wcscale * (3. / 2.) * (sdc_zpos(2) -sdc_zpos(1))
      call gspos ('WBBU', 1, 'WCHB', 0., 0., z, 0, 'ONLY') ! U plane
*
      z = - wcscale * (1. / 2.) * (sdc_zpos(2) -sdc_zpos(1))
      call gspos ('WAAX', 1, 'WCHA', 0., 0., z, 0, 'ONLY') ! X plane
      z =   wcscale * (1. / 2.) * (sdc_zpos(2) -sdc_zpos(1))
      call gspos ('WABX', 1, 'WCHA', 0., 0., z, 0, 'ONLY') ! X plane
      z = - wcscale * (1. / 2.) * (sdc_zpos(2) -sdc_zpos(1))
      call gspos ('WBAX', 1, 'WCHB', 0., 0., z, 0, 'ONLY') ! X plane
      z =   wcscale * (1. / 2.) * (sdc_zpos(2) -sdc_zpos(1))
      call gspos ('WBBX', 1, 'WCHB', 0., 0., z, 0, 'ONLY') ! X plane
*
      z =   wcscale * (3. / 2.) * (sdc_zpos(2) -sdc_zpos(1))
      call gspos ('WAAV', 1, 'WCHA', 0., 0., z, 0, 'ONLY') ! V plane
      z =   wcscale * (5. / 2.) * (sdc_zpos(2) -sdc_zpos(1))
      call gspos ('WABV', 1, 'WCHA', 0., 0., z, 0, 'ONLY') ! V plane
      z =   wcscale * (3. / 2.) * (sdc_zpos(2) -sdc_zpos(1))
      call gspos ('WBAV', 1, 'WCHB', 0., 0., z, 0, 'ONLY') ! V plane
      z =   wcscale * (5. / 2.) * (sdc_zpos(2) -sdc_zpos(1))
      call gspos ('WBBV', 1, 'WCHB', 0., 0., z, 0, 'ONLY') ! V plane

* Now place the wire chambers in the mother volume
*
      x = sdc_xcenter(1)
      y = - sdc_ycenter(1)
      z = sdc_1_zpos
      call gspos ('WCHA', 1, 'SHUT', x, y, z, 0, 'ONLY') ! upper chamber
      x = sdc_xcenter(2)
      y = - sdc_ycenter(2)
      z = sdc_2_zpos
      call gspos ('WCHB', 1, 'SHUT', x, y, z, 0, 'ONLY') ! bottom chamber
*
* Define the individual wire cells
* See the file "displaynumbering.help" for a description of the numbering of the
* various detector elements
*
*****
*UUUU
*****
      par(1) = sdc_pitch(1) / 2./1000.        ! make the cells "wire" thin
      par(2) = uwirelength/2.
      par(3) = (sdc_zpos(2) - sdc_zpos(1))/ 2./1000. ! half width of chamber planes
      wspace = sdc_pitch(1) / SIN(sdc_alpha_angle(1))
*
* First define all the "boxes" for all the U wires in both chambers...
* Then position the U wires plane by plane
      do ichamber=1,2
        iplane = 1
        x = -(sdc_nrwire(1) + 1.) / 2. * wspace
        do isector=1,4
          if(isector.eq.3) then
            iplane = 2
            x = -(sdc_nrwire(1) + 1.) / 2. * wspace
          endif
          write(plane,'(a,a,a,a)') 'W',char(64 + ichamber),char(64+iplane),'U'
          do iwire = 1,24
            x = x + wspace
            ytemp = xwirelength/2.0
            xtemp = ytemp/tan(sdc_alpha_angle(1)) + x
            if(xtemp.gt.ywirelength/2.0) then
              xplus = ywirelength/2.0
              yplus = (xplus-x)*tan(sdc_alpha_angle(1))
            else
              xplus = xtemp
              yplus = ytemp
            endif
            ytemp = -xwirelength/2.0
            xtemp = ytemp/tan(sdc_alpha_angle(1)) + x
            if(xtemp.lt.-ywirelength/2.0) then
              xminus = -ywirelength/2.0
              yminus = (xminus-x)*tan(sdc_alpha_angle(1))
            else
              xminus = xtemp
              yminus = ytemp
            endif
            par(2) = sqrt((xplus-xminus)**2+(yplus-yminus)**2)/2.0
            write (wire,'(a,a,a,a)') char(64 + ichamber),'U',
     $           char(64 + isector),char(64 + iwire)
            call g_ugsvolu (wire, 'BOX ', DETMEDIA, par, 3, ivolu) ! U cell
            call gsatt (wire, 'SEEN', 0) ! can't see the wire cells
            call gspos (wire, 1, plane, (xminus+xplus)/2
     $           , (yminus+yplus)/2, 0., 3, 'ONLY')
          enddo
        enddo
      enddo
*

*****
*XXX
*****
      par(1) = sdc_pitch(3) / 2. /1000.       ! half width of cell
      par(2) = xwirelength/2.             ! the length of the xwirelengths
      par(3) = (sdc_zpos(4) - sdc_zpos(3))/ 2./1000. ! half width of chamber planes
      wspace = sdc_pitch(3)
*
* First define all the "boxes" for all the X wires in both chambers...
* Then position the X wires plane by plane...
*
      do ichamber=1,2
        iplane = 1
        x = -(sdc_nrwire(3) + 1.) / 2. * wspace
        do isector=1,8
          if(isector.eq.5) then
            iplane = 2
            x = -(sdc_nrwire(3) + 1.) / 2. * wspace
          endif
          write (plane,'(a,a,a,a)') 'W',char(64 + ichamber),char(64+iplane),'X'
          do iwire = 1,16
            x = x + wspace
            write (wire,'(a,a,a,a)') char(64 + ichamber),'X',
     $           char(64 + isector),char(64 + iwire)
            call g_ugsvolu (wire, 'BOX ', DETMEDIA, par, 3, ivolu) ! X cell
            call gsatt (wire, 'SEEN', 0) ! can't see the wire cells
            call gspos (wire, 1, plane, x, 0., 0., 0, 'ONLY')
          enddo
        enddo
      enddo
*
*
*****
*VVVV
*****
      par(1) = sdc_pitch(5) / 2./1000.        ! half width of cell
      par(2) = vwirelength/2.
      par(3) = (sdc_zpos(6) - sdc_zpos(5))/ 2./1000. ! half width of chamber planes
*
      wspace = sdc_pitch(5) / SIN(sdc_alpha_angle(5))        

* First define all the "boxes" for all the V wires in both chambers...
* Then position the V wires plane by plane...
      do ichamber=1,2
        iplane =1
        x = -(sdc_nrwire(5) + 1.) / 2. * wspace
        do isector=1,4
          if(isector.eq.3) then
            iplane = 2
            x = -(sdc_nrwire(5) + 1.) / 2. * wspace
          endif
          write (plane,'(a,a,a,a)') 'W',char(64 + ichamber),char(64+iplane),'V'
          do iwire = 1,24
            x = x + wspace
            ytemp = -xwirelength/2.0
            xtemp = ytemp/tan(sdc_alpha_angle(5)) + x
            if(xtemp.gt.ywirelength/2.0) then
              xplus = ywirelength/2.0
              yplus = (xplus-x)*tan(sdc_alpha_angle(5))
            else
              xplus = xtemp
              yplus = ytemp
            endif
            ytemp = xwirelength/2.0
            xtemp = ytemp/tan(sdc_alpha_angle(5)) + x
            if(xtemp.lt.-ywirelength/2.0) then
              xminus = -ywirelength/2.0
              yminus = (xminus-x)*tan(sdc_alpha_angle(5))
            else
              xminus = xtemp
              yminus = ytemp
            endif
             par(2) = sqrt((xplus-xminus)**2+(yplus-yminus)**2)/2.0
            write (wire,'(a,a,a,a)') char(64 + ichamber),'V',
     $           char(64 + isector),char(64 + iwire)
            call g_ugsvolu (wire, 'BOX ', DETMEDIA, par, 3, ivolu) ! U cell
            call gsatt (wire, 'SEEN', 0) ! can't see the wire cells
             
c            write (wire,'(a,a,a,a)') char(64 + ichamber),'V',
c     $           char(64 + isector),char(64 + iwire)
            call gspos (wire, 1, plane, (xminus+xplus)/2
     $           , (yminus+yplus)/2, 0., 4, 'ONLY')
          enddo
        enddo
      enddo
*
*
* Now define the hodoscope layers
* See the file "displaynumbering.help" for a description of the numbering of the
* various detector elements

      par(1) = sscin_1x_size * sscin_1x_nr / 2.
      par(2) = sscin_1y_size * sscin_1y_nr / 2.
      par(3) = sscin_1x_dzpos * hodoscale / 2.
      call g_ugsvolu ('HDX1', 'BOX ', DETMEDIA, par, 3, ivolu) ! X plane hodo
      call g_ugsvolu ('HDY1', 'BOX ', DETMEDIA, par, 3, ivolu) ! Y plane hodo
      call gsatt ('HDX1', 'SEEN', 0)   ! can't see the hodo box
      call gsatt ('HDY1', 'SEEN', 0)   ! can't see the hodo box
      par(1) = sscin_2x_size * sscin_2x_nr / 2.
      par(2) = sscin_2y_size * sscin_2y_nr / 2.
      par(3) = sscin_2x_dzpos * hodoscale /2.
      call g_ugsvolu ('HDX2', 'BOX ', DETMEDIA, par, 3, ivolu) ! X plane hodo
      call g_ugsvolu ('HDY2', 'BOX ', DETMEDIA, par, 3, ivolu) ! Y plane hodo
      call gsatt ('HDX2', 'SEEN', 0)   ! can't see the hodo box
      call gsatt ('HDY2', 'SEEN', 0)   ! can't see the hodo box

! box for front hodos
      par(1) = sscin_1x_size * sscin_1x_nr / 2.
      par(2) = sscin_1y_size * sscin_1y_nr / 2.
      par(3) = sscin_1x_dzpos*hodoscale + (sscin_1y_zpos-sscin_1x_zpos)/2.
      call g_ugsvolu ('HOD1', 'BOX ', DETMEDIA, par, 3, ivolu) ! hodoscope box
      call gsatt ('HOD1', 'SEEN', 0)	! can't see the hodo box

! box for back hodos
      par(1) = sscin_2x_size * sscin_2x_nr / 2.
      par(2) = sscin_2y_size * sscin_2y_nr / 2.
      par(3) = sscin_2x_dzpos*hodoscale + (sscin_2y_zpos-sscin_2x_zpos)/2.
      call g_ugsvolu ('HOD2', 'BOX ', DETMEDIA, par, 3, ivolu) ! hodoscope box
      call gsatt ('HOD2', 'SEEN', 0)	! can't see the hodo box
*                                         added by Derek
*
      x = -sscin_1x_offset
      y = sscin_1y_offset
      z = sscin_1x_zpos
      call gspos ('HOD1', 1, 'SHUT', x, y, z, 0, 'ONLY') ! lower hodo
      x = -sscin_2x_offset
      y = sscin_2y_offset
      z = sscin_2x_zpos
      call gspos ('HOD2', 1, 'SHUT', x, y, z, 0, 'ONLY') ! upper hodo

      z= -(sscin_1x_offset*hodoscale + (sscin_1y_zpos-sscin_1x_zpos))/2.
      call gspos ('HDX1', 1, 'HOD1', 0., 0., z, 0, 'ONLY') ! X plane
      call gspos ('HDY1', 1, 'HOD1', 0., 0., -z, 0, 'ONLY') ! Y plane
      z= -(sscin_2x_offset*hodoscale + (sscin_2y_zpos-sscin_2x_zpos))/2.
      call gspos ('HDX2', 1, 'HOD2', 0., 0., z, 0, 'ONLY') ! X plane
      call gspos ('HDY2', 1, 'HOD2', 0., 0., -z, 0, 'ONLY') ! Y plane

* Now define the strips for the hodoscopes

      x = (sscin_1x_nr + 1.) * sscin_1x_size / 2. ! starting loci
      do i = 1, sscin_1x_nr
        x = x - sscin_1x_size
        write (scinname,'(a,a)') 'H1X',char(64 + i)
        par(1) = sscin_1x_size / 2. ! half width of X strips
        par(2) = sscin_1y_size * sscin_1y_nr / 2.
        par(3) = sscin_1x_dzpos * hodoscale / 2. !half thickness of hodoscope in z
        call g_ugsvolu (scinname, 'BOX ', DETMEDIA, par, 3, ivolu) ! X plane hodo
        call gspos (scinname, i, 'HDX1', x, 0., 0., 0, 'ONLY')
      enddo
      y = (sscin_1y_nr + 1.) * sscin_1y_size / 2. ! starting loci
      do i = 1, sscin_1y_nr
        y = y - sscin_1y_size
        write (scinname,'(a,a)') 'H1Y',char(64 + i)
        par(1) = sscin_1x_size * sscin_1x_nr / 2.
                                        ! half width of hodoscope in x
        par(2) = sscin_1y_size / 2.	! half width of X strips
        par(3) = sscin_1y_dzpos * hodoscale / 2. !half thickness of hodoscope in z
        call g_ugsvolu (scinname, 'BOX ', DETMEDIA, par, 3, ivolu) ! Y plane hodo
        call gspos (scinname, i, 'HDY1', 0., y, 0., 0, 'ONLY')
      enddo
      x = (sscin_2x_nr + 1.) * sscin_2x_size / 2. ! starting loci
      do i = 1,sscin_2x_nr
        x = x - sscin_2x_size
        write (scinname,'(a,a)') 'H2X',char(64 + i)
        par(1) = sscin_2x_size / 2. ! half width of X strips
        par(2) = sscin_2y_size * sscin_2y_nr / 2.
        par(3) = sscin_2x_dzpos * hodoscale / 2. !half thickness of hodoscope in z
        call g_ugsvolu (scinname, 'BOX ', DETMEDIA, par, 3, ivolu) ! X plane hodo
        call gspos (scinname, i, 'HDX2', x, 0., 0., 0, 'ONLY')
      enddo
      y = (sscin_2y_nr + 1.) * sscin_2y_size / 2. ! starting loci
      do i = 1, sscin_2y_nr
        y = y - sscin_2y_size
        write (scinname,'(a,a)') 'H2Y',char(64 + i)
        par(1) = sscin_2x_size * sscin_2x_nr / 2.
        par(2) = sscin_2y_size / 2.	! half width of X strips
        par(3) = sscin_2y_dzpos * hodoscale / 2. !half thickness of hodoscope in z
        call g_ugsvolu (scinname, 'BOX ', DETMEDIA, par, 3, ivolu) ! Y plane hodo
        call gspos (scinname, i, 'HDY2', 0., y, 0., 0, 'ONLY')
      enddo

* Now define the shower detector
* See the file "displaynumbering.help" for a description of the numbering of the
* various detector elements

! half width of the shower in x
      par(1) = smax_cal_rows * scal_block_zsize / 2.
! half width of the shower in y
      par(2) = scal_block_ysize / 2.
! half height of the shower detector
      par(3) = smax_cal_columns * scal_block_xsize / 2.
      call g_ugsvolu ('SHOW', 'BOX ', DETMEDIA, par, 3, ivolu)

!for the x offset, we take the center of the top and bottom blocks
!This assumes that all the blocks are
!the same heighth and width as scal_1pr
      x = -(scal_block_xc(1) + scal_block_xc(smax_cal_rows))/2
      y = scal_block_yc(1)
      z = scal_1pr_zpos + smax_cal_columns*scal_block_xsize/2.
      call gspos ('SHOW', 1, 'SHUT', x, y, z, 0, 'ONLY')
      call gsatt ('SHOW','SEEN',0)

      par(1) = smax_cal_rows * scal_block_zsize / 2.! half width of shower in x
      par(2) = scal_block_ysize / 2.    ! half width of the shower in y
      par(3) = scal_block_xsize / 2.    ! half height of the shower detector

      z = -(smax_cal_columns + 1.) / 2. * scal_block_xsize
      do ilayer =1,smax_cal_columns
        z = z + scal_block_xsize

        write (layername,'(a,i1)') 'LAY',ilayer

        par(1) = smax_cal_rows * scal_block_zsize / 2. ! half width of shower
        call g_ugsvolu (layername, 'BOX ', DETMEDIA, par, 3, ivolu)
        call gspos(layername, 1, 'SHOW', 0., 0., z, 0, 'ONLY')

        par(1) = scal_block_zsize / 2.	! half width of a block
        x =  (smax_cal_rows - 1.) / 2. * scal_block_zsize
        do irow = 1, smax_cal_rows
          write (blockname,'(a,i1,a)') 'BL',ilayer,char(64 + irow)
          call g_ugsvolu (blockname, 'BOX ', DETMEDIA, par, 3, ivolu)
          call gspos(blockname, 1, layername, x, 0., 0., 0, 'ONLY')
          x = x - scal_block_zsize
        enddo
      enddo
*

      end
