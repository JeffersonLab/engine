      subroutine h_one_ev_geometry
*
* This routine will get the detector position and size information from CTP,
* then use this information for defining the different GEANT geometry structures
*
* August, 1994, Pat Welch, Oregon State University, tpw@physics.orst.edu
*
* Note: Subdivided volumes won't work for doing coordinate transforms.  Or
*	at least I didn't see a method around them.  So I have defined all
*	the subvolumes explicitly. (TPW)
* $Log: h_one_ev_geometry.f,v $
* Revision 1.8  1996/11/22 15:36:37  saw
* (SAW) Some code cleanup
*
* Revision 1.7  1996/04/30 14:09:54  saw
* (JRA) Some new code
*
* Revision 1.6  1996/01/17 16:35:37  cdaq
* (DVW) Tweak hodoscale
*
* Revision 1.5  1995/10/06 18:39:36  cdaq
* (DVW) Changed to ctp geometry variables and eliminated call to h_one_ev.par.
*
* Revision 1.4  1995/09/18 14:35:22  cdaq
* (DVW) Improvements
*
* Revision 1.3  1995/05/22  18:58:03  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts
*
* Revision 1.2  1995/01/27  19:31:37  cdaq
* (SAW) Change file names to be hms specific.
*
c Revision 1.1  1995/01/10  18:43:44  cdaq
c Initial revision
c
*
      implicit none

      include 'hms_data_structures.cmn'
      include 'hms_geometry.cmn'
      include 'hms_calorimeter.cmn'
      include 'hms_scin_parms.cmn'

      real*4    HHUT_WIDTH,HHUT_HEIGHT
      parameter (HHUT_WIDTH = 100.)     ! full width of the det. hut
      parameter (HHUT_HEIGHT = 800.)    ! full height of the det. hut

      integer HHUTMEDIA                 ! non-sensitive tracking media
      integer DETMEDIA                  ! sensitive tracking media
      parameter (HHUTMEDIA = 1, DETMEDIA = 2)
      real*4 hodoscale
      parameter (hodoscale = 2.)
      real*4 wcscale
      parameter (wcscale = 1.)
      real*4 xwirelength
      real*4 ywirelength
      real*4 uwirelength
      real*4 vwirelength

      real*4 numxwires           ! add 1 to the number of x, u, and v wires
      real*4 numywires           ! for ease in looping over the wires...
      real*4 numuwires
      real*4 numvwires

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
      real*4 raddeg
      parameter (raddeg = 3.14159265/180.)


* First define two general media that everything is made of
* one is insensitive, and the other is sensitive

      call gstmed (HHUTMEDIA, 'air', 15, 0, 0,0.,20.,1.,0.5,1.,1.,0,0)
      call gstmed (DETMEDIA, 'det', 15, 1, 0,0.,20.,1.,0.5,1.,1.,0,0)

* Now define the mother volume that the detectors sit in

      par(1) = HHUT_WIDTH / 2.          ! half width in x of mother volume
      par(2) = HHUT_WIDTH / 2.          ! half width in y of mother volume
      par(3) = HHUT_HEIGHT / 2.         ! half height in z of mother volume
      call g_ugsvolu ('HHUT', 'BOX ', HHUTMEDIA, par, 3, ivolu)
      call gsatt ('HHUT', 'SEEN', 0)	! can't see the hut

* Get the number of wires from the ctp file, and add one to the x, u, and v for
* ease in looping over the wires
* See the file "displaynumbering.help" for a description of the numbering of the
* various detector elements
*

      numxwires = hdc_nrwire(1) + 1.
      numywires = hdc_nrwire(2)
      numuwires = hdc_nrwire(3) + 1.
      numvwires = hdc_nrwire(4) + 1.
      xwirelength = numywires*hdc_pitch(2)
      ywirelength = numxwires * hdc_pitch(1)
      uwirelength = xwirelength / SIN(hdc_alpha_angle(3))
      vwirelength = xwirelength / SIN(hdc_alpha_angle(4))

* Now define the wire chambers as a collection of 6 planes

      par(1) = numxwires*hdc_pitch(1)/ 2. ! half width of chamber planes
      par(2) = numywires*hdc_pitch(2)/ 2. ! half width of chamber planes
      par(3) = wcscale * (hdc_zpos(2) - hdc_zpos(1))/ 2. 
      do ichamber = 1,2
         do iplane = 1,2
            write(planename,'(a,a,a,a)') "W",char(64+ichamber),char(64+iplane),"X"
            call g_ugsvolu (planename, 'BOX ', DETMEDIA, par, 3, ivolu) 
            write(planename,'(a,a,a,a)') "W",char(64+ichamber),char(64+iplane),"Y"
            call g_ugsvolu (planename, 'BOX ', DETMEDIA, par, 3, ivolu) 
            write(planename,'(a,a,a,a)') "W",char(64+ichamber),char(64+iplane),"U"
            call g_ugsvolu (planename, 'BOX ', DETMEDIA, par, 3, ivolu) 
            write(planename,'(a,a,a,a)') "W",char(64+ichamber),char(64+iplane),"V"
            call g_ugsvolu (planename, 'BOX ', DETMEDIA, par, 3, ivolu) 
         enddo
      enddo

* make a volumes for 6 planes
      par(3) = wcscale * (6./5. * (hdc_zpos(6) - hdc_zpos(1))) / 2.
      call g_ugsvolu ('WCHA', 'BOX ', DETMEDIA, par, 3, ivolu) ! Wire chamber
      call g_ugsvolu ('WCHB', 'BOX ', DETMEDIA, par, 3, ivolu) ! Wire chamber

* Now place the planes within the wire chamber, start with X

      z = -(5. / 2.) * (hdc_zpos(2) -hdc_zpos(1))
      call gspos ('WAAX', 1, 'WCHA', 0., 0., z, 0, 'ONLY') ! X plane
      z =  (5. / 2.) * (hdc_zpos(2) -hdc_zpos(1))
      call gspos ('WABX', 1, 'WCHA', 0., 0., z, 0, 'ONLY') ! X plane
      z = -(5. / 2.) * (hdc_zpos(2) -hdc_zpos(1))
      call gspos ('WBAX', 1, 'WCHB', 0., 0., z, 0, 'ONLY') ! X plane
      z =  (5. / 2.) * (hdc_zpos(2) -hdc_zpos(1))
      call gspos ('WBBX', 1, 'WCHB', 0., 0., z, 0, 'ONLY') ! X plane

* Now place the planes within the wire chamber, now the Y's

      z = -(3. / 2.) * (hdc_zpos(2) -hdc_zpos(1))
      call gspos ('WAAY', 1, 'WCHA', 0., 0., z, 0, 'ONLY') ! Y plane
      z =  (3. / 2.) * (hdc_zpos(2) -hdc_zpos(1))
      call gspos ('WABY', 1, 'WCHA', 0., 0., z, 0, 'ONLY') ! Y plane
      z = -(3. / 2.) * (hdc_zpos(2) -hdc_zpos(1))
      call gspos ('WBAY', 1, 'WCHB', 0., 0., z, 0, 'ONLY') ! Y plane
      z =  (3. / 2.) * (hdc_zpos(2) -hdc_zpos(1))
      call gspos ('WBBY', 1, 'WCHB', 0., 0., z, 0, 'ONLY') ! Y plane

* Now place the planes within the wire chamber, now the U's

      z = -(1. / 2.) * (hdc_zpos(2) -hdc_zpos(1))
      call gspos ('WAAU', 1, 'WCHA', 0., 0., z, 0, 'ONLY') ! U plane
      z = -(1. / 2.) * (hdc_zpos(2) -hdc_zpos(1))
      call gspos ('WBAU', 1, 'WCHB', 0., 0., z, 0, 'ONLY') ! U plane

* Now place the planes within the wire chamber, now the V's

      z =  (1. / 2.) * (hdc_zpos(2) -hdc_zpos(1))
      call gspos ('WAAV', 1, 'WCHA', 0., 0., z, 0, 'ONLY') ! V plane
      z =  (1. / 2.) * (hdc_zpos(2) -hdc_zpos(1))
      call gspos ('WBAV', 1, 'WCHB', 0., 0., z, 0, 'ONLY') ! V plane


* Now place the wire chambers in the mother volume

      x = hdc_xcenter(1)
      y = - hdc_ycenter(1)
      z = hdc_1_zpos
      call gspos ('WCHA', 1, 'HHUT', x, y, z, 0, 'ONLY') ! upper chamber
      x = hdc_xcenter(2)
      y = - hdc_ycenter(2)
       z = hdc_2_zpos
      call gspos ('WCHB', 1, 'HHUT', x, y, z, 0, 'ONLY') ! bottom chamber
*
* Define the individual wire cells
* See the file "displaynumbering.help" for a description of the numbering of the
* various detector elements
*
*****
*XXXX
*****
      par(1) = hdc_pitch(1) / 2./1000.        ! make the cells "wire" thin
      par(2) = xwirelength/ 2. ! half width of chamber planes
      par(3) = (hdc_zpos(2) - hdc_zpos(1))/ 2. /1000. ! half width of chamber planes

      wspace = hdc_pitch(1)
*
* Now position the X wires plane by plane
*
      do ichamber=1,2
        iplane = 1
        x = - (numxwires) /2. * wspace
        do isector=1,12
          if (isector.eq.7)  then
            iplane = 2
            x = - (numxwires) /2. * wspace
          endif
          write (plane,'(a,a,a,a)') 'W',char(64+ichamber),char(64+iplane),'X'
          do iwire=1,19
            x = x + wspace
            write (wire,'(a,a,a,a)') char(64+ichamber),"X",
     $           char(64 + isector),char(64 + iwire)
            call g_ugsvolu (wire, 'BOX ', DETMEDIA, par, 3, ivolu) ! X cell
            call gsatt (wire, 'SEEN', 0) ! can't see the wire cells
            call gspos (wire, 1, plane, x, 0., 0., 0, 'ONLY')
          enddo
        enddo
      enddo
*

*****
*YYYY
*****
      par(1) = ywirelength/ 2. ! half width of chamber planes
      par(2) = hdc_pitch(2) / 2. / 1000.       ! half width of cell
      par(3) = (hdc_zpos(3) - hdc_zpos(2))/ 2./1000. ! half width of chamber planes
      wspace = hdc_pitch(2)
*
* Now position the Y wires plane by plane
*
      do ichamber=1,2
        iplane = 1
        y = -(numywires + 1.) / 2. * wspace
        do isector=1,4
          if(isector.eq.3) then
            iplane = 2
            y = -(numywires + 1.) / 2. * wspace
          endif
          write (plane,'(a,a,a,a)') 'W',char(64+ichamber),char(64+iplane),'Y'
          do iwire=1,26
            y = y + wspace
            write (wire,'(a,a,a,a)') char(64+ichamber),"Y",
     $           char(64 + isector),char(64 + iwire)
            call g_ugsvolu (wire, 'BOX ', DETMEDIA, par, 3, ivolu) ! Y cell
            call gsatt (wire, 'SEEN', 0) ! can't see the wire cells
            call gspos (wire, 1, plane, 0., y, 0., 0, 'ONLY')
          enddo
         enddo
      enddo
*
*****
*UUUU
*****
      par(1) = hdc_pitch(1) / 2./1000.        ! make the cells "wire" thin
      par(2) = uwirelength/2.
      par(3) = (hdc_zpos(2) - hdc_zpos(1))/ 2. /1000. ! half width of chamber planes
      wspace = hdc_pitch(3) / SIN (hdc_alpha_angle(3))

* Now position the U wires plane by plane...
      do ichamber=1,2
        x = -(numuwires) / 2. * wspace
        write (plane,'(a,a,a)') "W",char(64+ichamber),"AU"
        do isector=1,6
          do iwire=1,18
            x = x + wspace
            write (wire,'(a,a,a,a)') char(64+ichamber),"U",
     $           char(64 + isector),char(64 + iwire)
            call g_ugsvolu (wire, 'BOX ', DETMEDIA, par, 3, ivolu) ! U cell
            call gsatt (wire, 'SEEN', 0) ! can't see the wire cells
            call gspos (wire, 1, plane, x, 0.0 , 0., 4, 'ONLY')
          enddo
        enddo
      enddo
*****
*VVVV
*****
      par(1) = hdc_pitch(1) / 2./1000.        ! make the cells "wire" thin
      par(2) = vwirelength/2.
      par(3) = (hdc_zpos(2) - hdc_zpos(1))/ 2. /1000. ! half width of chamber planes

      wspace = hdc_pitch(4) / SIN (hdc_alpha_angle(4))

* Now position the V wires plane by plane...
      do ichamber=1,2
        x = - (numvwires) / 2. * wspace
        write (plane,'(a,a,a)') "W",char(64+ichamber),"AV"
        do isector=1,6
          do iwire=1,18
            x = x + wspace
            write (wire,'(a,a,a,a)') char(64+ichamber),"V",
     $           char(64 + isector),char(64 + iwire)
            call g_ugsvolu (wire, 'BOX ', DETMEDIA, par, 3, ivolu) ! V cell
            call gsatt (wire, 'SEEN', 0) ! can't see the wire cells
            call gspos (wire, 1, plane, x, 0., 0., 3, 'ONLY')
          enddo
        enddo
      enddo
*
* Now define the hodoscope layers
* See the file "displaynumbering.help" for a description of the numbering of the
* various detector elements

      par(1) = hscin_1x_size * hscin_1x_nr / 2.    ! half width of X strips
      par(2) = hscin_1y_size * hscin_1y_nr / 2.    ! half width of Y strips
      par(3) = hscin_1x_dzpos * hodoscale / 2. !half thickness of hodoscope in z
      call g_ugsvolu ('HDX1', 'BOX ', DETMEDIA, par, 3, ivolu) ! X plane hodo
      call g_ugsvolu ('HDY1', 'BOX ', DETMEDIA, par, 3, ivolu) ! Y plane hodo

      call gsatt ('HDX1', 'SEEN', 0)   ! can't see the hodo box
      call gsatt ('HDY1', 'SEEN', 0)   ! can't see the hodo box
      par(1) = hscin_2x_size * hscin_2x_nr / 2.    ! half width of X strips
      par(2) = hscin_2y_size * hscin_2y_nr / 2.    ! half width of Y strips
      par(3) = hscin_2x_dzpos * hodoscale / 2. !half thickness of hodoscope in z
      call g_ugsvolu ('HDX2', 'BOX ', DETMEDIA, par, 3, ivolu) ! X plane hodo
      call g_ugsvolu ('HDY2', 'BOX ', DETMEDIA, par, 3, ivolu) ! Y plane hodo
      call gsatt ('HDX2', 'SEEN', 0)   ! can't see the hodo box
      call gsatt ('HDY2', 'SEEN', 0)   ! can't see the hodo box
     
! box for front hodos
      par(1) = hscin_1x_size * hscin_1x_nr / 2.
      par(2) = hscin_1y_size * hscin_1y_nr / 2.
      par(3) = hscin_1x_dzpos*hodoscale + (hscin_1y_zpos-hscin_1x_zpos)/2.
      call g_ugsvolu ('HOD1', 'BOX ', DETMEDIA, par, 3, ivolu) ! hodoscope box
      call gsatt ('HOD1', 'SEEN', 0)	! can't see the hodo box
*                                         added by Derek
! box for back hodos
      par(1) = hscin_2x_size * hscin_2x_nr / 2.
      par(2) = hscin_2y_size * hscin_2y_nr / 2.
      par(3) = hscin_2x_dzpos*hodoscale + (hscin_2y_zpos-hscin_2x_zpos)/2.
      call g_ugsvolu ('HOD2', 'BOX ', DETMEDIA, par, 3, ivolu) ! hodoscope box
      call gsatt ('HOD2', 'SEEN', 0)	! can't see the hodo box

      x = -hscin_1x_offset
      y = hscin_1y_offset
      z = hscin_1x_zpos
      call gspos ('HOD1', 1, 'HHUT', x, y, z, 0, 'ONLY') ! lower hodo
      x = -hscin_2x_offset
      y = hscin_2y_offset
      z = hscin_2x_zpos
      call gspos ('HOD2', 1, 'HHUT', x, y, z, 0, 'ONLY') ! upper hodo

      z= -(hscin_1x_dzpos*hodoscale + (hscin_1y_zpos-hscin_1x_zpos))/2.
      call gspos ('HDX1', 1, 'HOD1', 0., 0., z, 0, 'ONLY') ! X plane
      call gspos ('HDY1', 1, 'HOD1', 0., 0., -z, 0, 'ONLY') ! Y plane
      z= -(hscin_2x_dzpos*hodoscale + (hscin_2y_zpos-hscin_2x_zpos))/2.
      call gspos ('HDX2', 1, 'HOD2', 0., 0., z, 0, 'ONLY') ! X plane
      call gspos ('HDY2', 1, 'HOD2', 0., 0., -z, 0, 'ONLY') ! Y plane

* Now define the strips for the hodoscopes

      x = (hscin_1x_nr + 1.) * hscin_1x_size / 2. ! starting loci
      do i = 1, hscin_1x_nr
         x = x - hscin_1x_size
         write (scinname,'(a,a)') "H1X",char(64 + i)
         par(1) = hscin_1x_size / 2.  !half width of X strips
         par(2) = hscin_1y_size * hscin_1y_nr / 2.
         par(3) = hscin_1x_dzpos * hodoscale / 2.
         call g_ugsvolu (scinname, 'BOX ', DETMEDIA, par, 3, ivolu)
         call gspos (scinname, 1, 'HDX1', x, 0., 0., 0, 'ONLY')
      enddo
      y = (hscin_1y_nr + 1.) * hscin_1y_size / 2. ! starting loci
      do i = 1, hscin_1y_nr
         y = y - hscin_1y_size
         write (scinname,'(a,a)') "H1Y",char(64 + i)
         par(1) = hscin_1x_size * hscin_1x_nr / 2.
         par(2) = hscin_1y_size / 2.  !half width of X strips
         par(3) = hscin_1y_dzpos * hodoscale / 2.
         call g_ugsvolu (scinname, 'BOX ', DETMEDIA, par, 3, ivolu)
         call gspos (scinname, 1, 'HDY1', 0., y, 0., 0, 'ONLY')
      enddo
      x = (hscin_2x_nr + 1.) * hscin_2x_size / 2. ! starting loci
      do i = 1, hscin_2x_nr
         x = x - hscin_2x_size
         write (scinname,'(a,a)') "H2X",char(64 + i)
         par(1) = hscin_2x_size / 2.  !half width of X strips
         par(2) = hscin_2y_size * hscin_2y_nr / 2.
         par(3) = hscin_2x_dzpos * hodoscale / 2.
         call g_ugsvolu (scinname, 'BOX ', DETMEDIA, par, 3, ivolu)
         call gspos (scinname, 1, 'HDX2', x, 0., 0., 0, 'ONLY')
      enddo
      y = (hscin_2y_nr + 1.) * hscin_2y_size / 2. ! starting loci
      do i = 1, hscin_2y_nr
         y = y - hscin_2y_size
         write (scinname,'(a,a)') "H2Y",char(64 + i)
         par(1) = hscin_2x_size * hscin_2x_nr / 2.
         par(2) = hscin_2y_size / 2.  !half width of X strips
         par(3) = hscin_2y_dzpos * hodoscale / 2.
         call g_ugsvolu (scinname, 'BOX ', DETMEDIA, par, 3, ivolu)
         call gspos (scinname, 1, 'HDY2', 0., y, 0., 0, 'ONLY')
      enddo

* Now define the shower detector
* See the file "displaynumbering.help" for a description of the numbering of the
* various detector elements

! half width of the shower in x
      par(1) = hmax_cal_rows * hcal_block_zsize / 2.
! half width of the shower in y
      par(2) = hcal_block_ysize / 2.
! half height of the shower detector
      par(3) = hmax_cal_columns * hcal_block_xsize / 2.
      call g_ugsvolu ('SHOW', 'BOX ', DETMEDIA, par, 3, ivolu)

!for the x offset, we take the center of the top and bottom blocks
!This assumes that all the blocks are
!the same heighth and width as scal_1pr

      x = -(hcal_block_xc(1) + hcal_block_xc(hmax_cal_rows))/2
      y = hcal_block_yc(1)
      z = hcal_1pr_zpos + hmax_cal_columns*hcal_block_xsize/2.
      call gspos ('SHOW', 1, 'HHUT', x, y, z, 0, 'ONLY')
      call gsatt ('SHOW', 'SEEN',0)


      par(1) = hmax_cal_rows * hcal_block_zsize / 2. ! half width of shower in x
      par(2) = hcal_block_ysize / 2.    ! half width of the shower in y
      par(3) = hcal_block_xsize / 2.    ! half height of the shower detector

      z = -(hmax_cal_columns + 1.) / 2. * hcal_block_xsize
      do ilayer = 1,hmax_cal_columns
        z = z + hcal_block_xsize

        write (layername,'(a,i1)') 'LAY',ilayer

        par(1) = hmax_cal_rows * hcal_block_zsize / 2.
        call g_ugsvolu (layername, 'BOX ', DETMEDIA, par, 3, ivolu)
        call gspos(layername, 1, 'SHOW', 0., 0., z, 0, 'ONLY')

        par(1) = hcal_block_zsize / 2.  ! half width of a block
        x = (hmax_cal_rows - 1.) / 2. * hcal_block_zsize
        do irow = 1, hmax_cal_rows
          write (blockname,'(a,i1,a)') 'BL',ilayer,char(64 + irow)
          call g_ugsvolu (blockname, 'BOX ', DETMEDIA, par, 3, ivolu)
          call gspos(blockname,1,layername, x, 0., 0., 0, 'ONLY')
          x = x - hcal_block_zsize
        enddo
      enddo
        
      end
