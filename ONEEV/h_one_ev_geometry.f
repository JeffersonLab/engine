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
* $Log$
* Revision 1.3  1995/05/22 18:58:03  cdaq
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
      include 'hms_one_ev.par'

      integer HHUTMEDIA                 ! non-sensitive tracking media
      integer DETMEDIA                  ! sensitive tracking media
      parameter (HHUTMEDIA = 1, DETMEDIA = 2)
      real*4 hodoscale
      parameter (hodoscale = 3.)

      integer ivolu			! internal volume number
      real par(10)			! geometry parameters
      real x, y, z			! offset position for placement of dets
      integer i                         ! index variable

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

* Now define the wire chambers as a collection of 6 planes

      par(1) = hdc_nrwire(1)*hdc_pitch(1)/ 2. ! half width of chamber planes
      par(2) = hdc_nrwire(2)*hdc_pitch(2)/ 2. ! half width of chamber planes
      par(3) = (hdc_zpos(2) - hdc_zpos(1))/ 2. ! half width of chamber planes
      call g_ugsvolu ('WCXP', 'BOX ', DETMEDIA, par, 3, ivolu) ! X plane
      call g_ugsvolu ('WCYP', 'BOX ', DETMEDIA, par, 3, ivolu) ! Y plane
      call g_ugsvolu ('WCUP', 'BOX ', DETMEDIA, par, 3, ivolu) ! U plane
      call g_ugsvolu ('WCVP', 'BOX ', DETMEDIA, par, 3, ivolu) ! V plane

! make a volume for 6 planes
      par(3) = (6./5. * (hdc_zpos(6) - hdc_zpos(1))) / 2.
      call g_ugsvolu ('WCHA', 'BOX ', DETMEDIA, par, 3, ivolu) ! Wire chamber

* Now place the planes within the wire chamber

      z = -(5. / 2.) * (hdc_zpos(2) -hdc_zpos(1))
      call gspos ('WCXP', 1, 'WCHA', 0., 0., z, 0, 'ONLY') ! X plane
      z = -(3. / 2.) * (hdc_zpos(2) -hdc_zpos(1))
      call gspos ('WCYP', 1, 'WCHA', 0., 0., z, 0, 'ONLY') ! Y plane
      z = -(1. / 2.) * (hdc_zpos(2) -hdc_zpos(1))
      call gspos ('WCUP', 1, 'WCHA', 0., 0., z, 0, 'ONLY') ! U plane
      z =  (1. / 2.) * (hdc_zpos(2) -hdc_zpos(1))
      call gspos ('WCVP', 1, 'WCHA', 0., 0., z, 0, 'ONLY') ! V plane
      z =  (3. / 2.) * (hdc_zpos(2) -hdc_zpos(1))
      call gspos ('WCYP', 2, 'WCHA', 0., 0., z, 0, 'ONLY') ! Y plane
      z =  (5. / 2.) * (hdc_zpos(2) -hdc_zpos(1))
      call gspos ('WCXP', 2, 'WCHA', 0., 0., z, 0, 'ONLY') ! X plane

* Now place the wire chambers in the mother volume

      x = UPPER_CHAMBER_X_OFFSET
      y = UPPER_CHAMBER_Y_OFFSET
      z = hdc_1_zpos
      call gspos ('WCHA', 1, 'HHUT', x, y, z, 0, 'ONLY') ! upper chamber
      x = LOWER_CHAMBER_X_OFFSET
      y = LOWER_CHAMBER_Y_OFFSET
      z = hdc_2_zpos
      call gspos ('WCHA', 2, 'HHUT', x, y, z, 0, 'ONLY') ! bottom chamber
*
* Define the individual wire cells
*
      par(1) = hdc_pitch(1) / 2.        ! half width of cell
      par(2) = hdc_nrwire(2)*hdc_pitch(2)/ 2. ! half width of chamber planes
      par(3) = (hdc_zpos(2) - hdc_zpos(1))/ 2. ! half width of chamber planes
      call g_ugsvolu ('WCWX', 'BOX ', DETMEDIA, par, 3, ivolu) ! X cell
      call gsatt ('WCWX', 'SEEN', 0)	! can't see the wire cells
      x = -(hdc_nrwire(1) + 1.) / 2. * (par(1) * 2.)
      do i = 1, hdc_nrwire(1)
        x = x + par(1) * 2.
        call gspos ('WCWX', i, 'WCXP', x, 0., 0., 0, 'ONLY')
      enddo

      par(1) = hdc_nrwire(1)*hdc_pitch(1)/ 2. ! half width of chamber planes
      par(2) = hdc_pitch(2) / 2.        ! half width of cell
      par(3) = (hdc_zpos(3) - hdc_zpos(2))/ 2. ! half width of chamber planes
      call g_ugsvolu ('WCWY', 'BOX ', DETMEDIA, par, 3, ivolu) ! Y cell
      call gsatt ('WCWY', 'SEEN', 0)	! can't see the wire cells
      y = -(hdc_nrwire(2) + 1.) / 2. * (par(2) * 2.)
      do i = 1, hdc_nrwire(2)
        y = y + par(2) * 2.
        call gspos ('WCWY', i, 'WCYP', 0., y, 0., 0, 'ONLY')
      enddo

      par(1) = hdc_pitch(3) / 2.        ! half width of cell
      par(2) = hdc_nrwire(2)*hdc_pitch(2)/ 2. ! half width of chamber planes
      par(3) = (hdc_zpos(4) - hdc_zpos(3))/ 2. ! half width of chamber planes
      call g_ugsvolu ('WCWU', 'BOX ', DETMEDIA, par, 3, ivolu) ! U cell
      call gsatt ('WCWU', 'SEEN', 0)	! can't see the wire cells
      x = -(hdc_nrwire(3) + 1.) / 2. * (par(1) * 2.)
      do i = 1, hdc_nrwire(3)
        x = x + par(1) * 2.
        call gspos ('WCWU', i, 'WCUP', x, 0., 0., 3, 'ONLY')
      enddo

      par(1) = hdc_pitch(4) / 2.        ! half width of cell
      par(2) = hdc_nrwire(2)*hdc_pitch(2)/ 2. ! half width of chamber planes
      par(3) = (hdc_zpos(5) - hdc_zpos(4))/ 2. ! half width of chamber planes
      call g_ugsvolu ('WCWV', 'BOX ', DETMEDIA, par, 3, ivolu) ! V cell
      call gsatt ('WCWV', 'SEEN', 0)	! can't see the wire cells
      x = -(hdc_nrwire(4) + 1.) / 2. * (par(1) * 2.)
      do i = 1, hdc_nrwire(4)
        x = x + par(1) * 2.
        call gspos ('WCWV', i, 'WCVP', x, 0., 0., 4, 'ONLY')
      enddo

* Now define the hodoscope layers

      par(1) = hscin_1x_size * HODO_X_PADDLES / 2.
                                        ! half width of hodoscope in x
      par(2) = hscin_1y_size / 2.	! half width of X strips
      par(3) = HODO_THICKNESS * hodoscale / 2. !half thickness of hodoscope in z
      call g_ugsvolu ('HOYS', 'BOX ', DETMEDIA, par, 3, ivolu) ! Y plane hodo
        
      par(1) = hscin_1x_size / 2.	! half width of X strips
      par(2) = hscin_1y_size * HODO_Y_PADDLES / 2.
                                        ! half width of hodoscope in y
      par(3) = HODO_THICKNESS * hodoscale / 2. !half thickness of hodoscope in z
      call g_ugsvolu ('HOXS', 'BOX ', DETMEDIA, par, 3, ivolu) ! X plane hodo

! half width of hodoscope in x
      par(1) = hscin_1x_size * HODO_X_PADDLES / 2.
! half width of hodoscope in y
      par(2) = hscin_1y_size * HODO_Y_PADDLES / 2.
! half thickness of hodoscope in z, times 2 because of stagger
      par(3) = HODO_THICKNESS * hodoscale
      call g_ugsvolu ('HODX', 'BOX ', DETMEDIA, par, 3, ivolu) ! X plane hodo
      call g_ugsvolu ('HODY', 'BOX ', DETMEDIA, par, 3, ivolu) ! Y plane hodo

! box for hodo
      par(3) = HODO_THICKNESS*hodoscale + (hscin_1y_zpos-hscin_1x_zpos)/2.
      call g_ugsvolu ('HODO', 'BOX ', DETMEDIA, par, 3, ivolu) ! hodoscope box

      x = HODO_LOWER_X_OFFSET
      y = HODO_LOWER_Y_OFFSET
      z = hscin_1x_zpos
      call gspos ('HODO', 1, 'HHUT', x, y, z, 0, 'ONLY') ! lower hodo
      x = HODO_UPPER_X_OFFSET
      y = HODO_UPPER_Y_OFFSET
      z = hscin_2x_zpos
      call gspos ('HODO', 2, 'HHUT', x, y, z, 0, 'ONLY') ! upper hodo

      z= -(HODO_THICKNESS*hodoscale + (hscin_1y_zpos-hscin_1x_zpos))/2.
      call gspos ('HODX', 1, 'HODO', 0., 0., z, 0, 'ONLY') ! X plane
      call gspos ('HODY', 1, 'HODO', 0., 0., -z, 0, 'ONLY') ! Y plane


* Now define the strips for the hodoscopes

      x = (HODO_X_PADDLES + 1.) * hscin_1x_size / 2. ! starting loci
      do i = 1, HODO_X_PADDLES
        x = x - hscin_1x_size
        call gspos ('HOXS', i, 'HODX', x, 0., 0., 0, 'ONLY')
      enddo

      y = (HODO_Y_PADDLES + 1.) * hscin_1y_size / 2. ! starting loci
      do i = 1, HODO_Y_PADDLES
        y = y - hscin_1y_size
        call gspos ('HOYS', i, 'HODY', 0., y, 0., 0, 'ONLY')
      enddo

* Now define the shower detector

! half width of the shower in x
      par(1) = hmax_cal_rows * hcal_block_zsize / 2.
! half width of the shower in y
      par(2) = hcal_block_ysize / 2.
! half height of the shower detector
      par(3) = hmax_cal_columns * hcal_block_xsize / 2.
      call g_ugsvolu ('SHOW', 'BOX ', DETMEDIA, par, 3, ivolu)

      x = SHOWER_X_OFFSET 
      y = SHOWER_Y_OFFSET
      z = hcal_1pr_zpos + hmax_cal_columns*hcal_block_xsize/2.
      call gspos ('SHOW', 1, 'HHUT', x, y, z, 0, 'ONLY')

! half width of the shower in x
      par(1) = hmax_cal_rows * hcal_block_zsize / 2.
! half width of the shower in y
      par(2) = hcal_block_ysize / 2.
! half height of the shower detector
      par(3) = hcal_block_xsize / 2.
      call g_ugsvolu ('LAYE', 'BOX ', DETMEDIA, par, 3, ivolu)

      z = -(hmax_cal_columns + 1.) / 2. * hcal_block_xsize
      do i = 1, hmax_cal_columns
        z = z + hcal_block_xsize
        call gspos('LAYE', i, 'SHOW', 0., 0., z, 0, 'ONLY')
      enddo

      par(1) = hcal_block_zsize / 2.	! half width of a block
      par(2) = hcal_block_ysize / 2.	! half length of a block
      par(3) = hcal_block_xsize / 2.	! half height of a block
      call g_ugsvolu ('BLOC', 'BOX ', DETMEDIA, par, 3, ivolu)

      x = (hmax_cal_rows + 1.) / 2. * hcal_block_zsize
      do i = 1, hmax_cal_rows
        x = x - hcal_block_zsize
        call gspos('BLOC', i, 'LAYE', x, 0., 0., 0, 'ONLY')
      enddo

      end
