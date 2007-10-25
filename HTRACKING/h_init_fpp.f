      SUBROUTINE h_init_fpp(ABORT,err)
*--------------------------------------------------------
*    Hall C  HMS Focal Plane Polarimeter Code
*
*  Purpose: initialize FPP parameters and constants
* 
*  Created by Frank R. Wesselmann,  February 2004
*
*--------------------------------------------------------

      IMPLICIT NONE

      include 'gen_detectorids.par'
      include 'gen_decode_common.cmn'
      INCLUDE 'hms_data_structures.cmn'
      INCLUDE 'hms_fpp_params.cmn'
      INCLUDE 'hms_geometry.cmn'

      INCLUDE 'hms_fpp_params.dte'

      character*10 here
      parameter (here= 'h_init_fpp')

      logical ABORT
      character*(*) err

      real*4 pi
      parameter (pi=3.1415926535)

      integer*4 iSet, iChamber, iLayer, iPlane, iWire
      integer*4 ilab, iloc, ii, wlo, whi, wstep, sign
      real*4 sinalpha, sinbeta, singamma
      real*4 cosalpha, cosbeta, cosgamma
      real*4 matrix(3,3)
      real*4 Imatrix(3,3)

      ABORT= .FALSE.
      err= ' '


*     * re-map per-plane parameters into (set,chamber,layer) set
      do iset=1, H_FPP_N_DCSETS
       do ichamber=1, H_FPP_N_DCINSET
         do ilayer=1, H_FPP_N_DCLAYERS

*          * for these first quantities, the values for all chamber sets are identical, thus
*          * they got defined only for one set and get duplicated for all (two) sets (pairs)
           iPlane = H_FPP_N_DCLAYERS * (iChamber-1) + ilayer

           HFPP_spacing(iset,ichamber,ilayer)	  = inplanespacing(iPlane)
	   HFPP_layeroffset(iset,ichamber,ilayer) = planeoffset(iPlane)
	   HFPP_layerZ(iset,ichamber,ilayer)	  = planeZ(iPlane)

           HFPP_direction(iset,ichamber,ilayer,1) = cos( planeangle(iPlane) *pi/180.)
           HFPP_direction(iset,ichamber,ilayer,2) = sin( planeangle(iPlane) *pi/180.)
*          * when we do tracking, we work in 2D only, u and z
*          * u is part of (u,v) which is defined by a rotation  gamma  around
*          * the z-axis, from the x-axis towards the y-axis (right-handed)
*          * the z axis here is defined by the CHAMBERS not the HMS
*          * we later rotate the tracks appropriately into the HMS frame
*          * for now all we need are Px and Py such that   u = x*Px + y*Py

*          * the following quantities can have different values for each chamber sets,
*          * so we need to distinguish between chamber sets in the in/out, too!
           iPlane = iPlane + H_FPP_N_DCLAYERS * H_FPP_N_DCINSET * (iSet-1)
           HFPP_resolution(iset,ichamber,ilayer) = HFPP_planeresolution(iPlane)

         enddo
       enddo
      enddo


*     * re-map card position array into (set,chamber,layer,wire) variable
      do iset=1, H_FPP_N_DCSETS
       do ichamber=1, H_FPP_N_DCINSET
        do ilayer=1, H_FPP_N_DCLAYERS
	 do iWire=1, H_FPP_MAX_WIRES
	  HFPP_cardpos(iSet,iChamber,iLayer,iWire) = 0
         enddo
        enddo
       enddo
      enddo

      do ii=1,FPPNUMCARDS
	iSet     = fpp_planemap(2,ii)
	iChamber = fpp_planemap(3,ii)
	iLayer   = fpp_planemap(4,ii)
	wlo      = fpp_planemap(6,ii)
	whi      = fpp_planemap(7,ii)
	sign     = fpp_planemap(8,ii)
	if (wlo.lt.whi) then
	  wstep = 1
	else
	  wstep = -1
	endif
	do iWire=wlo,whi,wstep
	  HFPP_cardpos(iSet,iChamber,iLayer,iWire) = sign
	enddo
      enddo


*     * create mapping from plane to set,chamber,layer
      do iset=1, H_FPP_N_DCSETS
        do ichamber=1, H_FPP_N_DCINSET
          do ilayer=1, H_FPP_N_DCLAYERS

             iPlane = H_FPP_N_DCLAYERS * H_FPP_N_DCINSET * (iSet-1)
     >       	    + H_FPP_N_DCLAYERS * (iChamber-1)
     >       	    + iLayer

	     HFPP_plane2set(iPlane)	= iSet
	     HFPP_plane2chamber(iPlane)	= iChamber
	     HFPP_plane2layer(iPlane)	= iLayer

          enddo !ilayer
        enddo !ichamber
      enddo !iset



      if (.true.) then
*     * output geometry params to verify proper reading of param file
       do iset=1, H_FPP_N_DCSETS
            write(6,123) iset,HFPP_Xoff(iset), HFPP_Yoff(iset)
        do ichamber=1, H_FPP_N_DCINSET
         write(6,*) ''
           do ilayer=1, H_FPP_N_DCLAYERS

             iPlane = H_FPP_N_DCLAYERS * H_FPP_N_DCINSET * (iSet-1)
     >       	    + H_FPP_N_DCLAYERS * (iChamber-1)
     >       	    + ilayer

            write(6,124) ichamber,ilayer,
     >       HFPP_layerZ(iset,ichamber,ilayer),
     >       HFPP_direction(iset,ichamber,ilayer,1),HFPP_direction(iset,ichamber,ilayer,2),
     >       HFPP_layeroffset(iset,ichamber,ilayer), HFPP_Nwires(iplane),
     >       HFPP_spacing(iset,ichamber,ilayer), HFPP_resolution(iset,ichamber,ilayer),
     >       HFPP_gamma(iset)
           enddo
        enddo
       enddo
      endif

 123  FORMAT (2x,i1,'  chamber x,y ', 2(1x,f5.1))
 124  FORMAT (5x,2(1x,i1),'  z=',f6.1, '  dx,dy=',2(f6.3,1x),'  Off=',f6.1,
     >        '  wires=',i3,'  spc=',f4.2,'  res=',f5.3,'  rot=',f6.2)




*     * calculate the rotation matrices for chamber sets
*     * definition of Euler rotation angles as per  G. Arfken, pp.199
      do iset=1, H_FPP_N_DCSETS

            sinalpha = sin( HFPP_alpha(iset) *pi/180.)
            sinbeta  = sin( HFPP_beta(iset)  *pi/180.)
            singamma = sin( HFPP_gamma(iset) *pi/180.)

            cosalpha = cos( HFPP_alpha(iset) *pi/180.)
            cosbeta  = cos( HFPP_beta(iset)  *pi/180.)
            cosgamma = cos( HFPP_gamma(iset) *pi/180.)

*           * matrix is such that:  (x,y,z CHAMBER) = [M] x (x,y,z HMS)
*
*           *      ,---- 1=x,2=y,3=z in layer's coords
*           *      | ,-- 1=x,2=y,3=z in lab coords
            matrix(1,1)  =      cosalpha*cosbeta*cosgamma - sinalpha*singamma
            matrix(1,2)  =      sinalpha*cosbeta*cosgamma + cosalpha*singamma
            matrix(1,3)  = -1.0*         sinbeta*cosgamma

            matrix(2,1)  = -1.0*cosalpha*cosbeta*singamma - sinalpha*cosgamma
            matrix(2,2)  = -1.0*sinalpha*cosbeta*singamma + cosalpha*cosgamma
            matrix(2,3)  =               sinbeta*singamma

            matrix(3,1)  =      cosalpha*sinbeta
            matrix(3,2)  =      sinalpha*sinbeta
            matrix(3,3)  =               cosbeta

*           * INVERSE matrix is such that:  (x,y,z HMS) = [M-1] x (x,y,z CHAMBER)
*
*           *       ,---- 1=x,2=y,3=z in lab coords
*           *       | ,-- 1=x,2=y,3=z in layer's coords
            Imatrix(1,1) =	cosalpha*cosbeta*cosgamma - sinalpha*singamma
            Imatrix(1,2) = -1.0*cosalpha*cosbeta*singamma - sinalpha*cosgamma
            Imatrix(1,3) =	cosalpha*sinbeta

            Imatrix(2,1) =	sinalpha*cosbeta*cosgamma + cosalpha*singamma
            Imatrix(2,2) = -1.0*sinalpha*cosbeta*singamma + cosalpha*cosgamma
            Imatrix(2,3) =	sinalpha*sinbeta

            Imatrix(3,1) = -1.0*         sinbeta*cosgamma
            Imatrix(3,2) =	         sinbeta*singamma
            Imatrix(3,3) =	         cosbeta

*           * now copy the easy-to-read local matrix to the shared array
*           * note the reversal of indices between the matrices!!
            do ilab=1,3
              do iloc=1,3
                HFPP_Mrotation(iset,iloc,ilab) =  matrix(iloc,ilab)
                HFPP_Irotation(iset,ilab,iloc) = Imatrix(ilab,iloc)
              enddo !iloc
            enddo !ilab

      enddo !iset


*     * initialize driftmap
      call h_fpp_drift_init(ABORT,err)
      IF(ABORT) THEN
        call G_add_path(here,err)
      ENDIF


      RETURN
      END
