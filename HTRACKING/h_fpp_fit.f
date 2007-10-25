      SUBROUTINE h_fpp_fit_simple(DCset,Clusters,nPoints,Track,ABORT,err)
*--------------------------------------------------------
*    Hall C  HMS Focal Plane Polarimeter Code
*
*  Purpose: simple fit to wire centroids, treat all hits equally
* 
*  Created by Frank R. Wesselmann,  February 2004
*
*--------------------------------------------------------

      IMPLICIT NONE

      INCLUDE 'hms_data_structures.cmn'
      INCLUDE 'hms_geometry.cmn'
      include 'gen_detectorids.par'
      include 'gen_decode_common.cmn'
      INCLUDE 'hms_fpp_event.cmn'

      character*16 here
      parameter (here= 'h_fpp_fit_simple')

      integer*4 DCset   ! set of FPP DCs we are working on
      integer*4 Clusters(H_FPP_N_DCINSET,H_FPP_N_DCLAYERS)
      integer*4 nPoints
      real*4 Track(5)

      logical ABORT
      character*(*) err

      real*4 wirepos
      real*4 Coords(H_FPP_MAX_FITPOINTS,2)
      real*4 Sigmas(H_FPP_MAX_FITPOINTS)
      real*4 Project(H_FPP_MAX_FITPOINTS,2)
      real*4 FitParm(5)  ! does NOT include hit count

      integer*4 iChamber, iLayer, iCluster, iRaw, iHit, ii


*     * init result to bad
      Track(1) = H_FPP_BAD_COORD   !mx
      Track(2) = H_FPP_BAD_COORD   !bx
      Track(3) = H_FPP_BAD_COORD   !my
      Track(4) = H_FPP_BAD_COORD   !by
      Track(5) = H_FPP_BAD_CHI2


*     * transfer abstract Clusters into linear array of hit coords
      nPoints = 0

      do iChamber=1,H_FPP_N_DCINSET
       do iLayer=1,H_FPP_N_DCLAYERS

         iCluster = Clusters(iChamber,iLayer)
         if (iCluster.gt.0) then
           do iRaw=1,HFPP_nHitsinCluster(DCset,iChamber,iLayer,iCluster)

             nPoints = nPoints + 1
             iHit = HFPP_Clusters(DCset,iChamber,iLayer,iCluster,iRaw)

             wirepos = HFPP_layeroffset(DCset,iChamber,iLayer)
     >               + HFPP_spacing(DCset,iChamber,iLayer)*HFPP_raw_wire(iHit)

*            * tracking works in u-z coordinate system
             Coords(nPoints,1) = wirepos
             Coords(nPoints,2) = HFPP_layerZ(DCset,iChamber,iLayer)

             Project(nPoints,1) = HFPP_direction(DCset,iChamber,iLayer,1)
             Project(nPoints,2) = HFPP_direction(DCset,iChamber,iLayer,2)

*            * we dont use drift here so use 1/3 wire spacing as sigma!
             Sigmas(nPoints) = (HFPP_spacing(DCset,iChamber,iLayer)/3.0)**2

           enddo !iRaw
         endif

       enddo !iLayer
      enddo !iChamber


*     * then we feed these coords to our fitting routine to get track
      if (nPoints.gt.0) then

        call h_fpp_fit3d(nPoints, Coords, Sigmas, Project, FitParm)

*       * transfer results
        do ii=1,5
          Track(ii) = FitParm(ii)
        enddo !ii

      endif


      RETURN
      END


c==============================================================================
c==============================================================================
c==============================================================================
c==============================================================================


      SUBROUTINE h_fpp_fit_best_permutation(nPoints, Points, Sigma2s, 
     >                                      Projects, Drifts, BestTrack)
*--------------------------------------------------------
*    Hall C  HMS Focal Plane Polarimeter Code
*
*  Purpose: find best fit among the possible permutations obtainable
*           by arranging drift to be to left or right of each wire
* 
*  WARNING: the array  Drifts  contains the unsigned (L?R ambiguous)
*           drifts as input but _replaces_ them with the resolved
*           signed drifts as output
*           PROCTECT THE DATA IN THE CALLING ROUTINE!!!
* 
*  Created by Frank R. Wesselmann,  February 2004
*
*--------------------------------------------------------

      IMPLICIT NONE

      INCLUDE 'hms_data_structures.cmn'
      INCLUDE 'hms_geometry.cmn'
      include 'gen_detectorids.par'
      include 'gen_decode_common.cmn'
      INCLUDE 'hms_fpp_event.cmn'
      INCLUDE 'hms_fpp_params.cmn'

      integer*4 nPoints
      real*4 Points(H_FPP_MAX_FITPOINTS,2)   ! u,z
      real*4 Sigma2s(H_FPP_MAX_FITPOINTS)
      real*4 Projects(H_FPP_MAX_FITPOINTS,3)
      real*4 Drifts(H_FPP_MAX_FITPOINTS)
      real*4 BestTrack(6)  ! does NOT include hit count


** now we use the supplied hits and the absolute drift distance (in layer!)
** to find the permutation of positive and negative drift directions that
** produces the best track


      real*4 HitPos(H_FPP_MAX_FITPOINTS,2)
      real*4 DriftAbs(H_FPP_MAX_FITPOINTS), driftreal(H_FPP_MAX_FITPOINTS)
      real*4 BestDrifts(H_FPP_MAX_FITPOINTS)
      real*4 Track(6)  ! does NOT include hit count

      integer*4 iHit, attempts, toggleat, ii

      logical*4 drift2plus(H_FPP_MAX_FITPOINTS)
      logical*4 anyPerm2try, carry


*     * init result to bad
      BestTrack(1) = H_FPP_BAD_COORD  ! mx
      BestTrack(2) = H_FPP_BAD_COORD  ! bx
      BestTrack(3) = H_FPP_BAD_COORD  ! my
      BestTrack(4) = H_FPP_BAD_COORD  ! by
      BestTrack(5) = H_FPP_BAD_CHI2


      if (nPoints.lt.HFPP_minsethits) RETURN


*     * init
      do iHit=1,nPoints
        DriftAbs(iHit)   = Drifts(iHit) ! save absolute drifts
        drift2plus(iHit) = .false.      ! init left/right pointer to LEFT
	BestDrifts(iHit) = H_FPP_BAD_DRIFT
      enddo !iHit


      anyPerm2try = .true.
      attempts = 0
      do while (anyPerm2try)
        attempts = attempts+1

	do iHit=1,nPoints

*     	  * figure real drift from absolute and sign
          if (drift2plus(iHit)) then
	    driftreal(iHit) = abs(DriftAbs(iHit))
	  else
	    driftreal(iHit) = -1.*abs(DriftAbs(iHit))
	  endif

*     	  * adjust hit position based on drift
          HitPos(iHit,1) = Points(iHit,1) + driftreal(iHit)	! u
          HitPos(iHit,2) = Points(iHit,2)			! z

	enddo ! iHit


*     	* get track based on these drift values
	call h_fpp_fit3d(nPoints, HitPos, Sigma2s, Projects, Track)

*     	* remember best track and set of drift flags
      	if (Track(5).lt.BestTrack(5).and.Track(5).gt.0.0) then
      	  do ii=1,6
      	    BestTrack(ii) = Track(ii)
      	  enddo !ii
      	  do iHit=1, nPoints
      	     BestDrifts(iHit) = driftreal(iHit)
      	  enddo !iHit
      	endif

*	* get next combination of drift directions to try
*	* binary adding:   0 --> 1
*       *              or  1 --> 0+carry
*	* ALWAYS start at the lowest position and add the carry (if any)
*       * to the next highest; continue until no more carry or out of bits
*       * skip layers without a hit in use!
*       * we COULD do this using real binary math but this is likely faster
*       * than re-discovering the state of each "bit" for each iteration...
        toggleat=0
	carry = .true.
	do while (carry)
	  toggleat = toggleat+1
	  if (toggleat.gt.nPoints) EXIT
	  carry = drift2plus(toggleat)
	  drift2plus(toggleat) = .not.drift2plus(toggleat)
	enddo

	anyPerm2try = .not.carry  !only get carry here if all permutations were tried

      enddo !anyPerm2try


      do iHit=1, nPoints
        Drifts(iHit) = BestDrifts(iHit)
      enddo !iHit

      RETURN
      END


c==============================================================================
c==============================================================================
c==============================================================================
c==============================================================================


      SUBROUTINE h_fpp_fit3d(n, coords, sig2s, projs, params)
*--------------------------------------------------------
*    Hall C  HMS Focal Plane Polarimeter Code
*
*  Purpose: fit set of hits in 3space to FPP track
*
*  Created by Frank R. Wesselmann,  February 2004
*
*--------------------------------------------------------

      IMPLICIT NONE

      INCLUDE 'hms_data_structures.cmn'

*     * function arguments    INPUT: n, coords, sig2s    OUTPUT: params, chi2

      integer*4 n                          ! number of points to fit
      real*4 coords(H_FPP_MAX_FITPOINTS,2) ! coords of points to fit u,z
      real*4 sig2s(H_FPP_MAX_FITPOINTS)    ! resolution (sigma**2) of each
                                           ! point on own axis
      real*4 projs(H_FPP_MAX_FITPOINTS,2)  ! u=proj(1)*x + proj(2)*y
      real*4 params(5)                     ! resulting fit params mx bx my by
                                           !      and the reduced chi2 of fit

***
*** code is extension of simple 1-dimensional straight-line fit
***
*** generalization is that we fit orthogonal straight line projections
*** in x VS z and in y VS z simultaneously
***
*** each fit point is specified by an arbitrary coordinate u, which is
*** some linear combination of x and y, and the global z coordinate
*** also supplied are the projection factprs Px and Py correlating u
*** with x,y as given by    u = Px * x + Py * y
*** presumably, the coordinate u is in a system native to the supplied
*** data point, as in the measureing direction of a wire chamber plane
*** and these projection factors can be pre-determined and kept fixed
***
*** the fit results are still produced in x and y coordinates, slopes
*** and offsets s. th.  x = z * mx + bx
***               and   y = z * my + by
***
*** since the fit is a chi-squared minimization, a weight (or scale) is
*** needed to judge each points' significance; these are also to be 
*** interpreted in the u coordinate and are to be supplied as their own
*** square, to remove the repetitive squaring calculation
***
***
*** the expressions below for mx, bx, my and by were obtained using the
*** symbolic algebra software REDUCE with the command:
***
***    solve({Suxz = mx*Sxxzz + my*Sxyzz + bx*Sxxz + by*Sxyz,
***           Suyz = mx*Sxyzz + my*Syyzz + bx*Sxyz + by*Syyz,
***           Sux  = mx*Sxxz  + my*Sxyz  + bx*Sxx  + by*Sxy,
***           Suy  = mx*Sxyz  + my*Syyz  + bx*Sxy  + by*Syy}, {mx,my,bx,by});
***
*** for the sums we use the terminology that the S... variables are sums over
*** the different terms in the chi**2 expression where u indicates the
*** measured coordinate value, x and y are the respective projection factors
*** and z is the z coordinate of the measurement;  repetitions indicate powers
***   chi**2 = Sum (u_i - Px*mx*z_i - Px*bx - Py*my*z_i - Py*by)**2 / sig_i**2
***


*     * short form of fit parameters
      real*8 mx, my, bx, by

*     * short form of point coords, sigma and projection factors
      real*8 ui,zi, Px,Py, sigma2

*     * various sums
      real*8 Sux, Suxz, Suy, Suyz, Suu
      real*8 Sxx, Sxxz, Sxxzz
      real*8 Sxy, Sxyz, Sxyzz
      real*8 Syy, Syyz, Syyzz
      real*8 chi2

      real*8 denom
      integer*4 i


      mx =  dble(H_FPP_BAD_COORD)
      bx =  dble(H_FPP_BAD_COORD)
      my =  dble(H_FPP_BAD_COORD)
      by =  dble(H_FPP_BAD_COORD)

      chi2 = dble(H_FPP_BAD_CHI2)

      if (n .ge. 2) then

        Suu   = 0.D0
        Sux   = 0.D0
        Suy   = 0.D0
        Suxz  = 0.D0
        Suyz  = 0.D0
        Sxx   = 0.D0
        Sxy   = 0.D0
        Syy   = 0.D0
        Sxxz  = 0.D0
        Sxyz  = 0.D0
        Syyz  = 0.D0
        Sxxzz = 0.D0
        Sxyzz = 0.D0
        Syyzz = 0.D0

        do i=1, n
          ui = dble(coords(i,1))
          zi = dble(coords(i,2))
          Px = dble(projs(i,1))
          Py = dble(projs(i,2))
          sigma2 = dble(sig2s(i))   !sig2s are already squared!

          if (sigma2 .gt. 0.d0) then
            Suu   = Suu   + ui*ui/sigma2
            Sux   = Sux   + ui*Px/sigma2
            Suy   = Suy   + ui*Py/sigma2
            Suxz  = Suxz  + ui*Px*zi/sigma2
            Suyz  = Suyz  + ui*Py*zi/sigma2
            Sxx   = Sxx   + Px*Px/sigma2
            Sxy   = Sxy   + Px*Py/sigma2
            Syy   = Syy   + Py*Py/sigma2
            Sxxz  = Sxxz  + Px*Px*zi/sigma2
            Sxyz  = Sxyz  + Px*Py*zi/sigma2
            Syyz  = Syyz  + Py*Py*zi/sigma2
            Sxxzz = Sxxzz + Px*Px*zi*zi/sigma2
            Sxyzz = Sxyzz + Px*Py*zi*zi/sigma2
            Syyzz = Syyzz + Py*Py*zi*zi/sigma2
          endif
        enddo !n


        denom = Sxx*Sxxzz*Syy*Syyzz - Sxx*Sxxzz*Syyz*Syyz - Sxx*Sxyz*Sxyz*Syyzz
     >        + Sxx*Sxyz*Sxyzz*Syyz + Sxx*Sxyz*Sxyzz*Syyz - Sxx*Sxyzz*Sxyzz*Syy
     >        - Sxxz*Sxxz*Syy*Syyzz + Sxxz*Sxxz*Syyz*Syyz + Sxxz*Sxy*Sxyz*Syyzz
     >        + Sxxz*Sxy*Sxyz*Syyzz - Sxxz*Sxy*Sxyzz*Syyz - Sxxz*Sxy*Sxyzz*Syyz
     >        - Sxxz*Sxyz*Sxyz*Syyz - Sxxz*Sxyz*Sxyz*Syyz + Sxxz*Sxyz*Sxyzz*Syy
     >        + Sxxz*Sxyz*Sxyzz*Syy - Sxxzz*Sxy*Sxy*Syyzz + Sxxzz*Sxy*Sxyz*Syyz
     >        + Sxxzz*Sxy*Sxyz*Syyz - Sxxzz*Sxyz*Sxyz*Syy + Sxy*Sxy*Sxyzz*Sxyzz
     >        - Sxy*Sxyz*Sxyz*Sxyzz - Sxy*Sxyz*Sxyz*Sxyzz + Sxyz*Sxyz*Sxyz*Sxyz

        if (denom .ne. 0.D0) then

          denom = 1.d0/denom

          mx = denom *
     >      ( Sux*Sxxz*Syyz*Syyz - Sux*Sxxz*Syy*Syyzz + Sux*Sxy*Sxyz*Syyzz
     >      - Sux*Sxy*Sxyzz*Syyz - Sux*Sxyz*Sxyz*Syyz + Sux*Sxyz*Sxyzz*Syy
     >      + Suxz*Sxx*Syy*Syyzz - Suxz*Sxx*Syyz*Syyz - Suxz*Sxy*Sxy*Syyzz
     >      + Suxz*Sxy*Sxyz*Syyz + Suxz*Sxy*Sxyz*Syyz - Suxz*Sxyz*Sxyz*Syy
     >      + Suy*Sxx*Sxyzz*Syyz + Suy*Sxxz*Sxy*Syyzz - Suy*Sxxz*Sxyz*Syyz
     >      - Suy*Sxy*Sxyz*Sxyzz + Suy*Sxyz*Sxyz*Sxyz + Suyz*Sxx*Sxyz*Syyz
     >      - Suyz*Sxx*Sxyzz*Syy - Suyz*Sxxz*Sxy*Syyz + Suyz*Sxxz*Sxyz*Syy
     >      + Suyz*Sxy*Sxy*Sxyzz - Suyz*Sxy*Sxyz*Sxyz - Suy*Sxx*Sxyz*Syyzz)

          my = denom *
     >      ( Sux*Sxxz*Sxyzz*Syy - Sux*Sxxz*Sxyz*Syyz + Sux*Sxxzz*Sxy*Syyz
     >      - Sux*Sxxzz*Sxyz*Syy - Sux*Sxy*Sxyz*Sxyzz + Sux*Sxyz*Sxyz*Sxyz
     >      + Suxz*Sxx*Sxyz*Syyz - Suxz*Sxx*Sxyzz*Syy - Suxz*Sxxz*Sxy*Syyz
     >      + Suxz*Sxxz*Sxyz*Syy + Suxz*Sxy*Sxy*Sxyzz - Suxz*Sxy*Sxyz*Sxyz
     >      - Suy*Sxx*Sxxzz*Syyz + Suy*Sxx*Sxyz*Sxyzz + Suy*Sxxz*Sxxz*Syyz
     >      - Suy*Sxxz*Sxy*Sxyzz - Suy*Sxxz*Sxyz*Sxyz + Suy*Sxxzz*Sxy*Sxyz
     >      + Suyz*Sxx*Sxxzz*Syy - Suyz*Sxx*Sxyz*Sxyz - Suyz*Sxxz*Sxxz*Syy
     >      + Suyz*Sxxz*Sxy*Sxyz + Suyz*Sxxz*Sxy*Sxyz - Suyz*Sxxzz*Sxy*Sxy)

          bx = denom *
     >      ( Sux*Sxxzz*Syy*Syyzz - Sux*Sxxzz*Syyz*Syyz - Sux*Sxyz*Sxyz*Syyzz
     >      + Sux*Sxyz*Sxyzz*Syyz + Sux*Sxyz*Sxyzz*Syyz - Sux*Sxyzz*Sxyzz*Syy
     >      + Suxz*Sxxz*Syyz*Syyz + Suxz*Sxy*Sxyz*Syyzz - Suxz*Sxy*Sxyzz*Syyz
     >      - Suxz*Sxyz*Sxyz*Syyz + Suxz*Sxyz*Sxyzz*Syy + Suy*Sxxz*Sxyz*Syyzz
     >      - Suy*Sxxz*Sxyzz*Syyz - Suy*Sxxzz*Sxy*Syyzz + Suy*Sxxzz*Sxyz*Syyz
     >      + Suy*Sxy*Sxyzz*Sxyzz - Suy*Sxyz*Sxyz*Sxyzz - Suyz*Sxxz*Sxyz*Syyz
     >      + Suyz*Sxxz*Sxyzz*Syy + Suyz*Sxxzz*Sxy*Syyz - Suyz*Sxxzz*Sxyz*Syy
     >      - Suyz*Sxy*Sxyz*Sxyzz + Suyz*Sxyz*Sxyz*Sxyz - Suxz*Sxxz*Syy*Syyzz)

          by = denom *
     >      ( Sux*Sxxz*Sxyz*Syyzz - Sux*Sxxz*Sxyzz*Syyz - Sux*Sxxzz*Sxy*Syyzz
     >      + Sux*Sxxzz*Sxyz*Syyz + Sux*Sxy*Sxyzz*Sxyzz - Sux*Sxyz*Sxyz*Sxyzz
     >      - Suxz*Sxx*Sxyz*Syyzz + Suxz*Sxx*Sxyzz*Syyz + Suxz*Sxxz*Sxy*Syyzz
     >      - Suxz*Sxxz*Sxyz*Syyz - Suxz*Sxy*Sxyz*Sxyzz + Suxz*Sxyz*Sxyz*Sxyz
     >      + Suy*Sxx*Sxxzz*Syyzz - Suy*Sxx*Sxyzz*Sxyzz - Suy*Sxxz*Sxxz*Syyzz
     >      + Suy*Sxxz*Sxyz*Sxyzz + Suy*Sxxz*Sxyz*Sxyzz - Suy*Sxxzz*Sxyz*Sxyz
     >      + Suyz*Sxx*Sxyz*Sxyzz + Suyz*Sxxz*Sxxz*Syyz - Suyz*Sxxz*Sxy*Sxyzz
     >      - Suyz*Sxxz*Sxyz*Sxyz + Suyz*Sxxzz*Sxy*Sxyz - Suyz*Sxx*Sxxzz*Syyz)

          chi2 =    Suu  -    mx*Suxz  -    my*Suyz  -    bx*Sux  -    by*Suy
     >         - mx*Suxz + mx*mx*Sxxzz + mx*my*Sxyzz + mx*bx*Sxxz + mx*by*Sxyz
     >         - my*Suyz + my*mx*Sxyzz + my*my*Syyzz + my*bx*Sxyz + my*by*Syyz
     >         - bx*Sux  + bx*mx*Sxxz  + bx*my*Sxyz  + bx*bx*Sxx  + bx*by*Sxy
     >         - by*Suy  + by*mx*Sxyz  + by*my*Syyz  + by*bx*Sxy  + by*by*Syy

          if (n.gt.4) then
*           * reduced chi**2 -- 4 params & only 1 coord per data point
            chi2 = chi2/dfloat(n-4)
          else
            chi2 = -1.d0
          endif
        endif !denom


      else  ! whatever happened, no straight line fit is to be found here...

          mx =  dble(H_FPP_BAD_COORD)
          bx =  dble(H_FPP_BAD_COORD)
          my =  dble(H_FPP_BAD_COORD)
          by =  dble(H_FPP_BAD_COORD)
          chi2 = dble(H_FPP_BAD_CHI2)

      endif

      params(1) = sngl(mx)
      params(2) = sngl(bx)
      params(3) = sngl(my)
      params(4) = sngl(by)
      params(5) = sngl(chi2)

      RETURN
      END
