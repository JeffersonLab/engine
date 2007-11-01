*--------------------------------------------------------
*--------------------------------------------------------
*--------------------------------------------------------
* 
*    Hall C  HMS Focal Plane Polarimeter Code
* 
*  Created by Frank R. Wesselmann,  February 2004
*
*--------------------------------------------------------
*
*  this file contains several small geometry related routines
*
*--------------------------------------------------------



      SUBROUTINE h_fpp_uTrack(iSet,iCham,iLay,iTrack,uCoord)
*--------------------------------------------------------
*    Hall C  HMS Focal Plane Polarimeter Code
*
*  Purpose: determine in-layer coordinate of intersection
*           of given track and given drift chamber layer
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

      integer*4 iSet, iCham, iLay, iTrack
      real*4 uCoord

      real*4 x,y,z

      uCoord = H_FPP_BAD_COORD

      if (HFPP_N_tracks(iSet).le.0) RETURN
      if (iTrack.le.0.or.iTrack.gt.HFPP_N_tracks(iSet)) RETURN

      if (iSet.le.0.or.iSet.gt.H_FPP_N_DCSETS) RETURN
      if (iCham.le.0.or.iCham.gt.H_FPP_N_DCINSET) RETURN
      if (iLay.le.0.or.iLay.gt.H_FPP_N_DCLAYERS) RETURN

      z = HFPP_layerZ(iSet,iCham,iLay)

      x = HFPP_track_fine(iSet,iTrack,2) + HFPP_track_fine(iSet,iTrack,1)*z
      y = HFPP_track_fine(iSet,iTrack,4) + HFPP_track_fine(iSet,iTrack,3)*z

*     * determine generalized in-layer coordinate
      uCoord = HFPP_direction(iSet,iCham,iLay,1) * x
     >       + HFPP_direction(iSet,iCham,iLay,2) * y

      RETURN
      END


c==============================================================================
c==============================================================================
c==============================================================================
c==============================================================================


      SUBROUTINE h_fpp_FP2DC(iSet,Slope,FPcoords,DCcoords)
*--------------------------------------------------------
*    Hall C  HMS Focal Plane Polarimeter Code
*
*  Purpose: transforms coordinates from HMS focal plane
*           system to the coord system of the specified
*           set of FPP drift chambers
*           alternatively transforms SLOPES
* 
*  Created by Frank R. Wesselmann,  February 2004
*
*--------------------------------------------------------

      IMPLICIT NONE

      INCLUDE 'hms_data_structures.cmn'
      INCLUDE 'hms_geometry.cmn'
c      INCLUDE 'hms_fpp_event.cmn'

      integer*4 iSet
      logical*4 Slope
      real*4 FPcoords(3), DCcoords(3)

      integer*4 i,j
      real*4 MYcoords(3)


      if (Slope) then
*       * for slopes, we can ignore any position offset
        MYcoords(1) = FPcoords(1)
        MYcoords(2) = FPcoords(2)
        MYcoords(3) = FPcoords(3)
      else
*       * for coordinates, we need to subtract the offset
        MYcoords(1) = FPcoords(1) - HFPP_Xoff(iSet)
        MYcoords(2) = FPcoords(2) - HFPP_Yoff(iSet)
        MYcoords(3) = FPcoords(3) - HFPP_Zoff(iSet)
      endif

*     * use rotation matrix to rotate from focal plane coords to DCset coords
      do i=1,3  !x,y,z for DC
        DCcoords(i) = 0.0
	do j=1,3  !x,y,z for FP
	  DCcoords(i) = DCcoords(i) + HFPP_Mrotation(iSet,i,j) * MYcoords(j)
	enddo
      enddo

      if (slope) then
*       * for slopes, we need to renormalize to dz=1
        if (DCcoords(3).eq.0.0) then
	  DCcoords(1) = H_FPP_BAD_COORD
	  DCcoords(2) = H_FPP_BAD_COORD
	else
	  DCcoords(1) = DCcoords(1) / DCcoords(3)
	  DCcoords(2) = DCcoords(2) / DCcoords(3)
	  DCcoords(3) = 1.0
	endif
      endif


      RETURN
      END


c==============================================================================
c==============================================================================
c==============================================================================
c==============================================================================


      SUBROUTINE h_fpp_DC2FP(iSet,Slope,DCcoords,FPcoords)
*--------------------------------------------------------
*    Hall C  HMS Focal Plane Polarimeter Code
*
*  Purpose: transforms coordinates from the coord system
*           of the specified set of FPP drift chambers to
*           the the HMS focal plane system
*           alternatively transforms SLOPES
* 
*  Created by Frank R. Wesselmann,  February 2004
*
*--------------------------------------------------------

      IMPLICIT NONE

      INCLUDE 'hms_data_structures.cmn'
      INCLUDE 'hms_geometry.cmn'
c      INCLUDE 'hms_fpp_event.cmn'

      integer*4 iSet
      logical*4 Slope
      real*4 DCcoords(3), FPcoords(3)

      integer*4 i,j


*     * use INVERSE rotation matrix to rotate from DCset coords to focal plane coords
      do i=1,3  !x,y,z for FP
        FPcoords(i) = 0.0
	do j=1,3  !x,y,z for DC
	  FPcoords(i) = FPcoords(i) + HFPP_Irotation(iSet,i,j) * DCcoords(j)
	enddo
      enddo

      if (Slope) then
*       * for slopes, we need to renormalize to dz=1 if possible
        if (FPcoords(3).eq.0.0) then
	  FPcoords(1) = H_FPP_BAD_COORD
	  FPcoords(2) = H_FPP_BAD_COORD
	else
	  FPcoords(1) = FPcoords(1) / FPcoords(3)
	  FPcoords(2) = FPcoords(2) / FPcoords(3)
	  FPcoords(3) = 1.0
	endif

      else
*       * for coordinates, we need to add the offset
        FPcoords(1) = FPcoords(1) + HFPP_Xoff(iSet)
        FPcoords(2) = FPcoords(2) + HFPP_Yoff(iSet)
        FPcoords(3) = FPcoords(3) + HFPP_Zoff(iSet)
      endif

      RETURN
      END


c==============================================================================
c==============================================================================
c==============================================================================
c==============================================================================


      SUBROUTINE h_fpp_closest(Track1,Track2,sclose,zclose)
*--------------------------------------------------------
*    Hall C  HMS Focal Plane Polarimeter Code
*
*  Purpose: given two lines (tracks) in space, determine
*           the distance of closest approach and the 
*           average z-coordinate of the closest approach
* 
*  Created by Frank R. Wesselmann,  February 2004
*
*--------------------------------------------------------

      IMPLICIT NONE

      INCLUDE 'hms_data_structures.cmn'

      real*4 Track1(4), Track2(4)  ! IN mx,bx,my,by of two tracks
      real*4 sclose                ! OUT distance at closest approach
      real*4 zclose                ! OUT average z-coordinate at c.a.

      real*8 mx1,my1,bx1,by1
      real*8 mx2,my2,bx2,by2
      real*8 a1,a2,b,c1,c2
      real*8 x1,x2,y1,y2,z1,z2
      real*8 denom

      mx1 = dble(Track1(1))
      bx1 = dble(Track1(2))
      my1 = dble(Track1(3))
      by1 = dble(Track1(4))
 
      mx2 = dble(Track2(1))
      bx2 = dble(Track2(2))
      my2 = dble(Track2(3))
      by2 = dble(Track2(4))

      a1 = mx1*mx1 + my1*my1 + 1
      a2 = mx2*mx2 + my2*my2 + 1
      b  = mx1*mx2 + my1*my2 + 1
      c1 = (bx1 - bx2)*mx1 + (by1 - by2)*my1
      c2 = (bx1 - bx2)*mx2 + (by1 - by2)*my2
      
      denom = b*b - a1*a2
      if (denom.eq.0.0d0) then
	zclose = H_FPP_BAD_COORD
	sclose = H_FPP_BAD_COORD
      else
	z1 = (a2*c1 -  b*c2) / denom
	z2 = ( b*c1 - a1*c2) / denom

	x1 = z1 * mx1 + bx1
	y1 = z1 * my1 + by1

	x2 = z2 * mx2 + bx2
	y2 = z2 * my2 + by2

	zclose = sngl(0.5d0*(z1 + z2))
	sclose = sngl(sqrt( (x1-x2)**2 + (y1-y2)**2 + (z1-z2)**2 ))
      endif

c      write(*,*)'Zclose calculation 1: ',mx1,mx2,my1,my2
c      write(*,*)'Zclose calculation 2: ',bx1,bx2,by1,by2
c      write(*,*)'Zclose calculation 2a:',b,denom
c      write(*,*)'Zclose calculation 2b:',a1,a2,c1,c2
c      write(*,*)'Zclose calculation 3: ',ztrack2,z1,z2
c      write(*,*)'Zclose calculation 4: ',x1,x2,y1,y2
c      write(*,*)'Zclose calculation 5: ',zclose,sclose

      RETURN
      END

c==============================================================================
c==============================================================================
c==============================================================================
c==============================================================================


      SUBROUTINE h_fpp_conetest(Track1,DCset,zclose,theta,icone)
*--------------------------------------------------------
*    Hall C  HMS Focal Plane Polarimeter Code
*
*  Purpose: Calculate FPP conetest variable - assumes elliptical projection
*           onto the last layer of the 2nd chamber in a set.
*
*  Created by Edward J. Brash,  September 2007
*
*--------------------------------------------------------

      IMPLICIT NONE

      INCLUDE 'hms_data_structures.cmn'
      INCLUDE 'hms_geometry.cmn'

      real*4 Track1(4)  ! IN mx,bx,my,by of front track
      integer*4 DCset 		   ! which chamber set we are in
      real*4 zclose 	           ! previously calculated z of closest approach
      real*4 theta                 ! polar scattering angle
      integer*4 icone              ! Cone-test variable (1=pass, 0=fail)

      real*8 mx1,my1,bx1,by1
      real*4 ztrack2               ! central z-position of FPP set
      real*4 zback_off             ! offset from cntrl. pos. of last layer
      real*8 zback,xfront,yfront,ttheta
      real*8 r1x,r1y,r2x,r2y,xmin,xmax,ymin,ymax
      real*8 xpt1,xpt2,xpt3,xpt4,ypt1,ypt2,ypt3,ypt4
      integer*4 iSet,iChamber,iLayer,iPlane

      mx1 = Track1(1)*1.0d0
      bx1 = Track1(2)*1.0d0
      my1 = Track1(3)*1.0d0
      by1 = Track1(4)*1.0d0
 
      xmin=HFPP_Xoff(DCset)-HFPP_Xsize(DCset)/2.0 
      xmax=HFPP_Xoff(DCset)+HFPP_Xsize(DCset)/2.0 
      ymin=HFPP_Yoff(DCset)-HFPP_Ysize(DCset)/2.0 
      ymax=HFPP_Yoff(DCset)+HFPP_Ysize(DCset)/2.0 

      icone=1

      iSet=DCset
	do iChamber=1, H_FPP_N_DCINSET
       	   do iLayer=1, H_FPP_N_DCLAYERS
	      
              iPlane = H_FPP_N_DCLAYERS * H_FPP_N_DCINSET * (iSet-1)
     >            + H_FPP_N_DCLAYERS * (iChamber-1)
     >            + iLayer

	      zback_off = HFPP_layerZ(iSet,iChamber,iLayer)
	      ztrack2 = HFPP_Zoff(iSet)
	      zback = ztrack2 + zback_off

	      xfront = bx1 + mx1*zback
      	      yfront = by1 + my1*zback

      	      ttheta=tan(theta)

              r1x = (zback-zclose)*(mx1 + (ttheta-mx1)/(1.0+ttheta*mx1))
              r2x = (zback-zclose)*((ttheta+mx1)/(1.0-ttheta*mx1) - mx1)
              r1y = (zback-zclose)*(my1 +
     &           (ttheta-my1)/(1.0+ttheta*my1))
              r2y = (zback-zclose)*((ttheta+my1)/(1.0-ttheta*my1)
     &           - my1)

              xpt1=xfront-abs(r1x)
              ypt1=yfront
              xpt2=xfront+abs(r2x)
              ypt2=yfront
              xpt3=xfront
              ypt3=yfront-abs(r1y)
              xpt4=xfront
              ypt4=yfront+abs(r2y)

             if(xpt1.lt.xmin)icone=0
             if(xpt2.gt.xmax)icone=0
             if(ypt3.lt.ymin)icone=0
             if(ypt4.gt.ymax)icone=0
              
c              if(icone.eq.0) then
c                write(*,*)'chamber limits ',xmin,xmax,ymin,ymax
c                write(*,*)'(',xpt1,',',ypt1,')'
c                write(*,*)'(',xpt2,',',ypt2,')'
c                write(*,*)'(',xpt3,',',ypt3,')'
c                write(*,*)'(',xpt4,',',ypt4,')'
c              endif
              
	  enddo ! iLayer
	enddo ! iChamber
      
      RETURN
      END

c==============================================================================
c==============================================================================
c==============================================================================
c==============================================================================


      SUBROUTINE h_fpp_relative_angles(mx_ref,my_ref,mx_new,my_new,theta,phi)
*--------------------------------------------------------
*    Hall C  HMS Focal Plane Polarimeter Code
*
*  Purpose: find the POLAR angles between two tracks
*           reference track is likely the incident HMS track, and the 
*           new track is probably the FPP track
*           tracks are only identified by horizontal and vertical slopes
*           we rotate both tracks s.th. the ref track is the new z axis
*           then the simple polar angles of the rotated "new" track are
*           the ones we want
* 
*  Created by Frank R. Wesselmann,  February 2004
*
*--------------------------------------------------------

      IMPLICIT NONE

      INCLUDE 'hms_data_structures.cmn'

      real*4 mx_ref,my_ref	! IN  slopes of reference track (incident?)
      real*4 mx_new,my_new	! IN  slopes of interesting track (analyzer?)
      real*4 theta,phi		! OUT _polar_ angles of new track relative to ref

      real*8 alpha, beta	! horizontal and vertical angle of ref track
      real*8 M(3,3)		! rotation matrix: (row,column)
      real*8 r_i(3)		! unit vector along "new" track before rotation
      real*8 r_f(3)		! unit vector along "new" track after rotation
      real*8 r_in(3)		! unit vector along "in" track before rotation
      real*8 r_fin(3)		! unit vector along "in" track after rotation
      real*8 magnitude
      real*8 dtheta,dphi	! for convenience, double precision versions of OUT
      real*8 x,y,z,xin,yin,zin

      integer i,j


*     * figure out rotation matrix

c      write(*,*)'Theta calculation 1: ',mx_ref,mx_new,my_ref,my_new

      beta  = datan(dble(my_ref))
      alpha = datan(dble(mx_ref)*dcos(beta))     ! this ought to be safe as the negative angle works

      M(1,1) =       dcos(alpha)
      M(1,2) = -1.d0*dsin(alpha)*dsin(beta)
      M(1,3) = -1.d0*dsin(alpha)*dcos(beta)

      M(2,1) =  0.d0
      M(2,2) =                   dcos(beta)
      M(2,3) = -1.d0*            dsin(beta)

      M(3,1) =       dsin(alpha)
      M(3,2) =       dcos(alpha)*dsin(beta)
      M(3,3) =       dcos(alpha)*dcos(beta)

*     * normalize incoming vector

      xin = dble(mx_ref)
      yin = dble(my_ref)
      zin = 1.d0
      magnitude = dsqrt(xin*xin+yin*yin+zin*zin)
      r_in(1)=xin/magnitude
      r_in(2)=yin/magnitude
      r_in(3)=zin/magnitude
      
      do i=1,3
        r_fin(i) = 0.d0
	do j=1,3
	  r_fin(i) = r_fin(i) + M(i,j)*r_in(j)
	enddo !j
      enddo !i

c      write(*,*)r_in(1),r_in(2),r_in(3)
c      write(*,*)r_fin(1),r_fin(2),r_fin(3)
      
*     * normalize direction vector

      x = dble(mx_new)
      y = dble(my_new)
      z = 1.d0
      magnitude = dsqrt(x*x + y*y + z*z)
      r_i(1) = x / magnitude
      r_i(2) = y / magnitude
      r_i(3) = z / magnitude


*     * rotate vector of interest

      do i=1,3
        r_f(i) = 0.d0
	do j=1,3
	  r_f(i) = r_f(i) + M(i,j)*r_i(j)
	enddo !j
      enddo !i

c      write(*,*)r_i(1),r_i(2),r_i(3)
c      write(*,*)r_f(1),r_f(2),r_f(3)

*     * find polar angles

      dtheta = dacos(r_f(3))		! z = cos(theta)

      if (r_f(1).ne.0.d0) then
        if (r_f(1).gt.0.d0) then
          if (r_f(2).gt.0.d0) then         
            dphi = datan( r_f(2)/r_f(1) )	! y/x = tan(phi)
          else
            dphi = datan( r_f(2)/r_f(1) )	! y/x = tan(phi)
            dphi = dphi + 6.28318d0
          endif
        else
          dphi = datan( r_f(2)/r_f(1) )	! y/x = tan(phi)
          dphi = dphi + 3.14159d0
        endif                
      elseif (r_f(2).gt.0.d0) then
        dphi = 1.57080d0			! phi = +90
      elseif (r_f(2).lt.0.d0) then
        phi = 4.71239d0			! phi = +270
      else
        dphi = 0.d0			! phi undefined if theta=0 or r=0
      endif
      
      theta = sngl(dtheta)
      phi   = sngl(dphi)

c      write(*,*)'Theta, phi = ',theta*180.0/3.14159265,phi*180.0/3.14159265


      RETURN
      END
