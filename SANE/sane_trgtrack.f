*------------------------------------------------------------------------
*       
*       TRG_TRACK  GEN Target Rracking routines 
*       -=========-
*       
*       tracks ELECTRONS through polarized target B field
*
*	Raytracing of 3-d motion in polarized target field by solution
*	of differential equations of motion via 4th order Runge-Kutta. Field 
*	orientation arbitrary. 
*       
*       Note: - the HMS routines use a right handed coord. system with
*       x : pointing downwards
*       y : perpendicular to x,z, 
*                     pointing to the left (if seen in z-direction)
*       z : BEAM axis or HMS axis, pointing downstream or 
*       from the target to the focal plane respectively
*       
*       - the B field map uses a cylindrical coordinate system
*       with z along the field axis and r perpendicular to it
*       
*       - all length (x,y,z,dl,l,...) are measured in [cm]
*       - all velocities are measured in [cm/ns]
*       - all angles are measured counter clock wise in [deg]
*       - time is measured in [ns] 
*       - the B field is measured in [T]
*       
*       original devloped by ???
*       widely modified by MM 
*       - converted into subroutines  
*       - rotation algorytm correced (at moment: phi==0 assumed)
*       - changed coordinate system 
*       beam direction:   z
*       horizontal plane: zy
*       out of plane:     x  (points downwards) 
*       
*       Supplies:
*       trgInit (map,theta,phi) 
*       load the target field map
*       trgTrack (u,E,dl,l)
*       
*       Note: - Before calling trgTrack,trgXTrack or trgTrackToPlane
*       the target field map has to be loaded by a call to 
*       trgInit
*------------------------------------------------------------------------
	
	
*------------------------------------------------------------------------------
*       load the field map and calculate the magnetic field strength  
*       
	SUBROUTINE trgInit(map)
	IMPLICIT NONE
	CHARACTER map*(*)
        include 'sane_data_structures.cmn'
*       --  read field map (for calculations in the LAB system)
*       
*       Parameter:
*       map        I : filename of the fieldmap (=' ': uniform field test case)
*       
*       note: currently phi is always treated as 0
*       

	INTEGER    nz,nr 
	PARAMETER (nz = 337)
	PARAMETER (nr = 337)
	
	REAL*8   B_field_z(nz,nr),B_field_r(nz,nr),zz(nz),rr(nr)
	REAL*8   B_theta,B_stheta,B_ctheta,B_phi,B_sphi,B_cphi 
 
	COMMON  /trgFieldStrength/ B_field_z,B_field_r,zz,rr
	COMMON  /trgFieldAngles/   B_theta,B_stheta,B_ctheta,
     ,   B_phi,  B_sphi,  B_cphi 
 
	REAL*8      pi180
	PARAMETER (pi180 = 3.141592653/180.) 
	
	INTEGER ir,iz 
	REAL*8   xx, scale
  
	
				! if desired target field is 0, set it to nominal 5.1
				! if it is -999.9 set it to 0
c      scale = 10.0d0  ! return field in kG for GEANT, not T

	if (SANE_TGTFIELD_B .eq. 0.0) then
	   scale = 1.0
	   print *,"  f-f-f-f  target field NOT rescaled  f-f-f-f"
	elseif (SANE_TGTFIELD_B.eq. -999.9) then
	   scale = 0.0
	 print *,"  f-f-f-f  target field scaled to 0  f-f-f-f"
	else
	   scale = SANE_TGTFIELD_B/5.1
	   print *,"  f-f-f-f  target field scaled to ",
     ,	"  f-f-f-f"
	endif 
c	scale=10000
	
	IF (map .NE. ' ') THEN	!read in numerical field map
	   write(*,*)'OPENING MAP FILE =',map
	   OPEN (unit=1,file=map,status='old')
	   DO ir=1,nr
	      rr(ir) = 2.*float(ir-1)
	      zz(ir) = 2.*float(ir-1)
	      DO iz=1,nz
		 READ (1,*)xx,xx,B_field_z(iz,ir),B_field_r(iz,ir),xx,xx,xx
				! rescale field to desired value
		 B_field_z(iz,ir) = B_field_z(iz,ir) * scale
	      B_field_r(iz,ir) = B_field_r(iz,ir) * scale
	   ENDDO
        ENDDO
        CLOSE (unit=1)
	ELSE
	   DO ir=1,nr		! uniform 5T field over 26 cm in z
          rr(ir) = 2.*float(ir-1)	! and 16 cm in r
          zz(ir) = 2.*float(ir-1)
          DO iz=1,nz
	     B_field_r(iz,ir) = 0.
            IF (rr(ir) .LE. 16. .and. zz(ir) .LE. 26.) THEN
              B_field_z(iz,ir) = SANE_TGTFIELD_B 
            ELSE
	      B_field_z(iz,ir) = 0.0
	    ENDIF
	  ENDDO
	ENDDO
      ENDIF
      
      write(*,*) 'Target field : ',SANE_TGTFIELD_B 
      write(*,*) 'Target field initiated with file: ',map

      RETURN
      END
      
* ******************************************************
	Subroutine trgInitFieldANGLES(theta,phi)
*       theta,phi  I : inplane (theta) and out of plane (phi) angle
	IMPLICIT NONE
	REAL*8     theta,phi
	
	REAL*8   B_theta,B_stheta,B_ctheta,B_phi,B_sphi,B_cphi 
	COMMON  /trgFieldAngles/   B_theta,B_stheta,B_ctheta,
     >                           B_phi,  B_sphi,  B_cphi 
	REAL*8      pi180,p,p0
	PARAMETER (pi180 = 3.141592653/180.) 
c	write(*,*) 'target theta = ',theta
	B_theta  = theta
	B_stheta = SIN(theta*pi180)*cos(phi*pi180) 
	B_ctheta = COS(theta*pi180)
	
	! Note: for performance reasons B_phi is always treated 0 in trgField
	B_phi    = phi
	B_sphi   = SIN(phi*pi180) 
	B_cphi   = COS(phi*pi180)
	end

       

*------------------------------------------------------------------------
      subroutine TransformTo6Vector(x,y,z,px,py,pz,E,U)
cc
c
c     x,y,z coordinates ! Coor should be in z - along beamline
c                                           y - along the target insert
c                                           x perpendicular
c     px,py,pz - momentum components in GEV
c     E        - energy in GEV
c     U(6) is six dimentional vector used in Tracking
c
cc      
      real*8 U(6)
      real*4 x,y,z,px,py,pz,E
      
      U(1) = y
      U(2) = x
      U(3) = z
      U(4) = py/E*29.97
      U(5) = px/E*29.97
      U(6) = pz/E*29.97
      end
      subroutine TransformFROM6Vector(PCooR,PMom,E,U)
cc
c
c     x,y,z coordinates ! Coor should be in z - along beamline
c                                           y - along the target insert
c                                           x perpendicular
c     px,py,pz - momentum components in GEV
c     E        - energy in GEV
c     U(6) is six dimentional vector used in Tracking
c
cc      
      real*8 U(6)
      real*4 PCoor(3),PMom(3),E
      
      PCoor(2)  = U(1) 
      PCoor(1)  = U(2) 
      PCoor(3)  = U(3) 
      Pmom(2)   = U(4)*E/29.97
      Pmom(1)   = U(5)*E/29.97
      Pmom(3)   = U(6)*E/29.97
      end

      subroutine TransformFrom6VectorC(x,y,z,px,py,pz,E,U)
cc
c     Transforms from 6 Vector from tracking to XYZ,PX,PY,PZ 
c     x,y,z coordinates ! Coor should be in z - along beamline
c                                           y - along the target insert
c                                           x perpendicular
c     px,py,pz - momentum components in GEV
c     E        - energy in GEV
c     U(6) is six dimentional vector used in Tracking
c
cc      
      real*8 U(6)
      real*4 x,y,z,px,py,pz,E
      
      y    = U(1)
      x    = U(2) 
      z    = U(3)  
      py   = U(4)*E/29.97
      px   = U(5)*E/29.97
      pz   = U(6)*E/29.97
      end

ccccccccccccccccccccccccccccccccccccccccccccccccc

   
      SUBROUTINE trgTrack (u,E,dl,l)
      IMPLICIT NONE
      REAL*8    u(6),E,dl,l
* --  track a single particle with given start parameters
*
*     Parameter:
*       u   IO : coordinate vector (initial/final)
*                  u(1,2,3) : x, y, z [cm]
*                  u(4,5,6) : dx/dt, dy/dt, dz/dt [cm/ns] 
*       E   I  : particle energy [MeV] * sign of particle charge
*                (negative for electrons, positive for protons/deuterons)
*       dl  I  : step size [cm]
*       l   I  : tracking distance [cm]

      REAL*8  factor
      COMMON /trgConversionFactor/factor

      REAL*8   ts
      INTEGER i,n

      factor = 90./E
      ts     = dl/30.
      n      = ABS(l/dl)

      DO i=1,n 			!step thru time
  	CALL trgRK4(u,u,ts) 	!solve diff. eqs.
      ENDDO

      RETURN
      END

*------------------------------------------------------------------------

      SUBROUTINE trgXTrack (u,E,dl,l,Bdl,Xfun,id)
      IMPLICIT NONE
      REAL*8   u(6),E,dl,l,Bdl
      INTEGER id
      INTEGER  Xfun 
      EXTERNAL Xfun 
*  -- track a single particle with given start parameters and 
*     writes the track's coordinates to a hbook file  
*
*     Parameter:
*       u    IO : coordinate vector (initial/final)
*                   u(1,2,3) : x, y, z [cm]
*                   u(4,5,6) : dx/dt, dy/dt, dz/dt [cm/ns] 
*       E    I  : particle energy [MeV] * sign of particle charge
*                (negative for electrons, positive for protons/deuterons)
*       dl   I  : step size [cm]
*       l    I  : tracking distance [cm]
*       Bdl  O  : Integrated Bdl [Tcm]
*       Xfun I  : external function called after every iteration
*                 int Xfun (id,t,l,u)
*                    int  id         as passed to trgXtrack
*                    real*8t,l,u(9)   time, pathlength and act. coordinates
*       id   I  : histogram id


      REAL*8   factor
      COMMON /trgConversionFactor/factor

      REAL*8   ts,uu(9)
      INTEGER i,n,x

      factor = 90./E
      ts     = dl/30.
      n      = l/dl

      ! book start location
      DO i=1,6
        uu (i) = u(i)
      ENDDO
      DO i=7,9 
        uu (i) = 0.
      ENDDO
      x = Xfun(id,0.,0.,uu)
      
      ! track the particle and book location
      DO i=1,n  
        CALL trgRK4Bdl(uu,uu,ts)     ! solve diff. eqs.
        x = Xfun (id,i*ts,i*dl,uu)
      ENDDO

      DO i=1,6
        u(i) = uu (i)
      ENDDO

      ! calculate Bdl  ( B_x^2+B_y^2+B_z^2 )
      Bdl = SQRT(uu(7)**2+uu(8)**2+uu(9)**2)	 

      RETURN
      END

*------------------------------------------------------------------------

      SUBROUTINE trgTrackToLine (u,E,dl,P1,P2,ok)
      IMPLICIT NONE
      REAL*8   u(6),E,dl,p1(3),P2(3)
      LOGICAL ok
* --  track a single particle with given start parameters
*     and find the closest aproach of the particle track with a given lane
*
*     Parameter:
*        u     IO : coordinate vector (initial/final)
*                     u0(1,2,3) : x, y, z [cm]
*                     u0(4,5,6) : dx/dt, dy/dt, dz/dt [cm/ns] 
*        E     I  : particle energy [MeV] * sign of particle charge
*                   (negative for electrons, positive for protons/deuterons)
*        dl    I  : step size [cm]
*        P1 and P2 I  : two poinst on the lane 
*        ok    IO : status variable 
*                   - if false no action is taken 
*                   - set to false when no intersection point is found 
*                    
                 
      REAL*8  factor
      COMMON /trgConversionFactor/factor
      REAL*8 one, Coordinate(3)
      REAL*8   ts,n,an,bn,cn,dn,maxdist,dist0,dist1,u0(6),u1(6)
      integer idist0,idist1,icount	
      INTEGER i
      one = 1.
      
      IF (.NOT. OK) RETURN   

      factor =  90./E
      ts     = -dl/30.
      Coordinate(1) = u(1)
      Coordinate(2) = u(2)
      Coordinate(3) = u(3)
      call  Dist2Lane(P1,P2,Coordinate,dist0)
      maxdist = max(ABS(dist0)*4.,1.0)
      
      ! check for the tracking direction 
      CALL trgRK4(u,u0,ts)
	do i=1,6
	   u1(i) = u0(i)
	enddo
      
      Coordinate(1) = u1(1)
      Coordinate(2) = u1(2)
      Coordinate(3) = u1(3)
      call Dist2Lane(P1,P2,Coordinate,dist1) 
      IF (dist1.gt.dist0) ts=-ts
         
      ! track through the intersection plane 
      dist0 = dist1 
      icount=0
      DO WHILE (dist1.le.dist0.and.icount.lt.100000)
c      write(*,*)dist0,dist1,icount
	icount=icount+1 
	dist0 = dist1
	do i=1,6
	   u1(i) = u0(i)
	enddo
        CALL trgRK4(u1,u0,ts)
        Coordinate(1) = u0(1)
        Coordinate(2) = u0(2)
        Coordinate(3) = u0(3)
        call Dist2Lane(P1,P2,Coordinate,dist1) 

      ENDDO  
      if(icount.lt.100000)then
	do i=1,6
	   u(i) = u1(i)
	enddo
      endif
      if(icount.gt.100000) write(*,*)'Failed'                  
      RETURN
      END

      SUBROUTINE trgTrackToLineBDL (u,E,dl,P1,P2,ok)
      IMPLICIT NONE
      REAL*8   u(9),E,dl,p1(3),P2(3)
      LOGICAL ok
* --  track a single particle with given start parameters
*     and find the closest aproach of the particle track with a given lane
*
*     Parameter:
*        u     IO : coordinate vector (initial/final)
*                     u0(1,2,3) : x, y, z [cm]
*                     u0(4,5,6) : dx/dt, dy/dt, dz/dt [cm/ns] 
*        E     I  : particle energy [MeV] * sign of particle charge
*                   (negative for electrons, positive for protons/deuterons)
*        dl    I  : step size [cm]
*        P1 and P2 I  : two poinst on the lane 
*        ok    IO : status variable 
*                   - if false no action is taken 
*                   - set to false when no intersection point is found 
*                    
                 
      REAL*8  factor
      COMMON /trgConversionFactor/factor
      REAL*8 one, Coordinate(3)
      REAL*8   ts,n,an,bn,cn,dn,maxdist,dist0,dist1,u0(6),u1(6)
      integer idist0,idist1,icount	
      INTEGER i
      one = 1.
      
      IF (.NOT. OK) RETURN   

      factor =  90./E
      ts     = -dl/30.
      Coordinate(1) = u(1)
      Coordinate(2) = u(2)
      Coordinate(3) = u(3)
      call  Dist2Lane(P1,P2,Coordinate,dist0)
      maxdist = max(ABS(dist0)*4.,1.0)
      
      ! check for the tracking direction 
      CALL trgRK4(u,u0,ts)
	do i=1,6
	   u1(i) = u0(i)
	enddo
      
      Coordinate(1) = u1(1)
      Coordinate(2) = u1(2)
      Coordinate(3) = u1(3)
      call Dist2Lane(P1,P2,Coordinate,dist1) 
      IF (dist1.gt.dist0) ts=-ts
         
      ! track through the intersection plane 
      dist0 = dist1 
      icount=0
      DO WHILE (dist1.le.dist0.and.icount.lt.100000)
c      write(*,*)dist0,dist1,icount
	icount=icount+1 
	dist0 = dist1
	do i=1,6
	   u1(i) = u0(i)
	enddo
        CALL trgRK4(u1,u0,ts)
        Coordinate(1) = u0(1)
        Coordinate(2) = u0(2)
        Coordinate(3) = u0(3)
        call Dist2Lane(P1,P2,Coordinate,dist1) 

      ENDDO  
      if(icount.lt.100000)then
	do i=1,6
	   u(i) = u1(i)
	enddo
      endif
      if(icount.gt.100000) write(*,*)'Failed'                  
      RETURN
      END
c
c
	Subroutine Dist2Lane(PLine1,PLine2,Dot,Dist)
        IMPLICIT NONE
	real*8 PLine1(3),PLine2(3),Dot(3)
	real*8 Vect1(3),Vect2(3),VectCross(3)
	real*8 Dist,val0,val1
	call Sub3Vec(PLine1,PLine2,Vect1)
	call Sub3Vec(PLine1,DOT,Vect2)
	call CROSS(Vect1,Vect2,VectCross)
	call value(VectCross,val0)
	call value(Vect1,val1)
	Dist = val0/val1
	end
c
	Subroutine CROSS(P1,P2,P12)
        IMPLICIT NONE
	real*8 P1(3),P2(3),P12(3)
	p12(1) = P1(2)*P2(3) - P1(3)*P2(2)
	p12(2) = P1(3)*P2(1) - P1(1)*P2(3)
	p12(3) = P1(1)*P2(2) - P1(2)*P2(1)
	end
c
	Subroutine Value(P,val)
        IMPLICIT NONE
	real*8 P(3)
	real*8 Val
	Val = sqrt(P(1)**2+P(2)**2+P(3)**2)
	end
c
	Subroutine Sub3Vec(P1,P2,P12)
        IMPLICIT NONE
	real*8 P1(3),P2(3),P12(3)
	P12(1) = P1(1) - P2(1)
	P12(2) = P1(2) - P2(2)
	P12(3) = P1(3) - P2(3)
	end
	
*------------------------------------------------------------------------

      SUBROUTINE trgTrackToPlaneBDL (u,E,dl,a,b,c,d,ok)
      IMPLICIT NONE
!      REAL    u(6),E,dl,a,b,c,d 
      REAL*8    u(9),E,dl,a,b,c,d 
      LOGICAL ok
* --  track a single particle with given start parameters
*     and find the intersection of the particle track with a given plane
*
*     Parameter:
*        u     IO : coordinate vector (initial/final)
*                     u0(1,2,3) : x, y, z [cm]
*                     u0(4,5,6) : dx/dt, dy/dt, dz/dt [cm/ns] 
*        E     I  : particle energy [MeV] * sign of particle charge
*                   (negative for electrons, positive for protons/deuterons)
*        dl    I  : step size [cm]
*        a..d  I  : parameter of the intersection plane 
*                   0 = a*x+b*y+c*z+d; 
*        ok    IO : status variable 
*                   - if false no action is taken 
*                   - set to false when no intersection point is found 
*                    
c
      INCLUDE 'gen_event_info.cmn'
      logical outside_fieldmap
      common /mkjtemp/ outside_fieldmap
c                 
      REAL*8   factor
      COMMON /trgConversionFactor/factor

!      REAL    ts,n,an,bn,cn,dn,maxdist,dist0,dist1,u0(6),u1(6)
      REAL*8    ts,n,an,bn,cn,dn,maxdist,dist0,dist1,u0(9),u1(9)
       
      INTEGER i,steps,max_steps
!	For Bdl
	do i=7,9
	   u(i)=0.0
	   u0(i)=0.0
	   u1(i)=0.0
	end do

      IF (.NOT. OK) RETURN   
        
	n  = 1/SQRT (a*a+b*b+c*c)
	an = a*n
	bn = b*n
	cn = c*n
	dn = d*n
    
	factor =  90./E
!       ts     = -dl/30.
	ts     = -dl/sqrt(u(4)**2+u(5)**2+u(6)**2) ! to match MC OR - 4/04
	
	
	dist0   = u(1)*an + u(2)*bn + u(3)*cn + dn
	maxdist = max(ABS(dist0)*4.,1.0)
	
				! check for the tracking direction 
!      CALL trgRK4(u,u1,ts)
	CALL trgRK4Bdl(u,u1,ts)
	dist1 = u1(1)*an + u1(2)*bn + u1(3)*cn + dn  
	IF ((sign(1.d00,dist0) .EQ. sign(1.d00,dist1)) .AND.
     >    (ABS(dist0) .LT. ABS(dist1))) ts=-ts
         
      ! track through the intersection plane
      steps=0
      max_steps = int(max(dist0,10.*dl)/dl)*10
      if (sign(1.d00,dist0) .EQ. sign(1.d00,dist1)) then
      dist1 = dist0   
      DO WHILE ((sign(1.d00,dist0) .EQ. sign(1.d00,dist1)) .AND. ok) 
!        CALL trgRK4(u1,u0,ts)
        CALL trgRK4Bdl(u1,u0,ts)
        dist0 = u0(1)*an + u0(2)*bn + u0(3)*cn + dn 
        IF (sign(1.d00,dist0) .EQ. sign(1.d00,dist1)) THEN
!          CALL trgRK4(u0,u1,ts)
          CALL trgRK4Bdl(u0,u1,ts)
          dist1 = u1(1)*an + u1(2)*bn + u1(3)*cn + dn  
        ENDIF
        ok = (ABS(dist1) .LT. maxdist) .and. steps .lt. max_steps
        steps = steps + 1
      ENDDO  
      else
         do i=1,6
            u0(i) = u(i)
         enddo
      endif
      


      IF (ok) THEN        
        ! calculate the intersection point
        DO i=1,6
          u(i) = u0(i) + (u1(i)-u0(i)) * dist0/(dist0-dist1)
        ENDDO

!	Bdl

	do i=7,9
          u(i) = u0(i) + (u1(i)-u0(i)) * dist0/(dist0-dist1)
!	u(i)=u0(i)
	end do

      ENDIF
                  
      RETURN
      END
	
*------------------------------------------------------------------------

      SUBROUTINE trgTrackToPlane (u,E,dl,a,b,c,d,ok)
      IMPLICIT NONE
      REAL*8   u(6),E,dl,a,b,c,d 
      LOGICAL ok
* --  track a single particle with given start parameters
*     and find the intersection of the particle track with a given plane
*
*     Parameter:
*        u     IO : coordinate vector (initial/final)
*                     u0(1,2,3) : x, y, z [cm]
*                     u0(4,5,6) : dx/dt, dy/dt, dz/dt [cm/ns] 
*        E     I  : particle energy [MeV] * sign of particle charge
*                   (negative for electrons, positive for protons/deuterons)
*        dl    I  : step size [cm]
*        a..d  I  : parameter of the intersection plane 
*                   0 = a*x+b*y+c*z+d; 
*        ok    IO : status variable 
*                   - if false no action is taken 
*                   - set to false when no intersection point is found 
*                    
                 
      REAL*8  factor
      COMMON /trgConversionFactor/factor
      REAL*8 one
      REAL*8   ts,n,an,bn,cn,dn,maxdist,dist0,dist1,u0(6),u1(6)
      integer idist0,idist1	
      INTEGER i
      one = 1.
      IF (.NOT. OK) RETURN   
        
      n  = 1/SQRT (a*a+b*b+c*c)
      an = a*n
      bn = b*n
      cn = c*n
      dn = d*n
    
      factor =  90./E
      ts     = -dl/30.

      dist0   = u(1)*an + u(2)*bn + u(3)*cn + dn
      maxdist = max(ABS(dist0)*4.,1.0)
      
      ! check for the tracking direction 
      CALL trgRK4(u,u1,ts)
      dist1 = u1(1)*an + u1(2)*bn + u1(3)*cn + dn  
       IF ((SIGN(one,dist0) .EQ. SIGN(one,dist1)) .AND.
     >    (ABS(dist0) .LT. ABS(dist1))) ts=-ts
         
      ! track through the intersection plane 
      dist1 = dist0   
      DO WHILE ((SIGN(one,dist0) .EQ. SIGN(one,dist1)) .AND. ok) 
        CALL trgRK4(u1,u0,ts)
        dist0 = u0(1)*an + u0(2)*bn + u0(3)*cn + dn 

        IF (SIGN(one,dist0) .EQ. SIGN(one,dist1)) THEN
          CALL trgRK4(u0,u1,ts)
          dist1 = u1(1)*an + u1(2)*bn + u1(3)*cn + dn  
        ENDIF
        ok = (ABS(dist1) .LT. maxdist) 
      ENDDO  
      
      IF (ok) THEN        
        ! calculate the intersection point
        DO i=1,6
          u(i) = u0(i) + (u1(i)-u0(i)) * dist0/(dist0-dist1)
        ENDDO
      ENDIF
                  
      RETURN
      END
	
*------------------------------------------------------------------------

      SUBROUTINE trgField (x_,B_)
      IMPLICIT NONE
      REAL*8 x_(3),B_(3)
* --  calculate actual field
*
*     Parameter:
*        x_   I : lab coordinates  
*        B_   O : B field in lab coordinates
*
*      Notes:
*      - 2-Dimensional Linear Interpolation:                               
*        Assumes uniform spacing of fieldmap in x,y        
*      - for performance reasons B_phi is always treated 0 
       
      INTEGER    nz,nr 
      PARAMETER (nz = 337)
      PARAMETER (nr = 337)

      REAL*8   B_field_z(nz,nr),B_field_r(nz,nr),zz(nz),rr(nr)
      REAL*8   B_theta,B_stheta,B_ctheta,B_phi,B_sphi,B_cphi 
       
      COMMON  /trgFieldStrength/ B_field_z,B_field_r,zz,rr
      COMMON  /trgFieldAngles/   B_theta,B_stheta,B_ctheta,
     >                           B_phi,  B_sphi,  B_cphi 

      INTEGER i,j
      REAL*8   x(3),B(3),z,r,az,ar,a0,a1
     
      ! rotate to coordinates with z' along field direction
      x(1) =           x_(1)
      x(2) =  B_stheta*x_(3) + B_ctheta*x_(2)
      x(3) =  B_ctheta*x_(3) - B_stheta*x_(2)  
        
      ! compute zylinder coordinates
      z  = ABS  (x(3))
      r  = SQRT (x(1)**2 + x(2)**2)
        
      ! interpolate the field map 
      i = INT((z-zz(1))/(zz(2)-zz(1))) + 1                                              
      j = INT((r-rr(1))/(rr(2)-rr(1))) + 1                                              
      IF ((i+1 .GT. nz) .OR. (i .LT. 1) .OR. 
     >    (j+1 .GT. nr) .OR. (j .LT. 1)) THEN                
        B(1)=0.
        B(2)=0.
        B(3)=0.
        B_(1)=0.
        B_(2)=0.
        B_(3)=0.
      ELSE                                                                     
        ! calculate the Bz component 
        az = ((z-zz(i))/(zz(2)-zz(1))) 
        ar = ((r-rr(j))/(rr(2)-rr(1))) 
        a0=az*(B_field_z(i+1,j)  -B_field_z(i,j))  +B_field_z(i,j)                                           
        a1=az*(B_field_z(i+1,j+1)-B_field_z(i,j+1))+B_field_z(i,j+1)                                           
        B(3) = (ar*(a1-a0)+a0)           
        IF (r .gt. 0.) THEN
          ! calculate the Bx,By components 
          a0=az*(B_field_r(i+1,j)  -B_field_r(i,j))  +B_field_r(i,j)                                           
          a1=az*(B_field_r(i+1,j+1)-B_field_r(i,j+1))+B_field_r(i,j+1)                                           
          B(2) = (ar*(a1-a0)+a0)/r
          IF (x(3) .LT. 0.) B(2)= -B(2)
          B(1) = B(2)*x(1)
          B(2) = B(2)*x(2)       
           
          ! transform B field to lab. system
          B_(1) =            B(1)  
          B_(2) = - B_stheta*B(3) + B_ctheta*B(2)
          B_(3) =   B_ctheta*B(3) + B_stheta*B(2)  
        ELSE  
          B_(1) =   0.
          B_(2) = - B_stheta*B(3)
          B_(3) =   B_ctheta*B(3)
        ENDIF
      ENDIF	   
       
      RETURN
      END

*------------------------------------------------------------------------------
* solve the differential equation of the particle  
*
      SUBROUTINE trgDeriv(u,dudt)
      IMPLICIT NONE
      REAL*8 u(9),dudt(9)
* --  calculate the derivatives du(i)/dt for the runke kutta routine         
*
*     Parameter:
*       u     I : actual coordinate vector
*                   u(1,2,3)    I : x, y, z
*                   u(4,5,6)    I : dx/dt, dy/dt, dz/dt 
*                   u(7,8,9)    I : integral Bxdx, Bydy, Bzdz   
*       dudt  O : derivative du/dt
*                   dudt(1,2,3) : dx/dt, dy/dt, dz/dt 
*                   dudt(4,5,6) : d^2xdt^2, d^2ydt^2, d^2zdt^2
*                   dudt(7,8,9) : B x v

      REAL*8  factor
      COMMON /trgConversionFactor/factor

      REAL*8  B(3)

      CALL trgField (u,B)

      ! These are just the velocities
      dudt(1) = u(4)
      dudt(2) = u(5)
      dudt(3) = u(6)

      ! This is just (v_vec X B_vec)  
      dudt(7) = u(5)*B(3) - u(6)*B(2)
      dudt(8) = u(6)*B(1) - u(4)*B(3) 
      dudt(9) = u(4)*B(2) - u(5)*B(1)  

      ! This is just (v_vec X B_vec) * factor
      dudt(4) = dudt(7)*factor
      dudt(5) = dudt(8)*factor
      dudt(6) = dudt(9)*factor

      RETURN
      END
        
*------------------------------------------------------------------------------
                                                                     
      SUBROUTINE trgRK4(u0,u1,h)
      IMPLICIT NONE
      REAL*8    u0(6),u1(6),h
* --  Fourth-order Runge-Kutta from Numerical Recipes book
*     for tracking through the target field 
*
*     Parameter:
*       u0  I  : input  coordinate vector
*       u1  O  : output coordinate vector
*                u(1,2,3) : x, y, z
*                u(4,5,6) : dx/dt, dy/dt, dz/dt 
*       h   I  : time step
  
      INTEGER i
      REAL*8   ut(6),dudt(9),dut(9),dum(9),hh,h6

      hh=h*0.5
      h6=h/6.
 
      CALL trgDeriv(u0,dudt)
      DO i=1,6
	ut(i) = u0(i) + hh*dudt(i)
      ENDDO

      CALL trgDeriv(ut,dut)
      DO i=1,6
	ut(i) = u0(i) + hh*dut(i)
      ENDDO

      CALL trgDeriv(ut,dum)
      DO i=1,6
	ut(i) = u0(i) +h*dum(i)
        dum(i)= dut(i)  +dum(i)
      ENDDO

      CALL trgDeriv(ut,dut)
      DO i=1,6
        u1(i)=u0(i)+h6*(dudt(i)+dut(i)+2.*dum(i))
      ENDDO

      RETURN       
      END
 
*------------------------------------------------------------------------------

      SUBROUTINE trgRK4Bdl(u0,u1,h)
      IMPLICIT NONE
      REAL*8    u0(9),u1(9),h
* --  Fourth-order Runge-Kutta from Numerical Recipes book
*     for tracking through the target field (incl. B/dl calculation)
*
*     Parameter:
*      u0  I  : input  coordinate vector
*      u1  O  : output coordinate vector
*                 u(1,2,3) : x, y, z
*                 u(4,5,6) : dx/dt, dy/dt, dz/dt 
*                 u(7,8,9) : integral Bxdx, Bydy, Bzdz   
*      h   I  : time step
  
      INTEGER i
      REAL*8   ut(9),dudt(9),dut(9),dum(9),hh,h6

      hh=h*0.5
      h6=h/6.
 
      CALL trgDeriv(u0,dudt)
      DO i=1,9
	ut(i) = u0(i) + hh*dudt(i)
      ENDDO

      CALL trgDeriv(ut,dut)
      DO i=1,9
	ut(i) = u0(i) + hh*dut(i)
      ENDDO

      CALL trgDeriv(ut,dum)
      DO i=1,9
	ut(i) = u0(i) +h*dum(i)
        dum(i)= dut(i)  +dum(i)
      ENDDO

      CALL trgDeriv(ut,dut)
      DO i=1,9
        u1(i)=u0(i)+h6*(dudt(i)+dut(i)+2.*dum(i))
      ENDDO

      RETURN       
      END
