
      SUBROUTINE G_XYZ_sph(Tx,Ty,Tz,Tr,Ttheta,Tphi)
*----------------------------------------------------------------------
*- 
*-   Purpose and Methods : convert cartesian coord.s to spherical
*- 
*-   Inputs  : Tx	- X coord. (conventional right handed system) 
*-             Ty	- Y 
*-             Tz	- Z 
*-   Outputs : Tr	- conventional radial coord.
*-             Ttheta	-              theta angle (radians 0-TT)
*-	       Tphi	-              phi angle (radians 0-2*TT)
*- 
*-   Created  25-MAR-1992   Kevin B. Beard 
*-   Modified 19-OCT-1992   KBB	(fix phi)
*-   Modified for hall C 9/1/93: KBB
*     $Log: g_xyz_sph.f,v $
*     Revision 1.1  1994/02/09 14:18:59  cdaq
*     Initial revision
*
*- 
*----------------------------------------------------------------------
      IMPLICIT NONE
      real Tx,Ty,Tz,Tr,Ttheta,Tphi
      real xyprj
      logical off_orig,off_z_axis
      INCLUDE 'gen_constants.par'
*----------------------------------------------------------------------
*
	Tr= SQRT(Tx**2 + Ty**2 + Tz**2)
	off_orig= Tr .GT. 0.			!not at origin
	IF(off_orig) THEN
	  Ttheta= ATAN2( SQRT(Tx**2 + Ty**2), Tz )
	  off_z_axis= ABS(Tx).GT.0. .or. ABS(Ty).GT.0.
	  If(off_z_axis) Then
	    Tphi= ATAN2( Ty, Tx )		!defined from -TT to +TT
	    if(Tphi.LT.0.) Tphi= Tphi + 2*TT
	  Else
	    Tphi= 0.
	  EndIf
	ELSE
	  Tr= 0.
	  Ttheta= 0.
	  Tphi= 0.
	ENDIF
*
	RETURN
	end
