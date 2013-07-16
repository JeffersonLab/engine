
      SUBROUTINE G_sph_XYZ(r,theta,phi,x,y,z)
*----------------------------------------------------------------------
*- 
*-   Purpose and Methods : convert spherical coord.s to cartesian
*- 
*-   Inputs  : r	- conventional radial coord.
*-             theta	-              theta angle (radians)
*-	       phi	-              phi 
*-   Outputs : x	- X coord. (conventional right handed system) 
*-             y	- Y 
*-             z	- Z 
*- 
*-   Created  27-MAR-1992   Kevin B. Beard 
*-   Modified for hall C 9/1/93: KBB
*     $Log: g_sph_xyz.f,v $
*     Revision 1.1  1994/02/09 14:18:28  cdaq
*     Initial revision
*
*- 
*----------------------------------------------------------------------
      IMPLICIT NONE
      real r,theta,phi,x,y,z 
*----------------------------------------------------------------------
*
	x= r*SIN(theta)*COS(phi)
	y= r*SIN(theta)*SIN(phi)
	z= r*COS(theta) 
*
	RETURN
	end 
