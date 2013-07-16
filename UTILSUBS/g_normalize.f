
      SUBROUTINE G_normalize(x,y,z)
*----------------------------------------------------------------------
*- 
*-   Purpose and Methods : normalizes a vector 
*- 
*-   Inputs  : x	- X coord. (conventional right handed system) 
*-             y	- Y 
*-             z	- Z 
*-   Outputs : x	- X coord. (conventional right handed system) 
*-             y	- Y 
*-             z	- Z 
*- 
*-   Created  24-MAR-1992   Kevin B. Beard 
*-   Modified for hall C 9/1/93: KBB
*     $Log: g_normalize.f,v $
*     Revision 1.1  1994/02/09 14:16:38  cdaq
*     Initial revision
*
*- 
*----------------------------------------------------------------------
      IMPLICIT NONE
      real x,y,z,r 
      logical normalizable 
      real nothing
      parameter (nothing= 1.E-25)
      real r2
*----------------------------------------------------------------------
*
	r2= x**2 + y**2 + z**2
	normalizable= r2.GT.nothing
	IF(normalizable) THEN 
	  r= SQRT(r2)
	  x= x/r
	  y= y/r
	  z= z/r
	ELSE
	  x= 0. 
	  y= 0. 
	  z= 0. 
	ENDIF 
	RETURN
	end 
