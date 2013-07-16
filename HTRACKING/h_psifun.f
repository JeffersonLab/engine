      function h_psifun(ray,iplane)
*     this function calculates the psi coordinate of the intersection
*     of a ray (defined by ray) with a wire chamber plane. the geometry
*     of the plane is contained in the coeff array calculated in the
*     array splane_coeff
*
*     the ray is defined by
*     x = (z-zt)*tan(xp) + xt
*     y = (z-zt)*tan(yp) + yt
*      at some fixed value of zt*
*     ray(1) = xt
*     ray(2) = yt
*     ray(3) = tan(xp)
*     ray(4) = tan(yp)
*
*     d.f. geesaman                   1 September 1993
* $Log: h_psifun.f,v $
* Revision 1.2  1995/05/22 19:39:24  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.1  1994/02/19  06:17:49  cdaq
* Initial revision
*
*
      implicit none
      include "hms_data_structures.cmn"
      include "hms_geometry.cmn"
*
*     input
      real*4 ray(4)           ! xt,yt,xpt,ypt
      integer*4 iplane        ! plane number
*     output
      real*4 H_PSIFUN         ! value of psi coordinate of hit of ray in plane
*
*     local variables   
      real*4 denom,infinity,cinfinity
      parameter (infinity = 1.0d20)
      parameter (cinfinity = 1/infinity)
*
      H_PSIFUN =  ray(3)*ray(2)*hplane_coeff(1,iplane) 
     &        + ray(4)*ray(1)*hplane_coeff(2,iplane)
     &        + ray(3)*hplane_coeff(3,iplane) 
     &        + ray(4)*hplane_coeff(4,iplane)
     &        + ray(1)*hplane_coeff(5,iplane) 
     &        + ray(2)*hplane_coeff(6,iplane)
*
      denom = ray(3)*hplane_coeff(7,iplane) 
     &      + ray(4)*hplane_coeff(8,iplane) + hplane_coeff(9,iplane)
*
      if(abs(denom).lt.cinfinity) then
          H_PSIFUN=infinity
      else
          H_PSIFUN = H_PSIFUN/denom
      endif
      return
      end  
