      subroutine H_FCNCHISQ(npar,grad,fval,ray,iflag,dummy)
*     This subroutine calculates chi**2 for MINUIT for HMS. The
*     arguments are determined by MINUIT
*
*     d.f. geesaman             17 January 1994
* $Log$
* Revision 1.1  1994/02/19 06:14:15  cdaq
* Initial revision
*
*
      implicit none
      external H_DPSIFUN
      real*8 H_DPSIFUN
      include "gen_data_structures.cmn"
      include "hms_tracking.cmn"
      include "hms_geometry.cmn"
*
*     input
      real*8 ray(*),grad(*),dummy
      integer*4 npar,iflag
*     output
      real*8 fval                              ! value of chi2
*
*     local variables
      real*8 diff
      integer*4 ihit
      integer*4 hitnum,planenum

      fval=0.0d0
      do ihit=1,HNTRACK_HITS(htrack_fit_num,1)
         hitnum=HNTRACK_HITS(htrack_fit_num,ihit+1)
         planenum=HDC_PLANE_NUM(hitnum)
         diff=(dble(HDC_WIRE_COORD(hitnum))-H_DPSIFUN(ray,planenum))
     &        /dble(hdc_sigma(planenum))
         fval=fval+diff*diff
      enddo
      return
      end
