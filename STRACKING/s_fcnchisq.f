      subroutine S_FCNCHISQ(npar,grad,fval,ray,iflag,dummy)
*     This subroutine calculates chi**2 for MINUIT. The
*     arguments are determined by MINUIT
*
*     d.f. geesaman             8 September 1993
*     modified   dfg            14 Feb 1993   Change SPLANE_PARAM to 
*                                             sdc_sigma
* $Log$
* Revision 1.1  1994/02/21 16:13:20  cdaq
* Initial revision
*
*
      implicit none
      external S_DPSIFUN
      real*8 S_DPSIFUN
      include "gen_data_structures.cmn"
      include "sos_tracking.cmn"
      include "sos_geometry.cmn"
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
      do ihit=1,SNTRACK_HITS(strack_fit_num,1)
         hitnum=SNTRACK_HITS(strack_fit_num,ihit+1)
         planenum=SDC_PLANE_NUM(hitnum)
         diff=(dble(SDC_WIRE_COORD(hitnum))-S_DPSIFUN(ray,planenum))
     &        /dble(sdc_sigma(planenum))
         fval=fval+diff*diff
      enddo
      return
      end
