      subroutine H_TRACK_FIT(ABORT,err,ierr)
*     primary track fitting routine for the HMS spectrometer
*
*     Called by H_TRACK
*
*     d.f. geesaman         17 January 1994
*     modified 
*                           17 Feb 1994        dfg
*                              remove minuit. Make fit linear
*                              still does not do errors properly
* $Log$
* Revision 1.2  1994/02/22 05:25:00  cdaq
* (SAW) Remove dfloat calls with floating args
*
* Revision 1.1  1994/02/19  06:20:53  cdaq
* Initial revision
*
*
      implicit none
      include "gen_data_structures.cmn"
      include "hms_tracking.cmn"
      include "hms_geometry.cmn"
*
*
*     local variables
*
      logical ABORT
      character*50 here
      parameter (here='H_TRACK_FIT')
      character*(*) err
      integer*4 itrack                        ! track loop index
      integer*4 ihit,ierr
      integer*4 hit,plane
      integer*4 i,j                             ! loop index
      real*4   ray(hnum_fpray_param)
      real*8 TT(hnum_fpray_param)
      real*8 AA(hnum_fpray_param,hnum_fpray_param)      
      real*8 dray(hnum_fpray_param)
      real*8 error(hnum_fpray_param)
      real*4 chi2,dummychi2
      parameter (dummychi2 = 1.E4)
*     array to remap hplane_coeff to param number
      integer*4 remap(hnum_fpray_param)
      data remap/5,6,3,4/
      save remap
*
      ABORT= .FALSE.
      ierr=0
*     test for no tracks
      if(HNTRACKS_FP.ge.1) then
        do itrack=1,HNTRACKS_FP
          chi2= dummychi2
          htrack_fit_num=itrack
*     are there enough degrees of freedom
          HNFREE_FP(itrack)=HNTRACK_HITS(itrack,1)-hnum_fpray_param
          if(HNFREE_FP(itrack).gt.0) then
*     initialize parameters
           do i=1,hnum_fpray_param
            TT(i)=0.
            do ihit=2,HNTRACK_HITS(itrack,1)+1
              hit=HNTRACK_HITS(itrack,ihit)
              plane=HDC_PLANE_NUM(hit)
              TT(i)=TT(i)+((HDC_WIRE_COORD(hit)*
     &          hplane_coeff(remap(i),plane))
     &          /(hdc_sigma(plane)*hdc_sigma(plane)))
            enddo 
           enddo
          do i=1,hnum_fpray_param
           do j=1,hnum_fpray_param
           AA(i,j)=0.
            do ihit=2,HNTRACK_HITS(itrack,1)+1
              hit=HNTRACK_HITS(itrack,ihit)
              plane=HDC_PLANE_NUM(hit)
              AA(i,j)=AA(i,j) + (
     &           hplane_coeff(remap(i),plane)*hplane_coeff(remap(j),plane)
     &          /(hdc_sigma(plane)*hdc_sigma(plane)))
            enddo               ! end loop on ihit
           enddo                ! end loop on j
          enddo                 ! end loop on i
*
*     solve four by four equations
         call solve_four_by_four(TT,AA,dray,ierr)
*
      if(ierr.ne.0) then
         dray(1)=10000.
         dray(2)=10000.
         dray(3)=2.
         dray(4)=2.
      else
*      calculate chi2
        chi2=0.
        ray(1)=dray(1)
        ray(2)=dray(2)
        ray(3)=dray(3)
        ray(4)=dray(4)
         do ihit=2,HNTRACK_HITS(itrack,1)+1
              hit=HNTRACK_HITS(itrack,ihit)
              plane=HDC_PLANE_NUM(hit)
* note chi2 is single precision
           chi2=chi2+
     &         (HDC_WIRE_COORD(hit)
     &         -hplane_coeff(remap(1),plane)*ray(1)
     &         -hplane_coeff(remap(2),plane)*ray(2)
     &         -hplane_coeff(remap(3),plane)*ray(3)
     &         -hplane_coeff(remap(4),plane)*ray(4))**2
     &         /(hdc_sigma(plane)*hdc_sigma(plane))
        enddo
      endif

           HX_FP(itrack)=ray(1)
           HY_FP(itrack)=ray(2)
           HZ_FP(itrack)=0.                   ! z=0 of tracking.
           HXP_FP(itrack)=ray(3)
           HYP_FP(itrack)=ray(4)
          endif                               ! end test on degrees of freedom
          HCHI2_FP(itrack)=chi2
        enddo                                 ! end loop over tracks
      endif
*     test if we want to dump out trackfit results
      if(hdebugtrackprint.ne.0) then
         call h_print_tracks
      endif                                   ! end test on zero tracks
1000  return
      end

*

