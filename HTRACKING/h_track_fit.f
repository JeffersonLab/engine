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
* Revision 1.6  1994/12/06 15:45:27  cdaq
* (DJM) Take slices in Z to look for best focus
*
* Revision 1.5  1994/10/12  18:52:06  cdaq
* (DJM) Initialize some variables
* (SAW) Prettify indentation
*
* Revision 1.4  1994/09/01  12:29:07  cdaq
* (DJM) Make registered versions of residuals
*
* Revision 1.3  1994/08/18  02:45:53  cdaq
* (DM) Add calculation of residuals
*
* Revision 1.2  1994/02/22  05:25:00  cdaq
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
      external H_DPSIFUN
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
      integer*4 i,j,k                             ! loop index
      real*4 z_slice

      real*8   H_DPSIFUN
      real*8   pos
      real*4   ray(hnum_fpray_param)
      real*8   ray1(4)
      real*8   ray2(4)
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
*  initailize residuals

      do itrack=1,HNTRACKS_MAX
        do plane=1,HMAX_NUM_DC_PLANES
          hdc_residual(itrack,plane)=1000
          hdc_sing_res(itrack,plane)=1000
          hdc1_sing_res(plane)=1000
          hdc2_sing_res(plane)=1000
          hdc1_dbl_res(plane)=1000
          hdc2_dbl_res(plane)=1000

        enddo
      enddo


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
     &               hplane_coeff(remap(i),plane))
     &               /(hdc_sigma(plane)*hdc_sigma(plane)))
              enddo 
            enddo
            do i=1,hnum_fpray_param
              do j=1,hnum_fpray_param
                AA(i,j)=0.
                if(j.lt.i)then
                  AA(i,j)=AA(j,i)
                else 
                  do ihit=2,HNTRACK_HITS(itrack,1)+1
                    hit=HNTRACK_HITS(itrack,ihit)
                    plane=HDC_PLANE_NUM(hit)
                    AA(i,j)=AA(i,j) + (
     &                   hplane_coeff(remap(i),plane)*hplane_coeff(remap(j)
     $                   ,plane)/(hdc_sigma(plane)*hdc_sigma(plane)))
                  enddo                 ! end loop on ihit
                endif                   ! end test on j .lt. i
              enddo                     ! end loop on j
            enddo                       ! end loop on i
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
*     calculate chi2
              chi2=0.
              
              ray(1)=dray(1)
              ray(2)=dray(2)
              ray(3)=dray(3)
              ray(4)=dray(4)
              do ihit=2,HNTRACK_HITS(itrack,1)+1
                hit=HNTRACK_HITS(itrack,ihit)
                plane=HDC_PLANE_NUM(hit)

* note chi2 is single precision
                hdc_sing_res(itrack,plane)=
     &               (HDC_WIRE_COORD(hit)
     &               -hplane_coeff(remap(1),plane)*ray(1)
     &               -hplane_coeff(remap(2),plane)*ray(2)
     &               -hplane_coeff(remap(3),plane)*ray(3)
     &               -hplane_coeff(remap(4),plane)*ray(4))
                chi2=chi2+((hdc_sing_res(itrack,plane))**2
     &               )/(hdc_sigma(plane)*hdc_sigma(plane))
*djm
                if(itrack.eq.1 .and. plane.ge.1 .and. plane.le.6)
     &               hdc1_sing_res(plane) = hdc_sing_res(itrack,plane)
                if(itrack.eq.2 .and. plane.ge.7 .and. plane.le.12)
     &               hdc2_sing_res(plane) = hdc_sing_res(itrack,plane)
              enddo
            endif

            HX_FP(itrack)=ray(1)
            HY_FP(itrack)=ray(2)
            HZ_FP(itrack)=0.            ! z=0 of tracking.
            HXP_FP(itrack)=ray(3)
            HYP_FP(itrack)=ray(4)
          endif                         ! end test on degrees of freedom
          HCHI2_FP(itrack)=chi2
        enddo                           ! end loop over tracks
      endif

*     A reasonable selection of slices is presently -80,-60,-40,-20,0,20,40
*     ,60,80.   Zero is the nominal midplane between the chambers, -80
*     corresponds closely to the exit flange position.  This slice pattern
*     is created with hz_wild=-80., hdelta_z_wild=20., and hnum_zslice=9 
      if(hz_slice_enable.ne.0)then
        do k=1,hnum_zslice
          z_slice = hz_wild + (k-1)*hdelta_z_wild
          hx_fp_wild(k) = hx_fp(1) + hxp_fp(1)*z_slice
          hy_fp_wild(k) = hy_fp(1) + hyp_fp(1)*z_slice
        enddo
      endif

* calculate residuals for each chamber if in single stub mode
* and there were 2 tracks found one in first chanber and one in the second

      if (hsingle_stub.ne.0) then
        if (HNTRACKS_FP.eq.2) then
          itrack=1
          ihit=2
          hit=HNTRACK_HITS(itrack,ihit)
          plane=HDC_PLANE_NUM(hit)
          if (plane.le.6) then
            itrack=2
            hit=HNTRACK_HITS(itrack,ihit)
            plane=HDC_PLANE_NUM(hit)
            if (plane.ge.7) then

* condition of above met calculating residuals  
* assigning rays to tracks in each chamber
* ray1 is ray from first chamber fit
* ray2 is ray from second chamber fit

              ray1(1)=dble(HX_FP(1))
              ray1(2)=dble(HY_FP(1))
              ray1(3)=dble(HXP_FP(1))
              ray1(4)=dble(HYP_FP(1))
              ray2(1)=dble(HX_FP(2))
              ray2(2)=dble(HY_FP(2))
              ray2(3)=dble(HXP_FP(2))
              ray2(4)=dble(HYP_FP(2))

              itrack=1
* loop over hits in second chamber
              do ihit=1,HNTRACK_HITS(itrack+1,1)

* calculate residual in second chamber from first chamber track
                hit=HNTRACK_HITS(itrack+1,ihit+1)
                plane=HDC_PLANE_NUM(hit)
                pos=H_DPSIFUN(ray1,plane)
                hdc_residual(itrack,plane)=HDC_WIRE_COORD(hit)-pos
* djm 8/31/94 stuff this variable into 1d array we can register
                hdc2_dbl_res(plane) = hdc_residual(1,plane)

              enddo

              itrack=2
* loop over hits in first chamber
              do ihit=1,HNTRACK_HITS(itrack-1,1)

* calculate residual in first chamber from second chamber track
                hit=HNTRACK_HITS(itrack-1,ihit+1)
                plane=HDC_PLANE_NUM(hit)
                pos=H_DPSIFUN(ray2,plane)
                hdc_residual(itrack,plane)=HDC_WIRE_COORD(hit)-pos
* djm 8/31/94 stuff this variable into 1d array we can register
                hdc1_dbl_res(plane) = hdc_residual(2,plane)

              enddo
            endif                       ! end plane ge 7
          endif                         ! end plane le 6
        endif                           ! end HNTRACKS_FP eq 2
      endif                             ! end hsignle_stub .ne. 0

*     test if we want to dump out trackfit results
      if(hdebugtrackprint.ne.0) then
        call h_print_tracks
      endif                             ! end test on zero tracks
 1000 return
      end

*
