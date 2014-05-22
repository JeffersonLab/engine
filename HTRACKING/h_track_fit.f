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
* $Log: h_track_fit.f,v $
* Revision 1.11  1996/01/16 21:42:18  cdaq
* (JRA) Remove slices code, misc fixes, reindent.
*
* Revision 1.10  1995/08/30 16:11:39  cdaq
* (JRA) Don't fill single_residual arrray
*
* Revision 1.9  1995/05/22  19:39:31  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.8  1995/04/06  19:32:58  cdaq
* (JRA) Rename residuals variables
*
* Revision 1.7  1995/01/27  20:26:50  cdaq
* (JRA) Remove Mack's personal focalplane diceamatic (z slicer) code
*
* Revision 1.6  1994/12/06  15:45:27  cdaq
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
      include "hms_data_structures.cmn"
      include "hms_tracking.cmn"
      include "hms_geometry.cmn"
      external h_dpsifun
*
*
*     local variables
*
      logical ABORT
      character*11 here
      parameter (here='H_TRACK_FIT')
      character*(*) err
      integer*4 itrk                        ! track loop index
      integer*4 ihit,ierr
      integer*4 hit,pln
      integer*4 i,j                             ! loop index
*      real*4 z_slice

      real*8   h_dpsifun
      real*8   pos
      real*8   ray1(4)
      real*8   ray2(4)
      real*8 TT(hnum_fpray_param)
      real*8 AA(hnum_fpray_param,hnum_fpray_param)
      real*8 dray(hnum_fpray_param)
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

      do pln=1,hdc_num_planes
        do itrk=1,hntracks_fp
          hdc_double_residual(itrk,pln)=1000
          hdc_single_residual(itrk,pln)=1000
        enddo
c fill the 1d arrays from the 2d arrays for good track (in h_physics)
c        hdc_sing_res(pln)=1000
        hdc_dbl_res(pln)=1000
      enddo

*     test for no tracks
      if(hntracks_fp.ge.1) then
        do itrk=1,hntracks_fp
          chi2= dummychi2
          htrack_fit_num=itrk

*     are there enough degrees of freedom
          hnfree_fp(itrk)=hntrack_hits(itrk,1)-hnum_fpray_param
          if(hnfree_fp(itrk).gt.0) then

*     initialize parameters
            do i=1,hnum_fpray_param
              TT(i)=0.
              do ihit=2,hntrack_hits(itrk,1)+1
                hit=hntrack_hits(itrk,ihit)
                pln=hdc_plane_num(hit)
                TT(i)=TT(i)+((hdc_wire_coord(hit)*
     &               hplane_coeff(remap(i),pln))
     &               /(hdc_sigma(pln)*hdc_sigma(pln)))
              enddo 
            enddo
            do i=1,hnum_fpray_param
              do j=1,hnum_fpray_param
                AA(i,j)=0.
                if(j.lt.i)then
                  AA(i,j)=AA(j,i)
                else 
                  do ihit=2,hntrack_hits(itrk,1)+1
                    hit=hntrack_hits(itrk,ihit)
                    pln=hdc_plane_num(hit)
                    AA(i,j)=AA(i,j) + (
     &                   hplane_coeff(remap(i),pln)*hplane_coeff(remap(j)
     $                   ,pln)/(hdc_sigma(pln)*hdc_sigma(pln)))
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
              
* calculate hit coord at each plane for chisquared and efficiency calculations.
              do pln=1,hdc_num_planes
                hdc_track_coord(itrk,pln)=hplane_coeff(remap(1),pln)*dray(1)
     &               +hplane_coeff(remap(2),pln)*dray(2)
     &               +hplane_coeff(remap(3),pln)*dray(3)
     &               +hplane_coeff(remap(4),pln)*dray(4)
              enddo

              do ihit=2,hntrack_hits(itrk,1)+1
                hit=hntrack_hits(itrk,ihit)
                pln=hdc_plane_num(hit)

* note chi2 is single precision

                hdc_single_residual(itrk,pln)=
     &              hdc_wire_coord(hit)-hdc_track_coord(itrk,pln)
                chi2=chi2+
     &              (hdc_single_residual(itrk,pln)/hdc_sigma(pln))**2
              enddo
            endif

            hx_fp(itrk)=dray(1)
            hy_fp(itrk)=dray(2)
            hz_fp(itrk)=0.            ! z=0 of tracking.
            hxp_fp(itrk)=dray(3)
            hyp_fp(itrk)=dray(4)
          endif                         ! end test on degrees of freedom
          hchi2_fp(itrk)=chi2
        enddo                           ! end loop over tracks
      endif

* calculate residuals for each chamber if in single stub mode
* and there were 2 tracks found one in first chanber and one in the second

      if (hsingle_stub.ne.0) then
        if (hntracks_fp.eq.2) then
          itrk=1
          ihit=2
          hit=hntrack_hits(itrk,ihit)
          pln=hdc_plane_num(hit)
          if (pln.le.6) then
            itrk=2
            hit=hntrack_hits(itrk,ihit)
            pln=hdc_plane_num(hit)
            if (pln.ge.7) then

* condition of above met calculating residuals  
* assigning rays to tracks in each chamber
* ray1 is ray from first chamber fit
* ray2 is ray from second chamber fit

              ray1(1)=dble(hx_fp(1))
              ray1(2)=dble(hy_fp(1))
              ray1(3)=dble(hxp_fp(1))
              ray1(4)=dble(hyp_fp(1))
              ray2(1)=dble(hx_fp(2))
              ray2(2)=dble(hy_fp(2))
              ray2(3)=dble(hxp_fp(2))
              ray2(4)=dble(hyp_fp(2))

              itrk=1
* loop over hits in second chamber
              do ihit=1,hntrack_hits(itrk+1,1)

* calculate residual in second chamber from first chamber track
                hit=hntrack_hits(itrk+1,ihit+1)
                pln=hdc_plane_num(hit)
                pos=h_dpsifun(ray1,pln)
                hdc_double_residual(itrk,pln)=hdc_wire_coord(hit)-pos
* djm 8/31/94 stuff this variable into 1d array we can register
                hdc_dbl_res(pln) = hdc_double_residual(1,pln)

              enddo

              itrk=2
* loop over hits in first chamber
              do ihit=1,hntrack_hits(itrk-1,1)

* calculate residual in first chamber from second chamber track
                hit=hntrack_hits(itrk-1,ihit+1)
                pln=hdc_plane_num(hit)
                pos=h_dpsifun(ray2,pln)
                hdc_double_residual(itrk,pln)=hdc_wire_coord(hit)-pos
* djm 8/31/94 stuff this variable into 1d array we can register
                hdc_dbl_res(pln) = hdc_double_residual(2,pln)

              enddo
            endif                       ! end pln ge 7
          endif                         ! end pln le 6
        endif                           ! end hntracks_fp eq 2
      endif                             ! end hsignle_stub .ne. 0

*     test if we want to dump out trackfit results
      if(hdebugtrackprint.ne.0) then
        call h_print_tracks
      endif                             ! end test on zero tracks
 1000 return
      end

*
