      subroutine S_TRACK_FIT(ABORT,err,ierr)
*     primary track fitting routine for the SOS spectrometer
*
*     Called by S_TRACK
*
*     d.f. geesaman         8 Sept 1993
* $Log: s_track_fit.f,v $
* Revision 1.8  1996/01/17 18:56:08  cdaq
* (JRA) Fill sdc_plane_wirecenter and sdc_plane_wirecoord arrays
*
* Revision 1.7  1995/10/11 18:15:12  cdaq
* (JRA) Comment out MINUIT track fitting for now.
*
* Revision 1.6  1995/08/31 20:44:56  cdaq
* (JRA) Don't fill single_residual arrray
*
* Revision 1.5  1995/07/20  19:06:05  cdaq
* (SAW) Move data statements for f2c compatibility
*
* Revision 1.4  1995/05/22  19:46:01  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.3  1995/04/06  19:45:16  cdaq
* (JRA) Rename residuals variables
*
* Revision 1.2  1994/11/23  14:24:18  cdaq
* (SPB) Recopied from hms file and modified names for SOS
*
* Revision 1.1  1994/02/21  16:42:27  cdaq
* Initial revision
*
      implicit none
      include "sos_data_structures.cmn"
      include "sos_tracking.cmn"
      include "sos_geometry.cmn"
*
c      external S_FCNCHISQ
c      external S_DPSIFUN
c      real*8 S_DPSIFUN
c      real*8 S_FCNCHISQ
*
*     local variables
*
      logical ABORT
      character*11 here
      parameter (here='S_TRACK_FIT')
      character*(*) err
      integer*4 itrk                    ! track loop index
      integer*4 i,j                     ! loop index
      integer*4 ierr                    ! error return flag
c      integer*4 ivarbl                  ! dummy MINUIT variable
      integer*4 ihit, pln, hit
c      real*8 pos
      real*8 dray(snum_fpray_param)
      real*8 TT(snum_fpray_param)
      real*8 AA(snum_fpray_param,snum_fpray_param)
c      real*8 error(snum_fpray_param)
c      real*8 initialray(snum_fpray_param)
c      real*8 initialsteps(snum_fpray_param)
c      real*8 zero
c      real*8 arglis(10)
c      real*8 bnd1,bnd2                     ! unused MUNUIT output variables
      real*8 chi2
c      real*8 fedm,errder                   ! unused MUNUIT output variables
c      integer*4 npari,nparz,istat          ! unused MUNUIT output variables
c      character*10 fitnames(4)

c      data initialray/0.D0,0.D0,.5D-2,.5D-2/     ! starting ray values
c      data initialsteps/1.D0,1.D0,.5E-2,.5D-2/
c      data zero/0.0D0/
c      data fitnames/' x_t ',' y_t ','tan(xp)','tan(yp)'/

c      save initialray,initialsteps,fitnames   ! starting ray, steps, names
      integer*4 remap(snum_fpray_param)
      data remap/5,6,3,4/
      save remap
*
      ABORT= .FALSE.
      ierr=0
*  initailize residuals

      do pln=1,sdc_num_planes
        do itrk=1,sntracks_fp
          sdc_double_residual(itrk,pln)=1000
          sdc_single_residual(itrk,pln)=1000
        enddo
c fill the 1d arrays from the 2d arrays in h_physics (don't clear here).
c        sdc_sing_res(pln)=1000
        sdc_dbl_res(pln)=1000
      enddo

*     test for no tracks
      if(sntracks_fp.ge.1) then
        do itrk=1,sntracks_fp
            strack_fit_num=itrk

*     are there enough degrees of freedom
          snfree_fp(itrk)=sntrack_hits(itrk,1)-snum_fpray_param
          if(snfree_fp(itrk).gt.0) then

c*     initialize parameters
c           do i=1,snum_fpray_param
c            call MNPARM(i,fitnames(i),initialray(i),initialsteps(i),
c     &                  zero,zero,ierr)  
c            if(ierr.ne.0) then
c              write(sluno,'(a,i,a)') ' Unable to define parameter no.',i,
c     &                           fitnames(i)
c              ierr=1
c              go to 1000                     ! error return
c            endif
c           enddo                              ! end loop over track param
c           do i=1,10
c            arglis(i)=0.
c           enddo
c*     Do track fit on track number strack_fit_num (passes in sos_tracking)
c           Call MNSETI(' Track Fitting in SOS Spectrometer')
c           Call MNEXCM(S_FCNCHISQ,'MIGRAD',arglis,0,ierr)
c           Call MNEXCM(S_FCNCHISQ,'MINOS',arglis,0,ierr)
c           Call MNSTAT(chi2,fedm,errder,npari,nparz,istat)
c           do i=1,snum_fpray_param
c             call MNPOUT(i,fitnames(i),ray(i),error(i),bnd1,bnd2,ivarbl)
c           enddo     
c           sx_fp(itrk)=real(ray(1))
c           sy_fp(itrk)=real(ray(2))
c           sz_fp(itrk)=0.                   ! z=0 of tracking.
c           sxp_fp(itrk)=real(ray(3))
c           syp_fp(itrk)=real(ray(4))
c           schi2_fp(itrk)=real(chi2)
c
c* calculate residuals
c           ray1(1)=dble(sx_fp(itrk))
c           ray1(2)=dble(sy_fp(itrk))
c           ray1(3)=dble(sxp_fp(itrk))
c           ray1(4)=dble(syp_fp(itrk))
c           do ihit=2,sntrack_hits(itrk,1)+1
c             hit=sntrack_hits(itrk,ihit)
c             pln=sdc_plane_num(hit)
c             pos=s_dpsifun(ray1,pln)
c             sdc_single_residual(itrk,pln)=sdc_wire_coord(hit)-pos
c           enddo
c         endif                          ! end test on degrees of freedom
c       enddo                            ! end loop over tracks
c      endif

*     initialize parameters
            do i=1,snum_fpray_param
              TT(i)=0.
              do ihit=2,sntrack_hits(itrk,1)+1
                hit=sntrack_hits(itrk,ihit)
                pln=sdc_plane_num(hit)
                TT(i)=TT(i)+((sdc_wire_coord(hit)*
     &               splane_coeff(remap(i),pln))
     &               /(sdc_sigma(pln)*sdc_sigma(pln)))
              enddo
            enddo
            do i=1,snum_fpray_param
              do j=1,snum_fpray_param
                AA(i,j)=0.
                if(j.lt.i)then
                  AA(i,j)=AA(j,i)
                else
                  do ihit=2,sntrack_hits(itrk,1)+1
                    hit=sntrack_hits(itrk,ihit)
                    pln=sdc_plane_num(hit)
                    AA(i,j)=AA(i,j) + (
     &                   splane_coeff(remap(i),pln)*splane_coeff(remap(j)
     $                   ,pln)/(sdc_sigma(pln)*sdc_sigma(pln)))
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

*              ray(1)=dray(1)
*              ray(2)=dray(2)
*              ray(3)=dray(3)
*              ray(4)=dray(4)

* calculate hit coord at each plane for chisquared and efficiency calculations.
              do pln=1,sdc_num_planes
                sdc_track_coord(itrk,pln)=splane_coeff(remap(1),pln)*dray(1)
     &               +splane_coeff(remap(2),pln)*dray(2)
     &               +splane_coeff(remap(3),pln)*dray(3)
     &               +splane_coeff(remap(4),pln)*dray(4)
              enddo

              do ihit=2,sntrack_hits(itrk,1)+1
                hit=sntrack_hits(itrk,ihit)
                pln=sdc_plane_num(hit)

* note chi2 is single precision
                sdc_plane_wirecenter(itrk,pln)=sdc_wire_center(hit)
                sdc_plane_wirecoord(itrk,pln)=sdc_wire_coord(hit)

                sdc_single_residual(itrk,pln)=
     &              sdc_wire_coord(hit)-sdc_track_coord(itrk,pln)
                chi2=chi2+
     &              (sdc_single_residual(itrk,pln)/sdc_sigma(pln))**2
              enddo
            endif

            sx_fp(itrk)=dray(1)
            sy_fp(itrk)=dray(2)
            sz_fp(itrk)=0.            ! z=0 of tracking.
            sxp_fp(itrk)=dray(3)
            syp_fp(itrk)=dray(4)
          endif                         ! end test on degrees of freedom
          schi2_fp(itrk)=chi2
        enddo                           ! end loop over tracks
      endif

*     test if we want to dump out trackfit results
      if(sdebugtrackprint.ne.0) then
         call s_print_tracks
      endif                                   ! end test on zero tracks
1000  return
      end
