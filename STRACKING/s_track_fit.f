      subroutine S_TRACK_FIT(ABORT,err,ierr)
*     primary track fitting routine for the SOS spectrometer
*
*     Called by S_TRACK
*
*     d.f. geesaman         8 Sept 1993
* $Log$
* Revision 1.4  1995/05/22 19:46:01  cdaq
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
*
      external S_FCNCHISQ
      external S_DPSIFUN
      real*8 S_DPSIFUN
      real*8 S_FCNCHISQ
*
*     local variables
*
      logical ABORT
      character*50 here
      parameter (here='S_TRACK_FIT')
      character*(*) err
      integer*4 itrack                        ! track loop index
      integer*4 i                             ! loop index
      integer*4 ierr                          ! error return flag
      integer*4 ivarbl                  ! dummy MINUIT variable
      integer*4 ihit, plane, hit
      real*8 pos
      real*8 ray(snum_fpray_param)
      real*8 ray1(4)
      real*8 error(snum_fpray_param)
      real*8 initialray(snum_fpray_param)
      real*8 initialsteps(snum_fpray_param)
      data initialray/0.D0,0.D0,.5D-2,.5D-2/     ! starting ray values
      data initialsteps/1.D0,1.D0,.5E-2,.5D-2/
      real*8 zero
      data zero/0.0D0/
      real*8 arglis(10)
      real*8 bnd1,bnd2                     ! unused MUNUIT output variables
      real*8 chi2
      real*8 fedm,errder                   ! unused MUNUIT output variables
      integer*4 npari,nparz,istat          ! unused MUNUIT output variables
      character*10 fitnames(4)
      data fitnames/' x_t ',' y_t ','tan(xp)','tan(yp)'/
      save initialray,initialsteps,fitnames   ! starting ray, steps, names
*
      ABORT= .FALSE.
      ierr=0
*  initailize residuals

      do plane=1,SDC_NUM_PLANES
        do itrack=1,SNTRACKS_MAX
          sdc_double_residual(itrack,plane)=1000
          sdc_single_residual(itrack,plane)=1000
        enddo
        sdc_sing_res(plane)=1000
        sdc_dbl_res(plane)=1000
      enddo

*     test for no tracks
      if(SNTRACKS_FP.ge.1) then
        do itrack=1,SNTRACKS_FP
            strack_fit_num=itrack
*     are there enough degrees of freedom
          SNFREE_FP(itrack)=SNTRACK_HITS(itrack,1)-snum_fpray_param
          if(SNFREE_FP(itrack).gt.0) then
*     initialize parameters
           do i=1,snum_fpray_param
            call MNPARM(i,fitnames(i),initialray(i),initialsteps(i),
     &                  zero,zero,ierr)  
            if(ierr.ne.0) then
              write(sluno,'(a,i,a)') ' Unable to define parameter no.',i,
     &                           fitnames(i)
              ierr=1
              go to 1000                     ! error return
            endif
           enddo                              ! end loop over track param
           do i=1,10
            arglis(i)=0.
           enddo
*     Do track fit on track number strack_fit_num (passes in sos_tracking)
           Call MNSETI(' Track Fitting in SOS Spectrometer')
           Call MNEXCM(S_FCNCHISQ,'MIGRAD',arglis,0,ierr)
           Call MNEXCM(S_FCNCHISQ,'MINOS',arglis,0,ierr)
           Call MNSTAT(chi2,fedm,errder,npari,nparz,istat)
           do i=1,snum_fpray_param
             call MNPOUT(i,fitnames(i),ray(i),error(i),bnd1,bnd2,ivarbl)
           enddo     
           SX_FP(itrack)=real(ray(1))
           SY_FP(itrack)=real(ray(2))
           SZ_FP(itrack)=0.                   ! z=0 of tracking.
           SXP_FP(itrack)=real(ray(3))
           SYP_FP(itrack)=real(ray(4))
           SCHI2_FP(itrack)=real(chi2)

* calculate residuals
           ray1(1)=dble(SX_FP(itrack))
           ray1(2)=dble(SY_FP(itrack))
           ray1(3)=dble(SXP_FP(itrack))
           ray1(4)=dble(SYP_FP(itrack))
           do ihit=2,SNTRACK_HITS(itrack,1)+1
             hit=SNTRACK_HITS(itrack,ihit)
             plane=SDC_PLANE_NUM(hit)
             pos=S_DPSIFUN(ray1,plane)
             sdc_single_residual(itrack,plane)=SDC_WIRE_COORD(hit)-pos
             sdc_sing_res(plane)=sdc_single_residual(itrack,plane)
           enddo
         endif                          ! end test on degrees of freedom
       enddo                            ! end loop over tracks
      endif

*     test if we want to dump out trackfit results
      if(sdebugtrackprint.ne.0) then
         call s_print_tracks
      endif                                   ! end test on zero tracks
1000  return
      end
