      subroutine h_physics_stat(ABORT,err)
*--------------------------------------------------------
*-
*-   Purpose and Methods : Calculate statistics and chamber efficencies for 
*-                         HMS physics analysis on HMS only part of
*-                            event.
*-                              
*-
*-      Required Input BANKS     HMS_DECODED_DC
*-                               HMS_FOCAL_PLANE
*-
*-      Output BANKS             CTP PARAMETERS
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
*-   Created 10-JUN-1994     D. F. Geesaman
* $Log$
* Revision 1.3  1995/02/23 15:37:57  cdaq
* (JRA) Move scint eff's to h_scin_eff, add call to h_cal_eff
*
* Revision 1.2  1994/06/15  20:22:49  cdaq
* (DFG) Add scin plane efficiency
*
c Revision 1.1  1994/06/15  19:09:37  cdaq
c Initial revision
c-                           
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*50 here
      parameter (here= 'h_physics_stat')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 'gen_data_structures.cmn'
      INCLUDE 'gen_routines.dec'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      INCLUDE 'hms_geometry.cmn'
      INCLUDE 'hms_physics_sing.cmn'
      INCLUDE 'hms_statistics.cmn'
*
*     local variables 
      integer*4 goodtrack,tothits,ihit,hitnum,plane
      real*4 normsigma
      real*8 ray(4)                     ! xt,yt,xpt,ypt
      EXTERNAL  H_DPSIFUN
      REAL*8 H_DPSIFUN
*--------------------------------------------------------
*
      ABORT= .FALSE.
      err= ' '
*     increment numbr of tracks
      hgoodtracksctr = hgoodtracksctr +1
*     loop over all hists
      goodtrack = HSNUM_FPTRACK
      tothits=HNTRACK_HITS(goodtrack,1)
      if(tothits.gt.0) then
*     get ray parameters
        ray(1) = DBLE(HX_FP(goodtrack))
        ray(2) = DBLE(HY_FP(goodtrack))
        ray(3) = DBLE(HXP_FP(goodtrack))
        ray(4) = DBLE(HYP_FP(goodtrack))

*     loop over all hits in track
         

        do ihit = 1, tothits
          hitnum=HNTRACK_HITS(goodtrack,1+ihit)
          plane = HDC_PLANE_NUM(hitnum)
          normsigma = (HDC_WIRE_COORD(hitnum)
     $         - REAL(H_DPSIFUN(ray,plane)))/hdc_sigma(plane)
          hplanehitctr(plane) = hplanehitctr(plane) + 1
          hplanesigmasq(plane) = hplanesigmasq(plane) + normsigma
     $         *normsigma
          hmeasuredsigma(plane) = SQRT(hplanesigmasq(plane) 
     &         / FLOAT(hplanehitctr(plane)))
          hchambereff(plane)=FLOAT(hplanehitctr(plane))
     $         /FLOAT(hgoodtracksctr)
        enddo                           ! endloop over hits in track
      endif                             ! end test on zero hits
*     
*     Scintillator efficiencies
      call h_scin_eff
*     
*     Calorimeter efficiencies
      call h_cal_eff
*
      RETURN
      END
