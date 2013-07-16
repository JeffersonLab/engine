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
* $Log: h_physics_stat.f,v $
* Revision 1.6  1995/10/10 16:50:08  cdaq
* (JRA) Comment out some redundant efficiency calculations
*
* Revision 1.5  1995/08/31 14:47:09  cdaq
* (JRA) Add calls to h_dc_eff and h_cer_eff
*
* Revision 1.4  1995/05/22  19:39:16  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.3  1995/02/23  15:37:57  cdaq
* (JRA) Move scint eff's to h_scin_eff, add call to h_cal_eff
*
* Revision 1.2  1994/06/15  20:22:49  cdaq
* (DFG) Add scin plane efficiency
*
* Revision 1.1  1994/06/15  19:09:37  cdaq
* Initial revision
*-                           
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*14 here
      parameter (here= 'h_physics_stat')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 'hms_data_structures.cmn'
      INCLUDE 'gen_routines.dec'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      INCLUDE 'hms_geometry.cmn'
      INCLUDE 'hms_physics_sing.cmn'
      INCLUDE 'hms_statistics.cmn'
      INCLUDE 'hms_bypass_switches.cmn'
*
*     local variables 
c      integer*4 goodtrack,tothits,ihit,hitnum,plane
c      real*4 normsigma
c      real*8 ray(4)                     ! xt,yt,xpt,ypt
c      EXTERNAL  H_DPSIFUN
c      REAL*8 H_DPSIFUN
*--------------------------------------------------------
*
      ABORT= .FALSE.
      err= ' '
c*     increment numbr of tracks
c      hgoodtracksctr = hgoodtracksctr +1
c*     loop over all hists
c      goodtrack = HSNUM_FPTRACK
c      tothits=HNTRACK_HITS(goodtrack,1)
c      if(tothits.gt.0) then
c*     get ray parameters
c        ray(1) = DBLE(HX_FP(goodtrack))
c        ray(2) = DBLE(HY_FP(goodtrack))
c        ray(3) = DBLE(HXP_FP(goodtrack))
c        ray(4) = DBLE(HYP_FP(goodtrack))

c*     loop over all hits in track
c        do ihit = 1, tothits
c          hitnum=HNTRACK_HITS(goodtrack,1+ihit)
c          plane = HDC_PLANE_NUM(hitnum)
c          normsigma = (HDC_WIRE_COORD(hitnum)
c     $         - REAL(H_DPSIFUN(ray,plane)))/hdc_sigma(plane)
c          hplanehitctr(plane) = hplanehitctr(plane) + 1
c          hplanesigmasq(plane) = hplanesigmasq(plane) + normsigma
c     $         *normsigma
c          hmeasuredsigma(plane) = SQRT(hplanesigmasq(plane) 
c     &         / FLOAT(hplanehitctr(plane)))
c          hchambereff(plane)=FLOAT(hplanehitctr(plane))
c     $         /FLOAT(hgoodtracksctr)
c        enddo                           ! endloop over hits in track
c      endif                             ! end test on zero hits
*
*
*     Drift chamber efficiencies
      if (hbypass_dc_eff.eq.0) call h_dc_trk_eff
*
*     Scintillator efficiencies
      if (hbypass_scin_eff.eq.0) call h_scin_eff
*
*     Cerenkov efficiencies
      if (hbypass_cer_eff.eq.0) call h_cer_eff
*
*     Calorimeter efficiencies
      if (hbypass_cal_eff.eq.0) call h_cal_eff
*
      RETURN
      END
