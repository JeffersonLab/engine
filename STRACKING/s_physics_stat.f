      subroutine s_physics_stat(ABORT,err)
*--------------------------------------------------------
*-
*-   Purpose and Methods : Calculate statistics and chamber efficencies for 
*-                         SOS physics analysis on SOS only part of
*-                            event.
*-                              
*-
*-      Required Input BANKS     SOS_DECODED_DC
*-                               SOS_FOCAL_PLANE
*-
*-      Output BANKS             CTP PARAMETERS
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
*-   Created 10-JUN-1994     D. F. Geesaman
* $Log: s_physics_stat.f,v $
* Revision 1.5  1995/10/10 16:49:32  cdaq
* (JRA) Comment out some redundant efficiency calculations
*
* Revision 1.4  1995/08/31 18:56:53  cdaq
* (JRA) Add call to s_cer_eff
*
* Revision 1.3  1995/05/22  19:45:44  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.2  1995/02/23  15:38:41  cdaq
* (JRA) Move scint eff's to s_scin_eff, add call to s_cal_eff
*
* Revision 1.1  1994/06/14  04:10:43  cdaq
* Initial revision
*
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*14 here
      parameter (here= 's_physics_stat')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 'sos_data_structures.cmn'
      INCLUDE 'gen_routines.dec'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      INCLUDE 'sos_geometry.cmn'
      INCLUDE 'sos_physics_sing.cmn'
      INCLUDE 'sos_statistics.cmn'
      INCLUDE 'sos_bypass_switches.cmn'
*     
*     local variables 
c      integer*4 goodtrack,tothits,ihit,hitnum,plane
c      real*4 normsigma
c      real*8 ray(4)                     ! xt,yt,xpt,ypt
c      EXTERNAL  S_DPSIFUN
c      REAL*8 S_DPSIFUN
*--------------------------------------------------------
*
      ABORT= .FALSE.
      err= ' '
c*     increment numbr of tracks
c      sgoodtracksctr = sgoodtracksctr +1
c*     loop over all hists
c      goodtrack = SSNUM_FPTRACK
c      tothits=SNTRACK_HITS(goodtrack,1)
c      if(tothits.gt.0) then
c*     get ray parameters
c        ray(1) = DBLE(SX_FP(goodtrack))
c        ray(2) = DBLE(SY_FP(goodtrack))
c        ray(3) = DBLE(SXP_FP(goodtrack))
c        ray(4) = DBLE(SYP_FP(goodtrack))

c*     loop over all hits in track
c        do ihit = 1, tothits
c          hitnum=SNTRACK_HITS(goodtrack,1+ihit)
c          plane = SDC_PLANE_NUM(hitnum)
c          normsigma = (SDC_WIRE_COORD(hitnum)
c     $         - REAL(S_DPSIFUN(ray,plane)))/sdc_sigma(plane)
c          splanehitctr(plane) = splanehitctr(plane) + 1
c          splanesigmasq(plane) = splanesigmasq(plane) + normsigma
c     $         *normsigma
c          smeasuredsigma(plane) = SQRT(splanesigmasq(plane) 
c     &         / FLOAT(splanehitctr(plane)))
c          schambereff(plane) =FLOAT(splanehitctr(plane))
c     $         /FLOAT(sgoodtracksctr)
c        enddo                           ! endloop over hits in track
c      endif                             ! end test on zero hits
*
*     Drift chamber efficiencies
      if (sbypass_dc_eff.eq.0) call s_dc_trk_eff
*
*     Scintillator efficiencies
      if (sbypass_scin_eff.eq.0) call s_scin_eff
*
*     Cerenkov efficiencies
      if (sbypass_cer_eff.eq.0) call s_cer_eff
*
*     Calorimeter efficiencies
      if (sbypass_cal_eff.eq.0) call s_cal_eff
*
      RETURN
      END
