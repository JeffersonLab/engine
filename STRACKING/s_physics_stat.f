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
* $Log$
* Revision 1.2  1995/02/23 15:38:41  cdaq
* (JRA) Move scint eff's to s_scin_eff, add call to s_cal_eff
*
* Revision 1.1  1994/06/14  04:10:43  cdaq
* Initial revision
*
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*50 here
      parameter (here= 's_physics_stat')
*
      logical ABORT
      character*(*) err
      integer*4 ierr
*
      INCLUDE 'gen_data_structures.cmn'
      INCLUDE 'gen_routines.dec'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      INCLUDE 'sos_geometry.cmn'
      INCLUDE 'sos_physics_sing.cmn'
      INCLUDE 'sos_statistics.cmn'
*     
*     local variables 
      integer*4 goodtrack,tothits,ihit,sigma,hitnum,plane
      real*4 normsigma
      real*8 ray(4)                     ! xt,yt,xpt,ypt
      EXTERNAL  S_DPSIFUN
      REAL*8 S_DPSIFUN
*--------------------------------------------------------
*
      ABORT= .FALSE.
      err= ' '
*     increment numbr of tracks
      sgoodtracksctr = sgoodtracksctr +1
*     loop over all hists
      goodtrack = SSNUM_FPTRACK
      tothits=SNTRACK_HITS(goodtrack,1)
      if(tothits.gt.0) then
*     get ray parameters
        ray(1) = DBLE(SX_FP(goodtrack))
        ray(2) = DBLE(SY_FP(goodtrack))
        ray(3) = DBLE(SXP_FP(goodtrack))
        ray(4) = DBLE(SYP_FP(goodtrack))

*     loop over all hits in track
         

        do ihit = 1, tothits
          hitnum=SNTRACK_HITS(goodtrack,1+ihit)
          plane = SDC_PLANE_NUM(hitnum)
          normsigma = (SDC_WIRE_COORD(hitnum)
     $         - REAL(S_DPSIFUN(ray,plane)))/sdc_sigma(plane)
          splanehitctr(plane) = splanehitctr(plane) + 1
          splanesigmasq(plane) = splanesigmasq(plane) + normsigma
     $         *normsigma
          smeasuredsigma(plane) = SQRT(splanesigmasq(plane) 
     &         / FLOAT(splanehitctr(plane)))
          schambereff(plane) =FLOAT(splanehitctr(plane))
     $         /FLOAT(sgoodtracksctr)
        enddo                           ! endloop over hits in track
      endif                             ! end test on zero hits
*
*     Scintillator efficiencies
      call s_scin_eff
*
*     Calorimeter efficiencies
      call s_cal_eff
*
      RETURN
      END
