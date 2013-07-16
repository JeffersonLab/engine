*=======================================================================
      function h_correct_cal_neg(x,y)
*=======================================================================
*-
*-      Purpose: Returns the impact point correction factor. This
*-               factor is to be applied to the energy depositions.
*-               This correction for single "NEG_PMT" readout from 
*-               LG-blocks. The final energy is the ADC value TIMES 
*-               the correction factor.
*-
*-      Input Parameters: x,y - impact point coordinates
*-
*-      Created 09 October 1997      H. Mkrtchyan
*
* $Log: h_correct_cal_neg.f,v $
* Revision 1.6  2003/04/03 00:43:13  jones
* Update to calibration (V. Tadevosyan0
*
* Revision 1.5  2003/03/21 22:33:22  jones
* Subroutines had arguments with abort,errmsg . But these arguments were not
* used when the subroutine was called. Also abort ,errmsg were not used in the
* subroutines. So eliminate abort,errmsg. (E. Brash)
*
* Revision 1.4  2002/09/26 14:43:17  jones
*    Different parameters a,b,c
*    Fit to pion data of run 23121
*    Different formula for h_correct_cal_neg
*
* Revision 1.2  1999/01/29 17:33:56  saw
* Cosmetic changes
*
* Revision 1.1  1999/01/21 21:40:13  saw
* Extra shower counter tube modifications
*
*
*
*-----------------------------------------------------------------------
*
      implicit none
      save
*
*      logical abort
*      character*(*) errmsg
      character*17 here
      parameter (here='H_CORRECT_CAL_NEG')
*
*
      real*4 x,y                ! Impact point coordinates
      real*4 h_correct_cal_neg
*
      include 'hms_data_structures.cmn'
      include 'hms_calorimeter.cmn'
*

c     Check calorimeter boundaries.

      if(y.lt.hcal_ymin) y=hcal_ymin
      if(y.gt.hcal_ymax) y=hcal_ymax

*
*     Fit to stright through pion data of run # 23121.
*
      h_correct_cal_neg=(64.36-y)/(64.36-y/1.66)

ccc      h_correct_cal_neg=exp(-y/200.)      !200 cm atten length. 
ccc      h_correct_cal_neg=h_correct_cal_neg*(1. + y*y/8000.) 
*   
      return
      end
