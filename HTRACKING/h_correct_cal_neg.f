*=======================================================================
      function h_correct_cal_neg(x,y,abort,errmsg)
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
* $Log$
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
      logical abort
      character*(*) errmsg
      character*17 here
      parameter (here='H_CORRECT_CAL_NEG')
      real*4 a,b,c	! Fit parameters.
      parameter (a=1.8904,b=-0.2289,c=-0.2724)
*
*
      real*4 x,y	! Impact point coordinates
      real*4 h_correct_cal_neg
      real*4 d,al	! Auxiliary variables.
*
      include 'hms_data_structures.cmn'
      include 'hms_calorimeter.cmn'
*
*     Fit to stright through pion data of run # 23121.
*
      if(y.lt.hcal_fv_ymin) y=hcal_fv_ymin
      if(y.gt.hcal_fv_ymax) y=hcal_fv_ymax

      d=hcal_ymax-y	! Distance to the PMT.
      al=alog(d)
      h_correct_cal_neg=1./(a+b*al+c/al)

ccc      h_correct_cal_neg=exp(-y/200.)      !200 cm atten length. 
ccc      h_correct_cal_neg=h_correct_cal_neg*(1. + y*y/8000.) 
*   
      return
      end
