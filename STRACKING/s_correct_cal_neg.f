*=======================================================================
      function s_correct_cal_neg(x,y,abort,errmsg)
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
*-      Created 27 January 1999      SAW
*
* $Log$
* Revision 1.2  1999/02/25 20:18:40  saw
* Vardan Tadevosyan shower code updates
*
* Revision 1.1  1999/01/29 17:34:57  saw
* Add variables for second tubes on shower counter
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
      parameter (here='S_CORRECT_CAL_NEG')
      real*4 a,b,c	! Fit parameters.
      parameter (a=2.3926,b=-0.371375,c=-0.25401)
*
*
      real*4 x,y         !Impact point coordinates
      real*4 s_correct_cal_neg
      real*4 d,al	! Auxiliary variables.
*
      include 'sos_data_structures.cmn'
      include 'sos_calorimeter.cmn'
*
*     Fit to MC data in the range of y [-30,+30].
*
      d=y+35.
      al=alog(d)
      s_correct_cal_neg=1./(a+b*al+c/al)

ccc      s_correct_cal_neg=exp(-y/200.)      !200 cm atten length. 
ccc      s_correct_cal_neg=s_correct_cal_neg*(1. + y*y/8000.) 
*   
      return
      end
