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
* Revision 1.3  1999/02/25 20:10:48  saw
* Vardan Tadevosyan shower code updates
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
      real*4 a,b,c     ! Fit parameters.
      parameter (a=2.3926,b=-0.371375,c=-0.25401)
*
*
      real*4 x,y         !Impact point coordinates
      real*4 h_correct_cal_neg
      real*4 d,al                       ! Auxiliary variables
*
      include 'hms_data_structures.cmn'
      include 'hms_calorimeter.cmn'
*
*   ! Here  I was used some preliminary function 
*     Fit to the MC data in the range of y [-30,+30].
*
      d=y-35.  ! Distance to the PMT.
      al=alog(d)
      h_correct_cal_neg=1./(a+b*al+c/al)

ccc      h_correct_cal_neg=exp(-y/200.)      !200 cm atten length. 
ccc      h_correct_cal_neg=h_correct_cal_neg*(1. + y*y/8000.) 
*   
      return
      end


