*=======================================================================
      function s_correct_cal_neg(x,y)
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
* Revision 1.2.2.1  2003/03/25 03:04:35  cdaq
*  match main brach mar-24-2003
*
* Revision 1.3  2003/03/21 22:58:02  jones
* Subroutines had arguments with abort,errmsg . But these arguments were not
* used when the subroutine was called. Also abort ,errmsg were not used in the
* subroutines. So eliminate abort,errmsg. (E. Brash)
*
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
*      logical abort
*      character*(*) errmsg
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
      if(y.lt.-35.0) then
	write(*,*)'******** Problem in s_correct_cal_neg ***********'
	write(*,*)'******** y should be in the range [-30,30] ******'
	write(*,*)'******** but has the value ',y,' ***'
	write(*,*)'******** Setting it to -33.0 and continuing ****'
	y=-33.0
      endif
      d=y+35.
      al=alog(d)
      s_correct_cal_neg=1./(a+b*al+c/al)

ccc      s_correct_cal_neg=exp(-y/200.)      !200 cm atten length. 
ccc      s_correct_cal_neg=s_correct_cal_neg*(1. + y*y/8000.) 
*   
      return
      end
