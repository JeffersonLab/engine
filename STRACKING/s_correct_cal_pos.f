*=======================================================================
      function s_correct_cal_pos(x,y,abort,errmsg)
*=======================================================================
*-
*-      Purpose: Returns the impact point correction factor. This
*-               factor is to be applied to the energy depositions.
*-               The final energy is the ADC value TIMES the correction factor.
*-
*-      Input Parameters: x,y - impact point coordinates
*-
*-      Created 15 Mar 1994      Tsolak A. Amatuni
*
* $Log$
* Revision 1.1  1999/01/29 17:34:57  saw
* Add variables for second tubes on shower counter
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
      parameter (here='S_CORRECT_CAL_POS')
*
      real*4 x,y         !Impact point coordinates
      real*4 s_correct_cal_pos
*
      include 'sos_data_structures.cmn'
      include 'sos_calorimeter.cmn'
*
*
      s_correct_cal_pos=exp(y/200.) !200 cm atten length.
      s_correct_cal_pos=s_correct_cal_pos/(1. + y*y/8000.)

*
      return
      end
