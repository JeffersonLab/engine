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
*
*
      real*4 x,y         !Impact point coordinates
      real*4 s_correct_cal_neg
*
      include 'sos_data_structures.cmn'
      include 'sos_calorimeter.cmn'
*
*   ! Here  I was used some preliminary function 
*
      s_correct_cal_neg=exp(-y/200.)      !200 cm atten length. 
      s_correct_cal_neg=s_correct_cal_neg*(1. + y*y/8000.) 
*   
      return
      end


