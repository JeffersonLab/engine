*=======================================================================
      function h_correct_cal_two(x,y,abort,errmsg)
*=======================================================================
*-
*-  Purpose: Returns the impact point correction factor. This
*-           factor is to be applied to the energy depositions.
*-           (This correction for the case when "POS_PMT"+"NEG_PMT".
*-           The final energy is the ADC value TIMES the correction factor.
*-
*-      Input Parameters: x,y - impact point coordinates
*-
*-      Created 09 October 1997      H. Mkrtchyan
*
* $Log$
* Revision 1.1  1999/01/21 21:40:14  saw
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
      character*13 here
      parameter (here='H_CORRECT_CAL_TWO')
*
*
      real*4 x,y         !Impact point coordinates
      real*4 h_correct_cal_two
*
      include 'hms_data_structures.cmn'
      include 'hms_calorimeter.cmn'
*
* ! I was used simple combination of "h_correct_cal_pos" and 
*   and "h_correct_cal_neg". To be corrected !! (Hamlet)
*
      h_correct_cal_two=exp(y/200.)+exp(-y/200.)     !200 cm atten length.    
      h_correct_cal_two=h_correct_cal_two*(1. + y*y/8000.)  
* 
      return
      end
