*=======================================================================
      function s_correct_cal_two(x,y)
*=======================================================================
*-
*-  Purpose: Returns the impact point correction factor. This
*-           factor is to be applied to the energy depositions.
*-           (This correction for the case when "POS_PMT"+"NEG_PMT".
*-           The final energy is the ADC value TIMES the correction factor.
*-
*-      Input Parameters: x,y - impact point coordinates
*-
*-      Created 27 September 1999      SAW
*
* $Log: s_correct_cal_two.f,v $
* Revision 1.2  2003/03/21 22:58:02  jones
* Subroutines had arguments with abort,errmsg . But these arguments were not
* used when the subroutine was called. Also abort ,errmsg were not used in the
* subroutines. So eliminate abort,errmsg. (E. Brash)
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
      parameter (here='S_CORRECT_CAL_TWO')
*
*
      real*4 x,y         !Impact point coordinates
      real*4 s_correct_cal_two
*
      include 'sos_data_structures.cmn'
      include 'sos_calorimeter.cmn'
*
* ! I was used simple combination of "s_correct_cal_pos" and 
*   and "s_correct_cal_neg". To be corrected !! (Hamlet)
*
      s_correct_cal_two=exp(y/200.)+exp(-y/200.)     !200 cm atten length.    
      s_correct_cal_two=s_correct_cal_two*(1. + y*y/8000.)  
* 
      return
      end
