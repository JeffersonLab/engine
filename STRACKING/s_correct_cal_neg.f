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
* $Log: s_correct_cal_neg.f,v $
* Revision 1.4  2003/04/03 00:45:01  jones
* Update to calorimeter calibration (V. Tadevosyan)
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
*
*
      real*4 x,y                ! Impact point coordinates
      real*4 s_correct_cal_neg
*
      include 'sos_data_structures.cmn'
      include 'sos_calorimeter.cmn'
*

c     Check calorimeter boundaries.

      if(y.lt.scal_ymin) y=scal_ymin
      if(y.gt.scal_ymax) y=scal_ymax

c     Tuned to straight through pions of run #23121. Works well for |Y|<20.

      s_correct_cal_neg=(100.+y)/(100.+y/3.)

      end
