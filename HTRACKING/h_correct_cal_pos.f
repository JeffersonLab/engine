*=======================================================================
      function h_correct_cal_pos(x,y,abort,errmsg)
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
* Revision 1.1  1999/01/21 21:40:14  saw
* Extra shower counter tube modifications
*
* Revision 1.5  1996/01/16 21:46:10  cdaq
* (JRA) Yet another sign change of quadratic term in attenuation correction
*
* Revision 1.4  1995/08/31 14:59:37  cdaq
* (JRA) Change sign of quadratic term in attenuation correction
*
* Revision 1.3  1995/05/22  19:39:08  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.2  1994/11/22  20:02:52  cdaq
* (???) Hack in a correction for attenuation length
*
* Revision 1.1  1994/04/12  21:30:48  cdaq
* Initial revision
*
*-----------------------------------------------------------------------
*
      implicit none
      save
*
      logical abort
      character*(*) errmsg
      character*13 here
      parameter (here='H_CORRECT_CAL_POS')
*
      real*4 x,y         !Impact point coordinates
      real*4 h_correct_cal_pos
*
      include 'hms_data_structures.cmn'
      include 'hms_calorimeter.cmn'
*
*
      h_correct_cal_pos=exp(y/200.) !200 cm atten length.
      h_correct_cal_pos=h_correct_cal_pos/(1. + y*y/8000.)

*
      return
      end
