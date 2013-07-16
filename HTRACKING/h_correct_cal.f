*=======================================================================
      function h_correct_cal(x,y)
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
* $Log: h_correct_cal.f,v $
* Revision 1.7  2003/04/03 00:43:13  jones
* Update to calibration (V. Tadevosyan0
*
* Revision 1.6  2003/03/21 22:33:22  jones
* Subroutines had arguments with abort,errmsg . But these arguments were not
* used when the subroutine was called. Also abort ,errmsg were not used in the
* subroutines. So eliminate abort,errmsg. (E. Brash)
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
*      logical abort
*      character*(*) errmsg
      character*13 here
      parameter (here='H_CORRECT_CAL')
*
      real*4 x,y         !Impact point coordinates
      real*4 h_correct_cal
*
      include 'hms_data_structures.cmn'
      include 'hms_calorimeter.cmn'
*
*
c     Check calorimeter boundaries.

      if(y.lt.hcal_ymin) y=hcal_ymin
      if(y.gt.hcal_ymax) y=hcal_ymax
*
      h_correct_cal=exp(y/200.) !200 cm atten length.
      h_correct_cal=h_correct_cal/(1. + y*y/8000.)

*
      return
      end
