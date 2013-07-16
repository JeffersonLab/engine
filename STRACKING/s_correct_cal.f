*=======================================================================
      function s_correct_cal(x,y)
*=======================================================================
*-
*-      Purpose: Returns the impact point correction factor. This
*-               factor is to be applied to the energy depositions.
*-
*-      Input Parameters: x,y - impact point coordinates
*-
*-      Created 15 Mar 1994      Tsolak A. Amatuni
* $Log: s_correct_cal.f,v $
* Revision 1.7  2003/04/03 00:45:01  jones
* Update to calorimeter calibration (V. Tadevosyan)
*
* Revision 1.6  2003/03/21 22:58:02  jones
* Subroutines had arguments with abort,errmsg . But these arguments were not
* used when the subroutine was called. Also abort ,errmsg were not used in the
* subroutines. So eliminate abort,errmsg. (E. Brash)
*
* Revision 1.5  1999/01/29 17:34:57  saw
* Add variables for second tubes on shower counter
*
* Revision 1.4  1995/05/22 19:45:34  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.3  1995/05/11  21:02:59  cdaq
* (???) Tweak hardwired attenuation length
*
* Revision 1.2  1994/11/22  21:09:22  cdaq
* (SPB) Recopied from hms file and modified names for SOS
*
* Revision 1.1  1994/04/13  18:10:02  cdaq
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
      parameter (here='S_CORRECT_CAL')
*
      real*4 x,y         !Impact point coordinates
      real*4 s_correct_cal
*
      include 'sos_data_structures.cmn'
      include 'sos_calorimeter.cmn'
*
*

c     Check calorimeter boundaries.

      if(y.lt.scal_ymin) y=scal_ymin
      if(y.gt.scal_ymax) y=scal_ymax
*
      s_correct_cal=exp(-y/400.)  !400 cm atten length.
      s_correct_cal=s_correct_cal/(1. + y*y/12000)
*
      return
      end
