*=======================================================================
      function h_correct_cal(x,y,abort,errmsg)
*=======================================================================
*-
*-      Purpose: Returns the impact point correction factor. This
*-               factor is to be applied to the energy depositions.
*-
*-      Input Parameters: x,y - impact point coordinates
*-
*-      Created 15 Mar 1994      Tsolak A. Amatuni
*
* $Log$
* Revision 1.2  1994/11/22 20:02:52  cdaq
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
      parameter (here='H_CORRECT_CAL')
*
      real*4 x,y         !Impact point coordinates
      real*4 h_correct_cal
*
      include 'gen_data_structures.cmn'
      include 'hms_calorimeter.cmn'
*
*
*      h_correct_cal=1.
      h_correct_cal=exp(y/200.) !200 cm atten length.
      h_correct_cal=h_correct_cal/(1. + y*y/8000.)

*
      return
      end
