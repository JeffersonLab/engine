*=======================================================================
      function s_correct_cal(x,y,abort,errmsg)
*=======================================================================
*-
*-      Purpose: Returns the impact point correction factor. This
*-               factor is to be applied to the energy depositions.
*-
*-      Input Parameters: x,y - impact point coordinates
*-
*-      Created 15 Mar 1994      Tsolak A. Amatuni
* $Log$
* Revision 1.3  1995/05/11 21:02:59  cdaq
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
      logical abort
      character*(*) errmsg
      character*13 here
      parameter (here='S_CORRECT_CAL')
*
      real*4 x,y         !Impact point coordinates
      real*4 s_correct_cal
*
      include 'gen_data_structures.cmn'
      include 'sos_calorimeter.cmn'
*
*
      s_correct_cal=1.
      s_correct_cal=exp(-y/400.)  !400 cm atten length.
      s_correct_cal=s_correct_cal/(1. - y*y/12000)
*
      return
      end
