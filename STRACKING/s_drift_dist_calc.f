      function s_drift_dist_calc(plane,wire,time)
*
*     function to calculate sos drift time from tdc value in sos
*     wire chambers
*
*     d.f. geesaman              17 feb 1994
* $Log$
* Revision 1.1  1994/02/21 16:08:13  cdaq
* Initial revision
*
*  
      implicit none
      include "gen_data_structures.cmn"
      include "sos_geometry.cmn"
*
*     input
*
      integer*4  plane      !  plane number of hit
      integer*4  wire       !  wire number  of hit
      real*4     time       !  drift time in ns
*
*     output
*
      real*4     s_drift_dist_calc        !  drift distance in cm
*
      s_drift_dist_calc = time*sdrift_velocity
      return
      end
