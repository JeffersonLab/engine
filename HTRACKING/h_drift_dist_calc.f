      function h_drift_dist_calc(plane,wire,time)
*
*     function to calculate hms drift time from tdc value in hms
*     wire chambers
*
*     d.f. geesaman              17 feb 1994
* $Log$
* Revision 1.1  1994/02/19 06:13:44  cdaq
* Initial revision
*
*     
*  
      implicit none
      include "gen_data_structures.cmn"
      include "hms_geometry.cmn"
*
*     input
*
      integer*4  plane      !  plane number of hit
      integer*4  wire       !  wire number  of hit
      real*4     time       !  drift time in ns
*
*     output
*
      real*4     h_drift_dist_calc        !  drift distance in cm
*
      h_drift_dist_calc = time*hdrift_velocity
      return
      end
