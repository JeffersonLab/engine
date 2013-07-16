      function h_drift_time_calc(plane,wire,tdc)
*
*     function to calculate hms drift time from tdc value in hms
*     wire chambers
*
*     d.f. geesaman              17 feb 1994
* $Log: h_drift_time_calc.f,v $
* Revision 1.5  1995/10/09 20:16:02  cdaq
* (JRA) Remove monte carlo data option
*
* Revision 1.4  1995/05/22 19:39:09  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.3  1994/10/11  21:07:05  cdaq
* (JRA) Replace simple functions with existing ctp variables
*
* Revision 1.2  1994/03/24  18:51:52  cdaq
* (DFG) Allow switch for monte carlo data
*
* Revision 1.1  1994/02/19  06:14:04  cdaq
* Initial revision
*
*     
*  
      implicit none
      include "hms_data_structures.cmn"
      include "hms_tracking.cmn"
      include "hms_geometry.cmn"
*
*     input
*
      integer*4  plane      !  plane number of hit
      integer*4  wire       !  wire number  of hit
      integer*4  tdc        !  tdc value
*
*     output
*
      real*4     h_drift_time_calc      !  drift time in nanoseconds
*
      h_drift_time_calc = hstart_time
     &        - float(tdc)*hdc_tdc_time_per_channel
     &        + hdc_plane_time_zero(plane)
      return
      end

