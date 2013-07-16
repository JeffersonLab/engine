      function s_drift_time_calc(plane,wire,tdc)
*
*     function to calculate sos drift time from tdc value in sos
*     wire chambers
*
*     d.f. geesaman              17 feb 1994
* $Log: s_drift_time_calc.f,v $
* Revision 1.5  1995/10/09 20:16:16  cdaq
* (JRA) Remove monte carlo data option
*
* Revision 1.4  1995/05/22 19:45:36  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.3  1994/11/22  21:10:31  cdaq
* (SPB) Recopied from hms file and modified names for SOS
*
* Revision 1.2  1994/03/24  19:52:20  cdaq
* (DFG) Allow switch for monte carlo data
*
* Revision 1.1  1994/02/21  16:08:30  cdaq
* Initial revision
*
*  
      implicit none
      include "sos_data_structures.cmn"
      include "sos_tracking.cmn"
      include "sos_geometry.cmn"
*
*     input
*
      integer*4  plane      !  plane number of hit
      integer*4  wire       !  wire number  of hit
      integer*4  tdc        !  tdc value
*
*     output
*
      real*4     s_drift_time_calc       !  drift time in nanoseconds

*
      s_drift_time_calc = sstart_time
     &     - float(tdc)*sdc_tdc_time_per_channel
     &     + sdc_plane_time_zero(plane)
      return
      end
