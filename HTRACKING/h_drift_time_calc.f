      function h_drift_time_calc(plane,wire,tdc)
*
*     function to calculate hms drift time from tdc value in hms
*     wire chambers
*
*     d.f. geesaman              17 feb 1994
* $Log$
* Revision 1.2  1994/03/24 18:51:52  cdaq
* (DFG) Allow switch for monte carlo data
*
* Revision 1.1  1994/02/19  06:14:04  cdaq
* Initial revision
*
*     
*  
      implicit none
      include "gen_data_structures.cmn"
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
      external h_tdc_time_per_channel,h_tdc_zero
      real*4 h_tdc_time_per_channel,h_tdc_zero
*
*     monte Carlo data does not set HSTART_TIME properly
*     check debuging switch
      if( hdebug_mc_start_time .ne. 0) then 
         h_drift_time_calc = 0.
     &        - FLOAT(tdc)*h_tdc_time_per_channel(plane,wire)
     &        + h_tdc_zero(plane,wire)                     

      else
         h_drift_time_calc = HSTART_TIME 
     &        - FLOAT(tdc)*h_tdc_time_per_channel(plane,wire)
     &        + h_tdc_zero(plane,wire)                     
      endif                             ! End check on monte carlo start time
      return
      end
