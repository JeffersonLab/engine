      function s_drift_time_calc(plane,wire,tdc)
*
*     function to calculate sos drift time from tdc value in sos
*     wire chambers
*
*     d.f. geesaman              17 feb 1994
* $Log$
* Revision 1.1  1994/02/21 16:08:30  cdaq
* Initial revision
*
*  
      implicit none
      include "gen_data_structures.cmn"
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
      external s_tdc_time_per_channel,s_tdc_zero
      real*4 s_tdc_time_per_channel,s_tdc_zero
*
      s_drift_time_calc = SSTART_TIME 
     &      - FLOAT(tdc)*s_tdc_time_per_channel(plane,wire)
     &      + s_tdc_zero(plane,wire)                     
      return
      end
