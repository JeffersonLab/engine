      function s_drift_time_calc(plane,wire,tdc)
*
*     function to calculate sos drift time from tdc value in sos
*     wire chambers
*
*     d.f. geesaman              17 feb 1994
* $Log$
* Revision 1.2  1994/03/24 19:52:20  cdaq
* (DFG) Allow switch for monte carlo data
*
* Revision 1.1  1994/02/21  16:08:30  cdaq
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
*     monte Carlo data does not set SSTART_TIME properly
*     check debuging switch
      if( sdebug_mc_start_time .ne. 0) then 
         s_drift_time_calc = 0.
     &        - FLOAT(tdc)*s_tdc_time_per_channel(plane,wire)
     &        + s_tdc_zero(plane,wire)                     

      else
         s_drift_time_calc = SSTART_TIME 
     &        - FLOAT(tdc)*s_tdc_time_per_channel(plane,wire)
     &        + s_tdc_zero(plane,wire)                     
      endif                             !  end check on monte carlo start time
      return
      end
