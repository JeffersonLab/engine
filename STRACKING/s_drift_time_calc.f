      function s_drift_time_calc(plane,wire,tdc)
*
*     function to calculate sos drift time from tdc value in sos
*     wire chambers
*
*     d.f. geesaman              17 feb 1994
* $Log$
* Revision 1.3  1994/11/22 21:10:31  cdaq
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
*     monte Carlo data does not set SSTART_TIME properly
*     check debuging switch
      if( sdebug_mc_start_time .ne. 0) then 
         s_drift_time_calc = 0.
     &        - FLOAT(tdc)*sdc_tdc_time_per_channel
     &        + sdc_plane_time_zero(plane)

      else
         s_drift_time_calc = SSTART_TIME 
     &        - FLOAT(tdc)*sdc_tdc_time_per_channel
     &        + sdc_plane_time_zero(plane)
      endif                             !  end check on monte carlo start time
      return
      end
