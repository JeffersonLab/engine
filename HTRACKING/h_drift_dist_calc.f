      function h_drift_dist_calc(plane,wire,time)
*
*     function to calculate hms drift time from tdc value in hms
*     wire chambers
*
*     d.f. geesaman              17 feb 1994
* $Log$
* Revision 1.2  1994/07/27 19:00:07  cdaq
* (DJM) map fractional area to distance. worked for the prototype chamber!
* (DFG) Add two regions of drift (commented out)
*
* Revision 1.1  1994/02/19  06:13:44  cdaq
* Initial revision
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
      real*4     timep      !  drift time in ns wrt expansion point
      real*4     fract      !  fraction of integrated drift time spectrum
*
*     output
*
      real*4     h_drift_dist_calc        !  drift distance in cm
*
*      if( time.gt. hdrift_t0_break(plane)) then
*          h_drift_dist_calc = (time-hdrift_t0_break(plane))*hdrift_velocity
*     &                        + hdrift_x0_break(plane)
*      else
*          h_drift_dist_calc = hdrift_x0_break(plane) *
*     &                        (abs(time)/hdrift_t0_break(plane))**.6666666
*      endif

* test time to get polynomial from blowing up on us. relax the following
* time window if offsets are poorly known at startup
      timep = time - 225.               !expand polynomial about point of zero slope.
      if( time.ge.-50. .and. time.le.250.) then
         fract = 1. - 6.5209162e-5*timep**2 - 1.9512024e-6*timep**3 
     &        - 1.9665608e-8*timep**4 - 7.3967721e-11*timep**5 
     &        - 9.3595921e-14*timep**6
         h_drift_dist_calc = 0.5*fract
      else
         if( time.lt.-50.) then
            h_drift_dist_calc = 0.0     !set to minimum value
         else
            if( time.gt. 250.)h_drift_dist_calc = 0.5 !set to maximum value
         endif      
      endif

      return
      end
