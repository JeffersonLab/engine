      function s_tdc_zero(plane,wire)
*
*     routinne to return tdc_zero offset (in ns) for a given sos plane and
*     wire
*
*     d.f. geesaman      17 feb 1994        first dummy routine
* $Log$
* Revision 1.1  1994/02/21 16:41:38  cdaq
* Initial revision
*
*
*     inputs
*     
*     integer*4      plane     sos plane number of hit
*     integer*4      wire      sos wire number of hit
*
*     output
*    
*     real*4         s_tdc_zero  offset
*
*      s_drift_time_calc = SSTART_TIME 
*     &      - FLOAT(tdc)*s_tdc_time_per_channel(plane,wire)
*     &      + s_tdc_zero(plane,wire)                     
*
      implicit none
      integer*4 plane,wire
      real*4 s_tdc_zero
      s_tdc_zero=0.      
      return
      end
