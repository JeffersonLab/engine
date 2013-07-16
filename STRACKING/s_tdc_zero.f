      function s_tdc_zero(plane,wire)
*
*     routine to return tdc_zero offset (in ns) for a given sos plane and
*     wire
*
*     d.f. geesaman      17 feb 1994        first dummy routine
* $Log: s_tdc_zero.f,v $
* Revision 1.3  1995/05/22 19:45:58  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.2  1994/06/14  04:38:30  cdaq
* (DFG) Make zero time a parameter
*
* Revision 1.1  1994/02/21  16:41:38  cdaq
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
      include 'sos_data_structures.cmn'
      include 'sos_tracking.cmn'
      include 'sos_geometry.cmn'            
      integer*4 plane,wire
      real*4 s_tdc_zero
      s_tdc_zero=sdc_plane_time_zero(plane)
      return
      end
