      real*4 function h_drift_dist_calc(plane,wire,time)
*
*     function to calculate hms drift time from tdc value in hms
*     wire chambers
*
*     d.f. geesaman              17 feb 1994
* $Log$
* Revision 1.4  1994/11/22 20:03:46  cdaq
* (SAW) Change fract to hfract.  Make fractinterp a local variable
*
* Revision 1.3  1994/08/18  03:34:21  cdaq
* (DJM) Use a lookup table to map fractional area to distance.  (worked for
* the prototype chamber).  Lots of parameters but easier than fitting.
* Could not use a single map for all 12 planes since drift time spectra a
* very different.
*
* Revision 1.2  1994/07/27  19:00:07  cdaq
* (DJM) map fractional area to distance. worked for the prototype chamber!
* (DFG) Add two regions of drift (commented out)
*
* Revision 1.1  1994/02/19  06:13:44  cdaq
* Initial revision
*  
      implicit none
      include 'gen_data_structures.cmn'
      include 'hms_geometry.cmn'
      include 'hms_tracking.cmn'        ! for lookup tables
*
*     input
*
      integer*4  plane      !  plane number of hit
      integer*4  wire       !  wire number  of hit
      integer*4  ilo,ihi    !  interpolate between bins ilo and ilo+1
      real*4     time       !  drift time in ns
      real*4     fractinterp        !  interpolated fraction 
*
*     output
*

* look in the appropriate drift time to distance table and perform a linear
* interpolation. minimum and maximum distance values are 0.0cm and 0.5cm. 
      if( hdriftbinsz.eq.0.0)then
        fractinterp = -1.0
        h_drift_dist_calc = 0.5*fractinterp
        return
      endif      
      ilo = int(time/hdriftbinsz) - int(hdrift1stbin/hdriftbinsz) + 1
      ihi = ilo + 1
      if( ilo.ge.1 .and. ihi.le.hdriftbins)then 
        fractinterp = hfract(ilo,plane) + 
     &    ( (hfract(ilo+1,plane)-hfract(ilo,plane))/hdriftbinsz )*
     &    (time - (ilo-1)*hdriftbinsz - hdrift1stbin)
      else
        if( ilo.lt.1 )then
          fractinterp = 0.0
        else
          if( ihi.gt.hdriftbins )fractinterp = 1.0
        endif
      endif

      h_drift_dist_calc = 0.5*fractinterp

      return
      end
