      function s_wire_center_calc(plane,wire)
*
*     function to calculate sos wire center positions in sos
*     wire chambers
*
*     d.f. geesaman              17 feb 1994
* $Log$
* Revision 1.1  1994/02/21 16:44:09  cdaq
* Initial revision
*
*  
      implicit none
      include "gen_data_structures.cmn"
      include "sos_geometry.cmn"
*
*     input
*
      integer*4  plane      ! plane number of hit
      integer*4  wire       ! wire number  of hit
*
*     output
*
      real*4     s_wire_center_calc       !  wire center in cm
*
      s_wire_center_calc = (FLOAT(wire)-sdc_central_wire(plane))
     &                     * sdc_pitch(plane)
      return
      end
