      function h_chamnum(ispace_point)
*     This function returns the chamber number of a space point
*      d.f. geesaman              17 January  1994
* $Log: h_chamnum.f,v $
* Revision 1.3  1996/04/30 12:32:51  saw
* (JRA) Remove (unneeded?) check on plane range
*
* Revision 1.2  1995/05/22 19:39:07  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.1  1994/02/19  06:13:14  cdaq
* Initial revision
*
      implicit none
      include "hms_data_structures.cmn"
      include "hms_tracking.cmn"
      include "hms_geometry.cmn"
*     output
      integer*4 h_chamnum
*     input
      integer*4 ispace_point
*     local variables
      integer*4 plane


      plane=HDC_PLANE_NUM(hspace_point_hits(ispace_point,3))
      h_chamnum=hdc_chamber_planes(plane)

      return
      end
*
