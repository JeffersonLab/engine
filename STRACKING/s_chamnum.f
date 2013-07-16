      function s_chamnum(ispace_point)
*     This function returns the chamber number of a space point
*      d.f. geesaman              8 Sept 1993
* $Log: s_chamnum.f,v $
* Revision 1.2  1995/05/22 19:45:33  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.1  1994/02/21  16:07:27  cdaq
* Initial revision
*
      implicit none
      include "sos_data_structures.cmn"
      include "sos_tracking.cmn"
      include "sos_geometry.cmn"
*     output
      integer*4 s_chamnum
*     input
      integer*4 ispace_point
*     local variables
      integer*4 plane
      s_chamnum=0
      plane=SDC_PLANE_NUM(sspace_point_hits(ispace_point,3))
      if(plane.gt.0 .and. plane.le. sdc_num_planes) then
          s_chamnum=sdc_chamber_planes(plane)
      endif
      return
      end
*
