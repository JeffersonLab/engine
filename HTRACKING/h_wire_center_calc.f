      function h_wire_center_calc(plane,wire)
*
*     function to calculate hms wire center positions in hms
*     wire chambers
*
*     d.f. geesaman              17 feb 1994
*          
*     modified   dfg  18 feb 1994
*                         add option to reverse plane wire numbering
* $Log$
* Revision 1.2  1994/02/22 05:34:03  cdaq
* (SAW) Remove FLOAT call on floating arg
*
* Revision 1.1  1994/02/19  06:22:00  cdaq
* Initial revision
*
*     
*  
      implicit none
      include "gen_data_structures.cmn"
      include "hms_geometry.cmn"
*
*     input
*
      integer*4  plane      ! plane number of hit
      integer*4  wire       ! wire number  of hit
*
*     output
*
      real*4     h_wire_center_calc       !  wire center in cm
*
*     if hdc_sire_counting(plane) is 1 then wires are number in reverse order
      if(hdc_wire_counting(plane).eq.0) then 
*          normal ordering
           h_wire_center_calc = (FLOAT(wire)-hdc_central_wire(plane))
     &                     * hdc_pitch(plane)
      else        
           h_wire_center_calc = 
     &         ((hdc_nrwire(plane) + FLOAT(1 - wire))- hdc_central_wire(plane))
     &                     * hdc_pitch(plane)
      endif
      return
      end
