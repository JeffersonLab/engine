       SUBROUTINE  h_print_raw_dc(ABORT,err)
*--------------------------------------------------------
*-
*-   Purpose and Methods : Dump HMS_RAW_DC BANKS
*-
*-      Required Input BANKS     HMS_RAW_DC
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
*-   Created 29-FEB-1994   D. F. Geesaman
* $Log$
* Revision 1.2.24.1  2008/07/29 16:38:19  puckett
* made output of h_print_raw_dc more informative
*
* Revision 1.2  1995/05/22 19:39:17  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.1  1994/03/24  20:15:58  cdaq
* Initial revision
*
*--------------------------------------------------------
       IMPLICIT NONE
       SAVE
*
       character*50 here
       parameter (here= 'h_print_raw_dc')
*
       logical ABORT
       character*(*) err

       real*4 wcenter ! calculate wire center from wire number
*
       integer*4 j
       include 'hms_data_structures.cmn'
       include 'gen_constants.par'
       include 'gen_units.par'
       include 'hms_tracking.cmn'
       include 'hms_geometry.cmn'          
*
*--------------------------------------------------------
       ABORT = .FALSE.
       err = ' '
       write(hluno,'(''        HMS_RAW_DC BANKS'')')
       write(hluno,'(''     HDC_RAW_TOT_HITS='',I4)') HDC_RAW_TOT_HITS
       if(HDC_RAW_TOT_HITS.GT.0) then
         write(hluno,
     $         '('' Num  Plane     Wire     Center     TDC Value'')')
         do j=1,hdc_raw_tot_hits
            if(hdc_wire_counting(hdc_raw_plane_num(j)).eq.0) then
               wcenter = ( float(hdc_raw_wire_num(j) ) - 
     $              hdc_central_wire(hdc_raw_plane_num(j)) ) * 
     $              hdc_pitch(hdc_raw_plane_num(j)) -
     $              hdc_center(hdc_raw_plane_num(j))
            else
               wcenter = ( float(hdc_nrwire(hdc_raw_plane_num(j)) + 1 - 
     $              hdc_raw_wire_num(j) ) - 
     $              hdc_central_wire(hdc_raw_plane_num(j)) ) * 
     $              hdc_pitch(hdc_raw_plane_num(j)) - 
     $              hdc_center(hdc_raw_plane_num(j))
            endif
            write(hluno,
     $           '(1x,i2,2x,i3,7x,i4,5x,f10.5,5x,i10)')
     $           j,hdc_raw_plane_num(j),hdc_raw_wire_num(j),wcenter,
     $           hdc_raw_tdc(j)
         enddo
       endif
       RETURN
       END
