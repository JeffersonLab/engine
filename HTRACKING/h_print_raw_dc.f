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
* Revision 1.1  1994/03/24 20:15:58  cdaq
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
*
       integer*4 j
       include 'gen_data_structures.cmn'
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
         write(hluno,'('' Num  Plane     Wire          TDC Value'')')
         write(hluno,'(1x,i2,2x,i3,7x,i4,5x,i10)')
     &     (j,HDC_RAW_PLANE_NUM(j),HDC_RAW_WIRE_NUM(j),
     &        HDC_RAW_TDC(j),j=1,HDC_RAW_TOT_HITS)    
       endif
       RETURN
       END
