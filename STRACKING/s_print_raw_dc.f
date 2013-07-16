       SUBROUTINE  s_print_raw_dc(ABORT,err)
*--------------------------------------------------------
*-
*-   Purpose and Methods : Dump SOS_RAW_DC BANKS
*-
*-      Required Input BANKS     SOS_RAW_DC
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
*-   Created 29-FEB-1994   D. F. Geesaman
* $Log: s_print_raw_dc.f,v $
* Revision 1.2  1995/05/22 19:45:46  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.1  1994/03/24  20:30:01  cdaq
* Initial revision
*
*--------------------------------------------------------
       IMPLICIT NONE
       SAVE
*
       character*50 here
       parameter (here= 's_print_raw_dc')
*
       logical ABORT
       character*(*) err
*
       integer*4 j
       include 'sos_data_structures.cmn'
       include 'gen_constants.par'
       include 'gen_units.par'
       include 'sos_tracking.cmn'
       include 'sos_geometry.cmn'          
*
*--------------------------------------------------------
       ABORT = .FALSE.
       err = ' '
       write(sluno,'(''        SOS_RAW_DC BANKS'')')
       write(sluno,'(''    SDC _RAW_TOT_HITS='',I4)') SDC_RAW_TOT_HITS
       if(SDC_RAW_TOT_HITS.GT.0) then
         write(sluno,'('' Num  Plane     Wire          TDC Value'')')
         write(sluno,'(1x,i2,2x,i3,7x,i4,5x,i10)')
     &     (j,SDC_RAW_PLANE_NUM(j),SDC_RAW_WIRE_NUM(j),
     &        SDC_RAW_TDC(j),j=1,SDC_RAW_TOT_HITS)    
       endif
       RETURN
       END
