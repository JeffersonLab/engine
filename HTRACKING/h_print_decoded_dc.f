       SUBROUTINE  h_print_decoded_dc(ABORT,err)
*--------------------------------------------------------
*-
*-   Purpose and Methods : Dump HMS_DECODED_DC BANKS
*-
*-      Required Input BANKS     HMS_DECODED_DC
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
*-   Created 29-FEB-1994   D. F. Geesaman
* $Log: h_print_decoded_dc.f,v $
* Revision 1.4  1995/10/10 16:51:54  cdaq
* (JRA) Remove drift distance from print out
*
* Revision 1.3  1995/05/22 19:39:16  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.2  1995/01/27  19:10:23  cdaq
* (JRA) Trivial write statement format changes
*
* Revision 1.1  1994/03/24  20:15:18  cdaq
* Initial revision
*
*--------------------------------------------------------
       IMPLICIT NONE
       SAVE
*
       character*50 here
       parameter (here= 'h_print_decoded_dc')
*
       logical ABORT
       character*(*) err
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
       write(hluno,'(''        HMS_DECODED_DC BANKS'')')
       write(hluno,'(''     HDC_TOT_HITS='',I4)') HDC_TOT_HITS
       if(HDC_TOT_HITS.GT.0) then
         write(hluno,'(''     HDC_HITS_PER_PLANE'')')
         write(hluno,'('' Plane='',18i4)') (j,j=1,hdc_num_planes)
         write(hluno,'(7x,18i4)')
     &      (HDC_HITS_PER_PLANE(j),j=1,hdc_num_planes)
         write(hluno,'('' Num  Plane     Wire    Wire Center '',
     &      ''TDC Value RAW DRIFT TIME'')')
         write(hluno,'(1x,i2,2x,i3,7x,i4,5x,F10.5,i8,2x,F10.5)')
     &        (j,HDC_PLANE_NUM(j),HDC_WIRE_NUM(j),
     &        HDC_WIRE_CENTER(j),HDC_TDC(j),HDC_DRIFT_TIME(j),
     &        j=1,HDC_TOT_HITS)
       endif
       RETURN
       END
