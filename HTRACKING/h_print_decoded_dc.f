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
* $Log$
* Revision 1.1  1994/03/24 20:15:18  cdaq
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
       include 'gen_data_structures.cmn'
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
         if(  hdebug_mc_start_time .ne. 0 ) then
           write(hluno,'(''  hdebug_mc_start_time is set.'',
     &     ''  HSTART_TIME is set to zero in DC decoding'')')
         endif
         write(hluno,'(''     HDC_HITS_PER_PLANE'')')
         write(hluno,'('' Plane='',18i4)') (j,j=1,hdc_num_planes)
         write(hluno,'(7x,18i4)')
     &      (HDC_HITS_PER_PLANE(j),j=1,hdc_num_planes)
         write(hluno,'('' Num  Plane     Wire    Wire Center '',
     &      ''TDC Value DRIFT TIME DRIFT DIST COORD'')')
         write(hluno,'(1x,i2,2x,i3,7x,i4,5x,F10.5,i8,2x,3F10.5)')       
     &     (j,HDC_PLANE_NUM(j),HDC_WIRE_NUM(j),
     &        HDC_WIRE_CENTER(j),HDC_TDC(j),HDC_DRIFT_TIME(j),
     &        HDC_DRIFT_DIS(j),HDC_WIRE_COORD(j),
     &        j=1,HDC_TOT_HITS)    
       endif
       RETURN
       END
