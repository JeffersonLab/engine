       SUBROUTINE  h_prt_raw_scin(ABORT,err)
*--------------------------------------------------------
*-
*-   Purpose and Methods : Dump HMS_RAW_SCIN BANKS
*-
*-      Required Input BANKS     HMS_RAW_SCIN
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
*-   Created 29-FEB-1994   D. F. Geesaman
* $Log$
* Revision 1.1  1994/04/13 15:43:19  cdaq
* Initial revision
*
*--------------------------------------------------------
       IMPLICIT NONE
       SAVE
*
       character*50 here
       parameter (here= 'h_prt_raw_scin')
*
       logical ABORT
       character*(*) err
*
       integer*4 j
       include 'gen_data_structures.cmn'
       include 'gen_constants.par'
       include 'gen_units.par'
       include 'hms_tracking.cmn'
*
*--------------------------------------------------------
       ABORT = .FALSE.
       err = ' '
       write(hluno,'(''        HMS_RAW_SCIN BANKS'')')
       write(hluno,'(''     HSCIN_TOT_HITS='',I4)') HSCIN_TOT_HITS
       if(HSCIN_TOT_HITS.GT.0) then
         write(hluno,'('' Num  Plane    Counter        ADC_POS''
     &   '' ADC_NEG  TDC_POS  TDC_NEG'')')
         write(hluno,'(1x,i2,2x,i3,7x,i4,8x,4i8)')
     &     (j,HSCIN_PLANE_NUM(j),HSCIN_COUNTER_NUM(j),
     &        HSCIN_ADC_POS(j),HSCIN_ADC_NEG(j),
     &        HSCIN_TDC_POS(j),HSCIN_TDC_NEG(j),
     &        j=1,HSCIN_TOT_HITS )
       endif
       RETURN
       END
