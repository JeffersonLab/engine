
       SUBROUTINE s_prt_raw_scin(ABORT,err)
*--------------------------------------------------------
*-
*-   Purpose and Methods : Dump SOS_RAW_SCIN BANKS
*-
*-      Required Input BANKS     SOS_RAW_SCIN
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
*-   Created 29-FEB-1994   D. F. Geesaman
* $Log$
* Revision 1.1  1994/04/13 18:21:45  cdaq
* Initial revision
*
*--------------------------------------------------------
       IMPLICIT NONE
       SAVE
*
       character*50 here
       parameter (here= 's_prt_raw_scin')
*
       logical ABORT
       character*(*) err
*
       integer*4 j
       include 'gen_data_structures.cmn'
       include 'gen_constants.par'
       include 'gen_units.par'
       include 'sos_tracking.cmn'
*
*--------------------------------------------------------
       ABORT = .FALSE.
       err = ' '
       write(sluno,'(''        SOS_RAW_SCIN BANKS'')')
       write(sluno,'(''     SSCIN_TOT_HITS='',I4)') SSCIN_TOT_HITS
       if(SSCIN_TOT_HITS.GT.0) then
         write(sluno,'('' Num  Plane    Counter        ADC_POS''
     &   '' ADC_NEG  TDC_POS  TDC_NEG'')')
         write(sluno,'(1x,i2,2x,i3,7x,i4,8x,4i8)')
     &     (j,SSCIN_PLANE_NUM(j),SSCIN_COUNTER_NUM(j),
     &        SSCIN_ADC_POS(j),SSCIN_ADC_NEG(j),
     &        SSCIN_TDC_POS(j),SSCIN_TDC_NEG(j),
     &        j=1,SSCIN_TOT_HITS )
       endif
       RETURN
       END
