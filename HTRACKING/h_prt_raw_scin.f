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
* Revision 1.2  1994/08/03 14:19:31  cdaq
* (JRA) Fix variable names
*
* Revision 1.1  1994/04/13  15:43:19  cdaq
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
      include 'tmp_pedestals.dte'
*
*--------------------------------------------------------
      ABORT = .FALSE.
      err = ' '
      write(hluno,'(''        HMS_RAW_SCIN BANKS'')')
      write(hluno,'(''     HSCIN_ALL_TOT_HITS='',I4)') HSCIN_ALL_TOT_HITS
      if(HSCIN_ALL_TOT_HITS.GT.0) then
        write(hluno,'('' Num  Plane    Counter        ADC_POS''
     &       '' ADC_NEG  TDC_POS  TDC_NEG'')')
        write(hluno,'(1x,i2,2x,i3,7x,i4,8x,4i8)')
     &       (j,HSCIN_ALL_PLANE_NUM(j),HSCIN_ALL_COUNTER_NUM(j),
     &       (HSCIN_ALL_ADC_POS(j)-HSCIN_ALL_PED_POS(j)),
     &       (HSCIN_ALL_ADC_NEG(j)-HSCIN_ALL_PED_NEG(j)),
     &       HSCIN_ALL_TDC_POS(j),
     &       HSCIN_ALL_TDC_NEG(j),
     &       j=1,HSCIN_ALL_TOT_HITS )
      endif
      RETURN
      END
