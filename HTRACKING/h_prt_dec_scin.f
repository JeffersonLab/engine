      SUBROUTINE  h_prt_dec_scin(ABORT,err)
*--------------------------------------------------------
*-
*-   Purpose and Methods : Dump HMS_DECODED_SCIN BANKS
*-
*-      Required Input BANKS     HMS_DECODED_SCIN
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
*-   Created 29-FEB-1994   D. F. Geesaman
* $Log$
* Revision 1.3  1994/08/02 20:00:48  cdaq
* (JRA) Print out some additional information
*
* Revision 1.2  1994/05/12  21:01:39  cdaq
* (DFG) Fix typo
*
* Revision 1.1  1994/04/13  15:42:58  cdaq
* Initial revision
*
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*50 here
      parameter (here= 'h_prt_dec_scin')
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

      write(hluno,'(''        HMS_REAL_SCIN BANKS'')')
      write(hluno,'(''     HSCIN_TOT_HITS='',I4)') HSCIN_TOT_HITS
      if(HSCIN_TOT_HITS.GT.0) then
        write(hluno,'('' Num  Plane    Counter        ADC_POS''
     &       '' ADC_NEG  TDC_POS  TDC_NEG'')')
        write(hluno,'(1x,i2,2x,i3,7x,i4,8x,4i8)')
     &       (j,HSCIN_PLANE_NUM(j),HSCIN_COUNTER_NUM(j),
     &       HSCIN_ADC_POS(j),HSCIN_ADC_NEG(j),
     &       HSCIN_TDC_POS(j),HSCIN_TDC_NEG(j),
     &       j=1,HSCIN_TOT_HITS )
      endif
      
      write(hluno,'(''        HMS_DECODED_SCIN BANKS'')')
      if(HSCIN_TOT_HITS.GT.0) then
        write(hluno,'('' Scintillator hits per plane'')')
        write(hluno,'('' Plane  '',10i4)') (j,j=1,HNUM_SCIN_PLANES)   
        write(hluno,'('' Number '',10i4)') 
     &       (HSCIN_HITS_PER_PLANE(j),j=1,HNUM_SCIN_PLANES)
        write(hluno,'('' Num    ZPOS    CENTER  HIT_COORD WIDTH  SLOPE''
     &       ''  COR_ADC  COR_TDC  TWO_GOOD'')')
        write(hluno,'(1x,i2,2x,5f8.3,2f10.3,4x,l2)')
     &       (j,HSCIN_ZPOS(j),HSCIN_CENTER_COORD(j),
     &       HSCIN_DEC_HIT_COORD(j),HSCIN_WIDTH(j),
     &       HSCIN_SLOP(j),HSCIN_COR_ADC(j),HSCIN_COR_TIME(j),
     &       HTWO_GOOD_TIMES(j), 
     &       j=1,HSCIN_TOT_HITS)    
        write(hluno,'('' HGOOD_START_TIME='', l2,
     &       ''  HGOOD_START_PLANE'',l2)')
     &       HGOOD_START_TIME,  HGOOD_START_PLANE
        write(hluno,'('' HSTART_TIME='',e10.4, 
     &       '' HSTART_HITNUM='',i3,''   HSTART_HITSIDE='',i3)')
     &       HSTART_TIME, HSTART_HITNUM, HSTART_HITSIDE
      endif
      RETURN
      END
