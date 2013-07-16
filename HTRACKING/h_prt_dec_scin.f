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
* $Log: h_prt_dec_scin.f,v $
* Revision 1.8  1996/01/16 21:55:27  cdaq
* (JRA)
*
* Revision 1.7  1995/05/22 19:39:23  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.6  1995/02/02  16:10:49  cdaq
* (JRA) Make hscin_all_adc_pos/neg floating
*
* Revision 1.5  1994/09/13  20:23:29  cdaq
* *** empty log message ***
*
* Revision 1.4  1994/09/13  20:20:21  cdaq
* (JRA) Change output format, add missing variables
*
* Revision 1.3  1994/08/02  20:00:48  cdaq
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
      include 'hms_data_structures.cmn'
      include 'gen_constants.par'
      include 'gen_units.par'
      include 'hms_tracking.cmn'
*
*--------------------------------------------------------
      ABORT = .FALSE.
      err = ' '

      write(hluno,'(''           ***HMS_REAL_SCIN BANKS***'')')
      write(hluno,'(''     HSCIN_TOT_HITS='',I4)') HSCIN_TOT_HITS
      if(HSCIN_TOT_HITS.GT.0) then
        write(hluno,'('' Num  Plane    Counter        ADC_POS'',
     &       '' ADC_NEG  TDC_POS  TDC_NEG'')')
        write(hluno,'(1x,i2,2x,i3,5x,i4,8x,2f8.2,2i8)')
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
        write(hluno,'('' Num    ZPOS    CENTER  HIT_COORD SLOP'',
     &       ''   COR_TDC  TWO_GOOD'')')
        write(hluno,'(1x,i2,2x,4f9.3,f10.3,4x,l2)')
     &       (j,HSCIN_ZPOS(j),HSCIN_CENTER_COORD(j),
     &       HSCIN_DEC_HIT_COORD(j),
     &       HSCIN_SLOP(j),HSCIN_COR_TIME(j),
     &       HTWO_GOOD_TIMES(j), 
     &       j=1,HSCIN_TOT_HITS)    
        write(hluno,'('' HGOOD_START_TIME='', l2)')
     &       HGOOD_START_TIME
        write(hluno,'('' HSTART_TIME='',e10.4)') HSTART_TIME
        write(hluno,*)
      endif
      RETURN
      END
