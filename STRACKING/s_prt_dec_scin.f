       SUBROUTINE  s_prt_dec_scin(ABORT,err)
*--------------------------------------------------------
*-
*-   Purpose and Methods : Dump SOS_DECODED_SCIN BANKS
*-
*-      Required Input BANKS     SOS_DECODED_SCIN
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
*-   Created 29-FEB-1994   D. F. Geesaman
* $Log: s_prt_dec_scin.f,v $
* Revision 1.7  1996/01/17 19:00:09  cdaq
* (JRA)
*
* Revision 1.6  1995/05/22 19:45:50  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.5  1995/04/06  19:40:51  cdaq
* (SAW) Fix typo
*
* Revision 1.4  1995/02/10  19:57:47  cdaq
* (JRA) Make sscin_all_adc_pos/neg floating
*
* Revision 1.4  1995/02/10  19:13:11  cdaq
* (JRA) Make sscin_all_adc_pos/neg floating
*
* Revision 1.3  1994/11/23  13:56:18  cdaq
* (SPB) Recopied from hms file and modified names for SOS
*
* Revision 1.2  1994/05/13  03:22:48  cdaq
* (DFG) Fix logical format statement
*
* Revision 1.1  1994/04/13  18:21:29  cdaq
* Initial revision
*
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*     
      character*50 here
      parameter (here= 's_prt_dec_scin')
*     
      logical ABORT
      character*(*) err
*     
      integer*4 j
      include 'sos_data_structures.cmn'
      include 'gen_constants.par'
      include 'gen_units.par'
      include 'sos_tracking.cmn'
*     
*--------------------------------------------------------
      ABORT = .FALSE.
      err = ' '
      
      write(sluno,'(''        ***SOS_REAL_SCIN BANKS***'')')
      write(sluno,'(''     SSCIN_TOT_HITS='',I4)') SSCIN_TOT_HITS
      if(SSCIN_TOT_HITS.GT.0) then
        write(sluno,'('' Num  Plane    Counter        ADC_POS'',
     &       ''ADC_NEG  TDC_POS  TDC_NEG'')')
        write(sluno,'(1x,i2,2x,i3,5x,i4,8x,2f8.2,2i8)')
     &       (j,SSCIN_PLANE_NUM(j),SSCIN_COUNTER_NUM(j),
     &       SSCIN_ADC_POS(j),SSCIN_ADC_NEG(j),
     &       SSCIN_TDC_POS(j),SSCIN_TDC_NEG(j),
     &       j=1,SSCIN_TOT_HITS )
      endif
      
      write(sluno,'(''        SOS_DECODED_SCIN BANKS'')')
      if(SSCIN_TOT_HITS.GT.0) then
         write(sluno,'('' Scintillator hits per plane'')')
         write(sluno,'('' Plane  '',10i4)') (j,j=1,SNUM_SCIN_PLANES)   
         write(sluno,'('' Number '',10i4)') 
     &        (SSCIN_HITS_PER_PLANE(j),j=1,SNUM_SCIN_PLANES)
         write(sluno,'('' Num     ZPOS    CENTER  HIT_COORD  SLOP'',
     &        ''   COR_TDC  TWO_GOOD'')')
         write(sluno,'(1x,i2,2x,4f9.3,f10.3,4x,l2)')
     &        (j,SSCIN_ZPOS(j),SSCIN_CENTER_COORD(j),
     &        SSCIN_DEC_HIT_COORD(j),
     &        SSCIN_SLOP(j),SSCIN_COR_TIME(j),
     &        STWO_GOOD_TIMES(j), 
     &        j=1,SSCIN_TOT_HITS)    
         write(sluno,'('' SGOOD_START_TIME='', l2)')
     &        SGOOD_START_TIME
         write(sluno,'('' SSTART_TIME='',e10.4)') SSTART_TIME 
         write(sluno,*)
      endif
      RETURN
      END
