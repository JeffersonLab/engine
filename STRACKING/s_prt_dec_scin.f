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
* $Log$
* Revision 1.2  1994/05/13 03:22:48  cdaq
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
      include 'gen_data_structures.cmn'
      include 'gen_constants.par'
      include 'gen_units.par'
      include 'sos_tracking.cmn'
*     
*--------------------------------------------------------
      ABORT = .FALSE.
      err = ' '
      write(sluno,'(''        SOS_DECODED_SCIN BANKS'')')
      if(SSCIN_TOT_HITS.GT.0) then
         write(sluno,'('' Scintillator hits per plane'')')
         write(sluno,'('' Plane  '',10i4)') (j,j=1,SNUM_SCIN_PLANES)   
         write(sluno,'('' Number '',10i4)') 
     &        (SSCIN_HITS_PER_PLANE(j),j=1,SNUM_SCIN_PLANES)
         write(sluno,'('' Num    ZPOS   CENTER  HIT_COORD WIDTH  SLOPE''
     &        ''   COR_ADC  COR_TDC  TWO_GOOD'')')
         write(sluno,'(1x,i2,2x,5f8.3,2f10.3,4x,l2)')
     &        (j,SSCIN_ZPOS(j),SSCIN_CENTER_COORD(j),
     &        SSCIN_DEC_HIT_COORD(j),SSCIN_WIDTH(j),
     &        SSCIN_SLOP(j),SSCIN_COR_ADC(j),SSCIN_COR_TIME(j),
     &        STWO_GOOD_TIMES(j), 
     &        j=1,SSCIN_TOT_HITS)    
         write(sluno,'('' SGOOD_START_TIME='', l2,
     &        ''  SGOOD_START_PLANE'',l2)')
     &        SGOOD_START_TIME,  SGOOD_START_PLANE
         write(sluno,'('' SSTART_TIME='',e10.4, 
     &        '' SSTART_HITNUM='',i3,''   SSTART_HITSIDE='',i3)')
     &        SSTART_TIME, SSTART_HITNUM, SSTART_HITSIDE
      endif
      RETURN
      END
