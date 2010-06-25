      subroutine SANE_CLEAR_EVENT(ABORT,err)
      IMPLICIT NONE
      SAVE
     
      character*13 here
      parameter (here= 'sane_clear_event')
     
      logical ABORT
      character*(*) err

      call lucite_sane_clear_event(ABORT,err)
      call cerenkov_sane_clear_event(ABORT,err)
      call tracker_sane_clear_event(ABORT,err)
      call SANE_DUMP_NTUP_VAR()

      end

**********************************************************
**********************************************************

      SUBROUTINE lucite_sane_clear_event(ABORT,err)
      IMPLICIT NONE
      SAVE
     
      INCLUDE 'sane_data_structures.cmn'
      INCLUDE 'sane_ntuple.cmn'
      character*13 here
      parameter (here= 'lucite_sane_clear_event')
     
      logical ABORT
      character*(*) err     
      integer*4 i

      LUCITE_SANE_RAW_TOT_HITS          = 0
      LUCITE_SANE_RAW_TOT_HITS2          = 0
      LUCITE_SANE_RAW_TOT_HITS3          = 0
c      LUCITE_SANE_RAW_PLANE             = 0

      do i=1,LUCITE_SANE_MAX_HITS

         LUCITE_SANE_RAW_COUNTER_NUM(i) = 0
         LUCITE_SANE_RAW_COUNTER_NUM2(i) = 0
         LUCITE_SANE_RAW_COUNTER_NUM3(i) = 0
         LUCITE_SANE_RAW_ADC_POS(i)     = 0
         LUCITE_SANE_RAW_ADC_NEG(i)     = 0
         LUCITE_SANE_RAW_TDC_POS(i)     = 0
         LUCITE_SANE_RAW_TDC_NEG(i)     = 0
         
      enddo
      do i=1,luc_hit
         ltdc_pos(i) = 0
         ltdc_neg(i) = 0
         ladc_pos(i) = 0
         ladc_neg(i) = 0
      enddo

       luc_hit = 0

      ABORT= .FALSE.
      err= ' '
      RETURN
      END

**********************************************************
**********************************************************

      SUBROUTINE cerenkov_sane_clear_event(ABORT,err)
      IMPLICIT NONE
      SAVE
    
      INCLUDE 'sane_data_structures.cmn'
      INCLUDE 'sane_ntuple.cmn'
      character*13 here
      parameter (here= 'cerenkov_sane_clear_event')
     
      logical ABORT
      character*(*) err     
      integer*4 i

      CERENKOV_SANE_RAW_TOT_HITS          = 0
      CERENKOV_SANE_RAW_TOT_HITS2          = 0
c      CERENKOV_SANE_RAW_PLANE             = 0

      do i=1,CERENKOV_SANE_MAX_HITS
         CERENKOV_SANE_RAW_COUNTER_NUM(i) = 0
         CERENKOV_SANE_RAW_COUNTER_NUM2(i) = 0
         CERENKOV_SANE_RAW_ADC(i)         = 0
         CERENKOV_SANE_RAW_TDC(i)         = 0
         if(i.le.cer_hit)then
            cer_tdc(i) = 0
            cer_adcc(i) = 0
         endif
         
      enddo
      cer_hit = 0


      ABORT= .FALSE.
      err= ' '
      RETURN
      END

**********************************************************
**********************************************************

      SUBROUTINE tracker_sane_clear_event(ABORT,err)
      IMPLICIT NONE
      SAVE

      character*50 here
      parameter (here= 'tracker_sane_clear_event')

      logical ABORT
      character*(*) err
      
      INCLUDE 'sane_data_structures.cmn'
      INCLUDE 'sane_ntuple.cmn'
      integer*4 i

      TRACKER_SANE_RAW_TOT_HITS_Y          = 0
      TRACKER_SANE_RAW_TOT_HITS_X          = 0
c      TRACKER_SANE_RAW_PLANE_Y             = 0
c      TRACKER_SANE_RAW_PLANE_X             = 0

      do i=1,TRACKER_SANE_MAX_HITS
         TRACKER_SANE_RAW_COUNTER_Y(i)     = 0
         TRACKER_SANE_RAW_COUNTER_X(i)     = 0
         TRACKER_SANE_RAW_TDC_Y(i)         = 0
         TRACKER_SANE_RAW_TDC_X(i)         = 0
         x1t_tdc(i) = 0
         x1t_row(i) = -200
         y1t_tdc(i) = 0
         y1t_row(i) = -200
         y2t_tdc(i) = 0
         y2t_row(i) = -200
      enddo
      x1t_hit = 0
      y1t_hit = 0
      y2t_hit = 0

      ABORT= .FALSE.
      err= ' '
      RETURN

      end

**********************************************************
**********************************************************
