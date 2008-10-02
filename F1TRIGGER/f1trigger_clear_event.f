      subroutine F1TRIGGER_CLEAR_EVENT(ABORT,err)
      IMPLICIT NONE
      SAVE
      include 'f1trigger_data_structures.cmn'
    
      character*13 here
      parameter (here= 'f1trigger_clear_event')
    
      logical ABORT
      character*(*) err
      integer*4 i

      TRIGGER_F1_RAW_TOT_HITS = 0
      
      do i=1,TRIGGER_F1_MAX_HITS
         TRIGGER_F1_RAW_PLANE(i) = 0
         TRIGGER_F1_RAW_COUNTER(i) = 0 
         TRIGGER_F1_START_TDC(i) = 0 
         TRIGGER_F1_START_TDC_COUNTER(i) = 0
      enddo
      
      ABORT= .FALSE.
      err= ' '
      RETURN
      end

