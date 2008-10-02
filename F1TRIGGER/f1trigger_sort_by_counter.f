      subroutine f1trigger_sort_by_counter()
      IMPLICIT NONE
      include 'f1trigger_data_structures.cmn'
      integer*4 i
c
c
c     Subroutine sorts Start times by counter
c
c      
      do i = 1, TRIGGER_F1_RAW_TOT_HITS
         TRIGGER_F1_START_TDC_COUNTER(TRIGGER_F1_RAW_COUNTER(i)) = TRIGGER_F1_START_TDC(i)
      enddo
c this seems to be wrong. Took out PB Oct. 2, 2008
c      do i = TRIGGER_F1_RAW_TOT_HITS, TRIGGER_F1_MAX_HITS
c         TRIGGER_F1_START_TDC_COUNTER(TRIGGER_F1_RAW_COUNTER(i)) = 0
c      enddo
c      write(6,'(''dbg f1 B'',3i10)') 
c     >  TRIGGER_F1_RAW_TOT_HITS,
c     >    TRIGGER_F1_START_TDC(1),
c     >    TRIGGER_F1_START_TDC_COUNTER(1)


      end
