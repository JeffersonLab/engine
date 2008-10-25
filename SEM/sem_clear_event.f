      subroutine SEM_CLEAR_EVENT(ABORT,err)
      IMPLICIT NONE
      SAVE
      INCLUDE 'sem_data_structures.cmn'
     
      character*13 here
      parameter (here= 'sem_clear_event')
     
      logical ABORT
      character*(*) err
      integer i
*  SEM hits

      N_TBPM_TOT_HITS = 0
      do i=1,N_TBPM_ALL_CHAN
        n_tbpm_addr1(i) = 0
        n_tbpm_addr2(i) = 0
        N_TBPM_RAW_DATA(i) = 0
      enddo


      end

**********************************************************
