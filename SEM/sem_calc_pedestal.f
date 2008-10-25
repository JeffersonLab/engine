      SUBROUTINE sem_calc_pedestal(ABORT,err)
      implicit none
      save
      INCLUDE 'sem_data_structures.cmn'

      logical ABORT
      character*(*) err
      character*18 here
      parameter (here='sem_calc_pedestal')
      integer*4 pmt,i
      real*4 num
      num = ndet_tbpm_ped_counts
      print *, 'artificially set PEDESTALS='
      do i = 1,N_TBPM_ALL_CHAN
        ndet_ped_tbpm(i) = float(ndet_ped_tbpm_sum(i))/num
        ndet_ped_tbpm_sig(i) = sqrt(abs(float(ndet_ped_tbpm_sum2(i))/num 
     &       - (ndet_ped_tbpm(i))**2))
      enddo


      end


