      SUBROUTINE sane_calc_pedestal(ABORT,err)
      implicit none
      save

      logical ABORT
      character*(*) err
      character*18 here
      parameter (here='sane_calc_pedestal')

      call lucite_sane_calc_pedestal(ABORT,err)
      if(ABORT) then
         call G_add_path(here,err)
         return
      endif
      call cerenkov_sane_calc_pedestal(ABORT,err)
      if(ABORT) then
         call G_add_path(here,err)
         return
      endif

      end

****************************************************************
**************************************************************** 

      Subroutine lucite_sane_calc_pedestal(ABORT,err)
      implicit none
      save

      logical ABORT
      character*(*) err
*
      integer*4 pmt
      INCLUDE 'sane_data_structures.cmn'
      luc_min_peds=7
      do pmt = 1 , (LUCITE_SANE_MAX_COUNTER_NUM)
         call Calc_Ped(
     &        luc_ped_num_pos(pmt),
     &        luc_min_peds,
     &        luc_ped_mean_pos(pmt),
     &        luc_ped_rms_pos(pmt),
     &        luc_ped_sum_pos(pmt),
     &        luc_ped_sum2_pos(pmt))
c         write(*,*)'NUM ',pmt,luc_ped_num_pos(pmt)
c         write(*,*)'Sum ',pmt, luc_ped_sum_pos(pmt)

         call Calc_Ped(
     &        luc_ped_num_neg(pmt),
     &        luc_min_peds,
     &        luc_ped_mean_neg(pmt),
     &        luc_ped_rms_neg(pmt),
     &        luc_ped_sum_neg(pmt),
     &        luc_ped_sum2_neg(pmt))

          luc_ped_threshold_neg(pmt) = luc_ped_mean_neg(pmt)
          luc_ped_threshold_pos(pmt) = luc_ped_mean_pos(pmt)
        
      enddo
      end

****************************************************************
**************************************************************** 

      Subroutine cerenkov_sane_calc_pedestal(ABORT,err)
      implicit none
      save

      logical ABORT
      character*(*) err
*
      integer*4 pmt
      INCLUDE 'sane_data_structures.cmn'
      cer_sane_min_peds=7
      do pmt = 1 , (CERENKOV_SANE_MAX_CER_COUNTER)
         call Calc_Ped(
     &        cer_sane_ped_num(pmt),
     &        cer_sane_min_peds,
     &        cer_sane_ped_mean(pmt),
     &        cer_sane_ped_rms(pmt),
     &        cer_sane_ped_sum(pmt),
     &        cer_sane_ped_sum2(pmt))
c         write(*,*)'NUM ',pmt,luc_ped_num_pos(pmt)
c         write(*,*)'Sum ',pmt, luc_ped_sum_pos(pmt)

          cer_sane_ped_threshold(pmt) = cer_sane_ped_mean(pmt)
        
      enddo

      end

****************************************************************
**************************************************************** 

      Subroutine Calc_Ped(Ped,PedMin,Mean,RMS,Sum,Sum2)
      implicit none
      save
      integer*4 Ped, PedMin, Sum, Sum2
      real*4 Mean, RMS, sig2
        if (Ped .ge. PedMin .and. PedMin .ne. 0) then
           Mean = Sum /float(Ped)
           sig2 = float(SUM2)/float(Ped)-Mean**2
           RMS  = sqrt(max(0.,sig2))
        endif

      end


