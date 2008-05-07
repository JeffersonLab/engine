      subroutine sane_analyze_pedestal(ABORT,err)
      implicit none
      save

      logical ABORT
      character*(*) err
      character*18 here
      parameter (here='sane_analyze_pedestal')
*
      call lucite_sane_analyze_pedestal(ABORT,err) ! bigcal
      if(ABORT) then
         call G_add_path(here,err)
         return
      endif
*
      call cerenkov_sane_analyze_pedestal(ABORT,err) ! bigcal
      if(ABORT) then
         call G_add_path(here,err)
         return
      endif
*

      end

*********************************************************************
*********************************************************************

      subroutine lucite_sane_analyze_pedestal(ABORT,err)
      implicit none
      save

      logical ABORT
      character*(*) err
*
      integer*4 ihit
      integer*4 blk
      
      INCLUDE 'sane_data_structures.cmn'


*
*
* LUCITE CERENKOV PEDESTALS
*
*

      do ihit = 1 , lucite_sane_raw_tot_hits
        blk = lucite_sane_raw_counter_num(ihit)
        luc_ped_limit_pos(blk)=1500
        luc_ped_limit_neg(blk)=1500
        CALL Analyze_PED(
     &       lucite_sane_raw_adc_neg(ihit),
     &       luc_ped_num_neg(blk),
     &       luc_ped_sum2_neg(blk),
     &       luc_ped_sum_neg(blk),
     &       luc_min_peds,
     &       luc_ped_limit_neg(blk)
     &       )
c        write(*,*)"RAW ADC",luc_raw_adc_neg(ihit)
c        write(*,*)"COUNTER",blk
c        write(*,*)"SUM",luc_ped_sum_neg(blk)

        CALL Analyze_PED(
     &       lucite_sane_raw_adc_pos(ihit),
     &       luc_ped_num_pos(blk),
     &       luc_ped_sum2_pos(blk),
     &       luc_ped_sum_pos(blk),
     &       luc_min_peds,
     &       luc_ped_limit_pos(blk)
     &       )
      enddo
c      write(*,*)'LUC SUM',luc_ped_sum_pos
      end

*********************************************************************
*********************************************************************
      
      subroutine cerenkov_sane_analyze_pedestal(ABORT,err)
      implicit none
      save

      logical ABORT
      character*(*) err
*
      integer*4 ihit
      integer*4 blk
      
      INCLUDE 'sane_data_structures.cmn'


*
*
* cer_saneITE CERENKOV PEDESTALS
*
      do ihit = 1 , cerenkov_sane_raw_tot_hits
        blk = cerenkov_sane_raw_counter_num(ihit)
        cer_sane_ped_limit(blk)=10000
        CALL Analyze_PED(
     &       cerenkov_sane_raw_adc(ihit),
     &       cer_sane_ped_num(blk),
     &       cer_sane_ped_sum2(blk),
     &       cer_sane_ped_sum(blk),
     &       cer_sane_min_peds,
     &       cer_sane_ped_limit(blk)
     &       )
      enddo
      end

*********************************************************************
*********************************************************************


      Subroutine Analyze_PED(Raw,Ped,Sum2,Sum,PedMin,PedLimit)
      implicit none
      integer *4 RAW,PED,SUM2,SUM,PedMin,PedLimit
      
        if (Raw .le. PedLimit) then

          SUM2 = SUM2 + RAW*RAW
          SUM  = SUM  + RAW

          Ped  = Ped  + 1

          if (Ped.eq.nint(PedMin/5.)) then
            PedLimit = 100 + SUM/Ped
          endif

        endif


      end
