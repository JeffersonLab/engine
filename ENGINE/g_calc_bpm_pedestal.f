      subroutine g_calc_bpm_pedestal(ABORT,err)
*
* $Log: g_calc_bpm_pedestal.f,v $
* Revision 1.2  1996/04/29 19:43:42  saw
* (JRA) Update bpm calculations
*
* Revision 1.1  1996/01/22 15:12:05  saw
* Initial revision
*
      implicit none
      save
*
      character*18 here
      parameter (here='g_calc_bpm_pedestal')
*
      logical ABORT
      character*(*) err
*
      integer*4 ibpm,isig
*
      INCLUDE 'gen_data_structures.cmn'
*
*
* extract bpm pedestal information from gmisc variables.
*
      do ibpm=1,gmax_num_bpms          !need some kind of 'map' for this.
        do isig=1,gnum_bpm_signals
          gbpm_adc_ped(isig,ibpm) = gmisc_ped(4*(ibpm-1)+isig,2) !2 is for ADC
        enddo
      enddo

      return
      end
