      subroutine g_calc_bpm_pedestal(ABORT,err)
*
* $Log$
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
      integer*4 ind
*
      INCLUDE 'gen_data_structures.cmn'
*
*
* extract bpm pedestal information from gmisc variables.
*
      do ind=1,gnum_bpm_signals   !need some kind of 'map' for this.
        gbpm_adc_ped(1,ind) = gmisc_ped(ind,2)     !2 is for ADCs
        gbpm_adc_ped(2,ind) = gmisc_ped(ind+4,2)
        gbpm_adc_ped(3,ind) = gmisc_ped(ind+8,2)
        gbpm_adc_ped(4,ind) = gmisc_ped(ind+12,2)
      enddo

      return
      end
