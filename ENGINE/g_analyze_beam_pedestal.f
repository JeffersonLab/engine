      subroutine g_analyze_beam_pedestal(ABORT,err)
*
* $Log$
* Revision 1.1  1996/01/22 15:08:20  saw
* Initial revision
*
      implicit none
      save
*
      character*23 here
      parameter (here='g_analyze_beam_pedestal')
*
      logical ABORT
      character*(*) err
*
      integer*4 ihit
      integer*4 ind
*
      INCLUDE 'gen_data_structures.cmn'
*
*
* MISC PEDESTALS
*
      do ihit = 1 , gmisc_tot_hits
        if (gmisc_raw_addr1(ihit).eq.2) then   !ADCs
          ind=gmisc_raw_addr2(ihit)      ! no sparsification yet - NEED TO FIX!!!!
          gmisc_ped_sum2(ind,2) = gmisc_ped_sum2(ind,2) +
     $         gmisc_raw_data(ihit)*gmisc_raw_data(ihit)    !2 is for ADCs
          gmisc_ped_sum(ind,2) = gmisc_ped_sum(ind,2) + gmisc_raw_data(ihit)
          gmisc_ped_num(ind,2) = gmisc_ped_num(ind,2) + 1
        endif
      enddo

      return
      end
