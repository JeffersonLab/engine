      subroutine s_trans_cer(abort,errmsg)

*-------------------------------------------------------------------
* author: Chris Cothran
* created: 5/25/95
*
* s_trans_cer fills the sos_decoded_cer common block
* with track independent corrections and parameters
* $Log: s_trans_cer.f,v $
* Revision 1.2  1996/01/17 18:45:10  cdaq
* (JRA) Make scer_adc pedestal subtracted value
*
* Revision 1.1  1995/08/31 15:04:22  cdaq
* Initial revision
*
*-------------------------------------------------------------------

      implicit none

      include 'sos_data_structures.cmn'
      include 'sos_cer_parms.cmn'

      logical abort
      character*1024 errmsg
      character*20 here
      parameter (here = 's_trans_cer')

      integer*4 nhit,tube

      save
      
      abort = .false.

      scer_num_hits = 0
      do tube=1,scer_num_mirrors
        scer_npe(tube) = 0.
        scer_adc(tube) = 0.
      enddo
      scer_npe_sum = 0.
      do nhit = 1, scer_tot_hits
        tube = scer_tube_num(nhit)
        scer_adc(tube) = scer_raw_adc(nhit) - scer_ped(tube)
        if (scer_adc(tube) .gt. scer_width(tube)) then
          scer_num_hits = scer_num_hits + 1
          scer_tube_num(scer_num_hits) = tube
          scer_npe(tube) = scer_adc(tube) * scer_adc_to_npe(tube)
*          write(*,*)tube,scer_adc(tube),scer_adc_to_npe(tube),scer_npe_sum
          scer_npe_sum = scer_npe_sum + scer_npe(tube)
        endif
      enddo

      return
      end
