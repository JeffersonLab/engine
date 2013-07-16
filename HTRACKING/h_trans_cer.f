      subroutine h_trans_cer(abort,errmsg)

*-------------------------------------------------------------------
* author: Chris Cothran
* created: 5/25/95
*
* h_trans_cer fills the hms_decoded_cer common block
* with track independent corrections and parameters
* $Log: h_trans_cer.f,v $
* Revision 1.2  1996/01/16 21:38:47  cdaq
* (JRA) Make hcer_adc pedestal subtracted value
*
* Revision 1.1  1995/08/30 15:30:15  cdaq
* Initial revision
*
*-------------------------------------------------------------------

      implicit none

      include 'hms_data_structures.cmn'
      include 'hms_cer_parms.cmn'

      logical abort
      character*1024 errmsg
      character*20 here
      parameter (here = 'h_trans_cer')

      integer*4 nhit,tube

      save
      
      abort = .false.

      hcer_num_hits = 0
      do tube=1,hcer_num_mirrors
        hcer_npe(tube) = 0.
        hcer_adc(tube) = 0.
      enddo
      hcer_npe_sum = 0.
      do nhit = 1, hcer_tot_hits
        tube = hcer_tube_num(nhit)
        hcer_adc(tube) = hcer_raw_adc(nhit) - hcer_ped(tube)
        if (hcer_adc(tube) .gt. hcer_width(tube)) then
          hcer_num_hits = hcer_num_hits + 1
          hcer_tube_num(hcer_num_hits) = tube
          hcer_npe(tube) = hcer_adc(tube) * hcer_adc_to_npe(tube)
          hcer_npe_sum = hcer_npe_sum + hcer_npe(tube)
        endif
      enddo

      return
      end
