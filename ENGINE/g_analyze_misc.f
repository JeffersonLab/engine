      subroutine g_analyze_misc(abort,errmsg)
*-------------------------------------------------------------------
* author: John Arrington
* created: 1/17/96
*
* g_analyze_misc takes the gen_decoded_misc common block and
*   generates decoded bpm/raster information.
*
* $Log$
* Revision 1.1  1996/01/22 15:08:37  saw
* Initial revision
*
*--------------------------------------------------------

      implicit none

      include 'gen_data_structures.cmn'

      logical abort
      character*1024 errmsg
      character*20 here
      parameter (here = 'g_analyze_misc')

      integer*4 ibpm,isig

      save

      abort = .false.
      errmsg = ' '

* BPM Signals.

      do isig=1,gnum_bpm_signals   !need some kind of 'map' for this.
        gbpm_raw_adc(1,isig) = gmisc_dec_data(isig,2)
        gbpm_raw_adc(2,isig) = gmisc_dec_data(isig+4,2)
        gbpm_raw_adc(3,isig) = gmisc_dec_data(isig+8,2)
        gbpm_raw_adc(4,isig) = gmisc_dec_data(isig+12,2)
      enddo

      do ibpm=1,gmax_num_bpms
        do isig=1,gnum_bpm_signals
          gbpm_adc(ibpm,isig)=gbpm_raw_adc(ibpm,isig)-gbpm_adc_ped(ibpm,isig)
        enddo
      enddo


* Raster Signals.

      gfrx_raw_adc = gmisc_dec_data(14,2)
      gfry_raw_adc = gmisc_dec_data(16,2)
      gfrx_adc = gfrx_raw_adc - gfrx_adc_ped
      gfry_adc = gfry_raw_adc - gfry_adc_ped
      gfrx_sync = gmisc_dec_data(13,2) - gfrx_sync_mean  !sign gives sync phase.
      gfry_sync = gmisc_dec_data(15,2) - gfry_sync_mean

      return
      end
