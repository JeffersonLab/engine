      subroutine g_analyze_misc(abort,errmsg)
*-------------------------------------------------------------------
* author: John Arrington
* created: 1/17/96
*
* g_analyze_misc takes the gen_decoded_misc common block and
*   generates decoded bpm/raster information.
*
* $Log$
* Revision 1.2  1996/04/29 19:41:09  saw
* (JRA) Update BPM code
*
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

      do ibpm=1,gmax_num_bpms       !need some kind of 'map' for this.
        do isig=1,gnum_bpm_signals
          gbpm_raw_adc(isig,ibpm) = gmisc_dec_data(4*(ibpm-1)+isig,2)
          gbpm_adc(isig,ibpm)=gbpm_raw_adc(isig,ibpm)-gbpm_adc_ped(isig,ibpm)
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
