      subroutine g_analyze_misc(abort,errmsg)
*-------------------------------------------------------------------
* author: John Arrington
* created: 1/17/96
*
* g_analyze_misc takes the gen_decoded_misc common block and
*   generates decoded bpm/raster information.
*
* $Log$
* Revision 1.3  1996/09/04 14:30:41  saw
* (JRA) Add beam position calculations
*
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
      real*8 normfry,fryphase,frydphase

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

* Beam position on target: this block calculates the beam Y position (beam
*                          line coordinate system) on the target in mm: gbeam_y
*
*         --- use Chen Yan's formula to calculate the Y amplitude ---
*             Vamp = 0.278*Ebeam*Xamp, where:
*               Vamp is the amplitude of the monitor signal in Volts
*               Ebeam is the beam energy in GeV  (gpbeam)
*               Xamp is the raster amplitude on target in mm
*             need two calibration constants to be defined in gconstants.param
*               gfry_defcalib := .278 or whatever it will be in the future
*               gfry_vperch := 1/(ADC channels per Volt input)
*
      if (gusefr .eq. 0) then      !do not correct for raster
        gbeam_y = gbeam_yoff
      else                         !correct for raster

        if (guse_frdefault .ne. 0) then   ! use nominal calibration
          gbeam_y = gfry_adc*gfry_vperch/(gfry_defcalib*gpbeam)
        else                             ! use user calibration

          if (guse_frphase .eq. 0) then   ! do not correct for FR phase
            gbeam_y = (gfry_adc/gfry_adcmax)*gfry_maxsize
          else                           ! correct for FR phase
            normfry = gfry_adc/gfry_adcmax
            normfry = min(1.0D0,normfry)
            normfry = max(-1.0D0,normfry)
            fryphase = asin(normfry)
            if( gfry_sync .gt. gfry_synccut ) then
              frydphase = gfry_dphase*3.141/180.
            else
              frydphase = -gfry_dphase*3.141/180.
            endif
            fryphase = fryphase + frydphase
            gbeam_y = sin(fryphase)*gfry_maxsize
          endif
        endif
        gbeam_y = gbeam_y + gbeam_yoff
      endif

      return
      end
