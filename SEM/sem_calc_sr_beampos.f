      SUBROUTINE sem_calc_sr_beampos
*--------------------------------------------------------
*-
*-   Purpose and Methods :  determines position of raster beam
*-                          at the target based on selected data.
*-
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE

      character*17 here
      parameter (here= 'sem_calc_sr_beampos')

      include 'gen_data_structures.cmn'
c      INCLUDE 'gen_constants.par'
c      include 'hms_bypass_switches.cmn'
      include 'sem_data_structures.cmn'
      include 'gen_constants.par'
      include 'gen_event_info.cmn'
c      include 'gen_scalers.cmn'
c      include 'n_id_histid.cmn'

      real*4 x_coord, y_coord


* some comments before the code:
* the SEM coordinate system is different from the HALL C coordinate
* system that is used throughout the analysis code.
*
* 1) the positive SEM x coordinate points to the right when looking
*    in beam direction, this corresponds to negative y direction of
*    the HALL C transport coordinate system.
*    the ADC positive x coordinate points DOWN.
*
* 2) the positive SEM y coordinate points vertical up when looking
*    down stream this corresponds to negative x of the HALL C coordinate
*    system.
*    the ADC positive y coordinate points RIGHT, but historically
*    it has been assumed that it points left, thus all signs are
*    inverted in the code -- but that's OK, because the ADC calibration
*    automatically fixes that
*
* 3) the raster adc values are calibrated using the sem by fitting
*    the profile spectrum of SEM vs RASTER_ADC. So converting
*    from raster adc into mm gives results in SEM coordinate system
*    and to convert back to HALL C system one has to change signs of
*    the parameters!
*
* 4) we now have a new SEM mode,  slow_raster_correction=4
*    the MEAN beam position comes from the SEM, via scalers, and the
*    event deviation is obtained from the raster ADC
*    we use the same ADC calibration constants for this mode
*    we use the same ADC calibration constants for this mode
*    but they have somewhat different meaning...
*
* 5) calibration of slow raster with SEM
*     * the ntbpmx and ntbpmy unit is millimeter
*     * the shared variables gSR_beamx,gSR_beamy use centimeters!

*=============================Executable Code =============================

* gsrx_adc is in ADC system
* ntbpmx is in SEM system
* x_coord is in SEM system
* gsrx_calib is in SEM system
* gSR_beamx is in TRANSPORT system


      if ((slow_raster_correction.eq.0)
     >    .or. (slow_raster_correction.eq.-1)
     >    .or. (slow_raster_correction.eq.1)) then     !use SEM data
        x_coord = ntbpmx
        y_coord = ntbpmy
        gsrx_calib = gsry_raw_adc  ! uncalibrated so we can
        gsry_calib = gsrx_raw_adc  !  determine calibration
*
*
* mkj 2/20/2004 added parameters n_sr_adcx_zero,n_sr_adcy_zero
*   so that now n_sr_offsety,n_sr_offsetx are offsets in beam
*   position in which +x is horizontal beam right,+y is vertical up.
*
      else if (slow_raster_correction.eq.2) then       ! use raster data
c         write(*,*)n_sr_size,n_sr_slopex,n_sr_adcx_zero
         gsry_calib = n_sr_size/n_sr_slopey*(gsry_raw_adc-n_sr_adcy_zero)!-
c     ,     n_fr_size/n_fr_slopex*(gfrx_raw_adc-n_fr_adcx_zero)   
         gsrx_calib = n_sr_size/n_sr_slopex*(gsrx_raw_adc-n_sr_adcx_zero)!+
         gsry_calib = gsry_calib + n_sr_offsety
         gsrx_calib = gsrx_calib + n_sr_offsetx
c     ,        n_fr_size/n_fr_slopey*(gfry_raw_adc-n_fr_adcy_zero)
c         write(*,*)n_fr_slopey,n_fr_slopex
         x_coord = gsrx_calib
         y_coord = gsry_calib
      if (gen_event_type .ne. 4 ) then
      call HFILL(10210,gsry_raw_adc,gsrx_raw_adc, 1.)
      call HFILL(10211,gfry_raw_adc,gfrx_raw_adc, 1.)

      call HFILL(10212,gsrx_calib,gsry_calib, 1.)
      call HFILL(10213,gbeam_y,gbeam_x, 1.)
      endif
c        write(*,*)gsrx_calib,gsry_calib
c      else if (slow_raster_correction.eq.4) then       ! use mixed mode
c        gsrx_calib = n_sr_slopex*(gsry_adc-n_sr_adcx_zero) + n_sr_offsetx
c        gsry_calib = n_sr_slopey*(gsrx_adc-n_sr_adcy_zero) + n_sr_offsety
c        x_coord = gsrx_calib - gsem_meanxpos
c        y_coord = gsry_calib - gsem_meanypos

      else                                         ! shouldn't happen!
        x_coord = 0.
        y_coord = 0.

      endif
*     * correct units and switch to TRANSPORT system
      gSR_beamx = x_coord 
      gSR_beamy = y_coord 
c      write(*,*)gSR_beamx,gSR_beamy

c*     * in any case plot event SEM vs calib ADC
c* mkj 1/23/02 change to plot tbpm without negative sign
c      call hf2(nidSEM_ADCx, n_sr_slopex*gsrx_adc+n_sr_offsetx, ntbpmy, 1.)
c      call hf2(nidSEM_ADCy, n_sr_slopey*gsry_adc+n_sr_offsety, ntbpmx, 1.)
c
cc mkj 1/23/02 switch y to horz axis
c      call hf2(nidndetsrADC, n_sr_slopey*gsry_adc+n_sr_offsety,
c     >                       n_sr_slopex*gsrx_adc+n_sr_offsetx, 1.)


      return
      end
