      subroutine g_analyze_misc(abort,errmsg)
*-------------------------------------------------------------------
* author: John Arrington
* created: 1/17/96
*
* g_analyze_misc takes the gen_decoded_misc common block and
*   generates decoded bpm/raster information.
*
* $Log$
* Revision 1.7  1999/11/04 20:35:14  saw
* Linux/G77 compatibility fixes
*
* Revision 1.6  1999/06/10 14:38:25  csa
* (CSA) Commented out debugging output
*
* Revision 1.5  1999/02/23 16:55:43  csa
* Correct gbeam calc, add a bunch of comments
*
* Revision 1.4  1999/02/10 17:43:38  csa
* Updated code for SEE bpms (P. Gueye?), added raster calculations
* (J. Reinhold), and added code for third target bpm
*
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
      include 'gen_constants.par'

      logical abort
      character*(*) errmsg
      character*20 here
      parameter (here = 'g_analyze_misc')

      integer*4 ibpm,ibpm_sample,n_use_bpms
      real*8 normfrx,frxphase,frxdphase
      real*8 normfry,fryphase,frydphase

      real*4 xp(gmax_num_bpms),xm(gmax_num_bpms)
      real*4 yp(gmax_num_bpms),ym(gmax_num_bpms)
      real*4 bpm_x(gmax_num_bpms,gbpm_sample_max)
      real*4 bpm_y(gmax_num_bpms,gbpm_sample_max)
      real*4 bpm_meanx(gmax_num_bpms),bpm_meany(gmax_num_bpms)
      real*4 sum_x,sum_y,sum_z,sum_zx,sum_zy,sum_zz,denom
      integer*4 n_use_bpm

      real*4 small
      parameter (small = 1.e-6)

      save

      abort = .false.
      errmsg = ' '

*     csa 2/99 -- Note that the SEE BPMs have a large delay in their
*     electronics. For a 20 kHz raster, it amounts to about 90 degrees.
*     There is a phase ambiguity which arises from this delay, and as a
*     result, the target position derived directly from the BPM *cannot*
*     be used on an event-by-event basis to correct for position. The
*     fast raster signal should be used for this (alternatively, the
*     phase ambiguity can be resolved using both the fast raster and
*     fast raster synch signals). We *can* use the BPM signals to
*     calculate a running centroid average (phase ambiguity is not
*     harmful here).

*     In principle, the following needs to be done only once per run...

      n_use_bpm = 3
      if (guse_bpmc .ne. 1) n_use_bpm = 2

*     csa 2/2/99 -- Until we understand the bpms we will not use the
*     info in the analyzer. Once someone has done a reasonable analysis
*     that convinces us we understand what we are getting, these defeats
*     can be removed.

      if(guse_bpm_in_recon .ne. 0) then
*         write(6,*)' g_analyze_misc: forcing guse_bpm_in_recon to 0'
         guse_bpm_in_recon = 0
      endif

      if(gusefr .ne. 0) then
*         write(6,*)' g_analyze_misc: forcing gusefr to 0 (NO Fast Raster corrections)'
         gusefr = 0
      endif

      if(guse_frdefault .ne. 1) then
*         write(6,*)' g_analyze_misc: forcing guse_frdefault to 1'
         guse_frdefault = 1
      endif

*     initialize beam coordinates

      gbeam_x  = 0.
      gbeam_xp = 0.
      gbeam_y  = 0.
      gbeam_yp = 0.

*     BPM Signals:
*     ============

*     pedestals are set in replay/PARAM/gbeam.param.*

      xp(1) = gmisc_dec_data(5,2)  - gbpm_xp_ped(1)
      xm(1) = gmisc_dec_data(6,2)  - gbpm_xm_ped(1)
      yp(1) = gmisc_dec_data(7,2)  - gbpm_yp_ped(1)
      ym(1) = gmisc_dec_data(8,2)  - gbpm_ym_ped(1)

      xp(2) = gmisc_dec_data(9,2)  - gbpm_xp_ped(2)
      xm(2) = gmisc_dec_data(10,2) - gbpm_xm_ped(2)
      yp(2) = gmisc_dec_data(11,2) - gbpm_yp_ped(2)
      ym(2) = gmisc_dec_data(12,2) - gbpm_ym_ped(2)

      xp(3) = gmisc_dec_data(1,2) - gbpm_xp_ped(3)
      xm(3) = gmisc_dec_data(2,2) - gbpm_xm_ped(3)
      yp(3) = gmisc_dec_data(3,2) - gbpm_yp_ped(3)
      ym(3) = gmisc_dec_data(4,2) - gbpm_ym_ped(3)

*     calibration constants are set in replay/PARAM/gbeam.param.* 

      do ibpm = 1,n_use_bpms
         gbpm_yprime(ibpm) =  gbpm_kappa*(xp(ibpm)-gbpm_alpha_x(ibpm)*xm(ibpm))/
     &        (xp(ibpm)+gbpm_alpha_x(ibpm)*xm(ibpm)+small)
         gbpm_xprime(ibpm) = -gbpm_kappa*(yp(ibpm)-gbpm_alpha_y(ibpm)*ym(ibpm))/
     &        (yp(ibpm)+gbpm_alpha_y(ibpm)*ym(ibpm)+small)
         gbpm_x(ibpm)      = ( gbpm_xprime(ibpm)+gbpm_yprime(ibpm))/sqrt(2.)
     $        +gbpm_x_off(ibpm)
         gbpm_y(ibpm)      = (-gbpm_xprime(ibpm)+gbpm_yprime(ibpm))/sqrt(2.)
     $        +gbpm_y_off(ibpm)
      enddo

*     calculate the mean over the last 'gbpm_sample' events 

      ibpm_sample = ibpm_sample+1
      if(ibpm_sample.eq.gbpm_sample+1) ibpm_sample=1
      do ibpm=1,n_use_bpms
         bpm_meanx(ibpm)         = bpm_meanx(ibpm) - 
     >        bpm_x(ibpm,ibpm_sample) + gbpm_x(ibpm)
         gbpm_meanx(ibpm)        = bpm_meanx(ibpm)/gbpm_sample
         bpm_x(ibpm,ibpm_sample) = gbpm_x(ibpm)
         bpm_meany(ibpm)         = bpm_meany(ibpm) - 
     >        bpm_y(ibpm,ibpm_sample) + gbpm_y(ibpm)
         gbpm_meany(ibpm)        = bpm_meany(ibpm)/gbpm_sample
         bpm_y(ibpm,ibpm_sample) = gbpm_y(ibpm)
      enddo   

*     csa 2/2/99 -- For the time being I am assuming that all three BPMs
*     are operational (with the exception that you can ignore H003 by
*     setting guse_bpmc to zero). Eventually it would be nice to make
*     the code automatically handle the case that one of the devices is
*     broken. I'm not sure how to robustly identify a failed bpm,
*     though.

*     We'll assume that the sigmas for the three BPMs are the 
*     same (i.e., we give them equal weight), which simplifies 
*     the math of the linear fit quite a bit. 

*     Note that only mean bpm information is used here (not event-
*     to-event).

      sum_x  = 0.
      sum_y  = 0.
      sum_z  = 0.
      sum_zx = 0.
      sum_zy = 0.
      sum_zz = 0.
      do ibpm=1,n_use_bpms
         sum_x  = sum_x + gbpm_meanx(ibpm)
         sum_y  = sum_y + gbpm_meany(ibpm)
         sum_z  = sum_z + gbpm_zpos(ibpm)
         sum_zx = sum_zx + gbpm_zpos(ibpm)*gbpm_meanx(ibpm)
         sum_zy = sum_zx + gbpm_zpos(ibpm)*gbpm_meany(ibpm)
         sum_zz = sum_zz + gbpm_zpos(ibpm)*gbpm_zpos(ibpm)
      enddo

      denom = sum_zz - sum_z*sum_z
      if (abs(denom) .lt. small) denom = small
      gbpm_beam_xp = (sum_zx - sum_x*sum_z)/denom
      gbpm_beam_x  = (sum_zz*sum_x - sum_zx*sum_z)/denom

      gbpm_beam_yp = (sum_zy - sum_y*sum_z)/denom
      gbpm_beam_y  = (sum_zz*sum_y - sum_zy*sum_z)/denom

*      write(6,*)' g_anal_misc: sum_x/y/z     =',sum_x,sum_y,sum_z
*      write(6,*)' g_anal_misc: sum_zx/zy/zz  =',sum_zx,sum_zy,sum_zz
*      write(6,*)' g_anal_misc: gbpm_beam_x/y =',gbpm_beam_x,gbpm_beam_y

      if(guse_bpm_in_recon.ne.0)then
         gbeam_x  = gbpm_beam_x
         gbeam_xp = gbpm_beam_xp
         gbeam_y  = gbpm_beam_y
         gbeam_yp = gbpm_beam_yp
      else
         gbeam_x  = gbeam_xoff
         gbeam_xp = gbeam_xpoff
         gbeam_y  = gbeam_yoff
         gbeam_yp = gbeam_ypoff
      endif
      gbeam_x  = gbeam_x  - gspec_xoff
      gbeam_xp = gbeam_xp - gspec_xpoff
      gbeam_y  = gbeam_y  - gspec_yoff
      gbeam_yp = gbeam_yp - gspec_ypoff

*     Fast Raster Signals:
*     ===================

      gfrx_raw_adc = gmisc_dec_data(14,2)
      gfry_raw_adc = gmisc_dec_data(16,2)

* gfrx_adc_mean and gfry_adc_mean calculation in g_calc_raster_pedestal.f does
* not seem to work so I am hardwiring them here. One option is to enter them as
* parameters in gbeam.param. djm 12/3/97 based on fpi run 17258

      gfrx_adc = gfrx_raw_adc - 3900.
      gfry_adc = gfry_raw_adc - 3725.
cdjm      gfrx_adc = gfrx_raw_adc - gfrx_adc_mean
cdjm      gfry_adc = gfry_raw_adc - gfry_adc_mean
      gfrx_sync = gmisc_dec_data(13,2) - gfrx_sync_mean !sign gives sync phase
      gfry_sync = gmisc_dec_data(15,2) - gfry_sync_mean

*     fast raster deflection on target is calculated in cm

      if (guse_frdefault .ne. 0) then ! no phase correction
         gfrx = (gfrx_adc/gfrx_adcpercm)*(gfr_cal_mom/gpbeam)
         gfry = (gfry_adc/gfry_adcpercm)*(gfr_cal_mom/gpbeam)
      else                      ! apply phase correction
         normfrx   = max(-1.0,min(1.0,(gfrx_adc/gfrx_adcmax)))
         frxphase  = asin(normfrx)
         frxdphase = sign(1.,gfrx_sync-gfrx_synccut)*gfrx_dphase*degree
         frxphase  = frxphase + frxdphase
         gfrx      = sin(frxphase)*gfrx_maxsize

         normfry   = max(-1.0,min(1.0,(gfry_adc/gfry_adcmax)))
         fryphase  = asin(normfry)
         frydphase = sign(1.,gfry_sync-gfry_synccut)*gfry_dphase*degree
         fryphase  = fryphase + frydphase
         gfry      = sin(fryphase)*gfry_maxsize
      endif

      gfrxp     = gfrx/gfrx_dist
      gfryp     = gfry/gfry_dist

      if (gusefr .ne. 0) then   ! correct for raster
         gbeam_x  = gbeam_x  + gfrx
         gbeam_xp = gbeam_xp + gfrxp
         gbeam_y  = gbeam_y  + gfry
         gbeam_yp = gbeam_yp + gfryp
      endif

      return
      end
