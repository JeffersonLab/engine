      subroutine g_analyze_misc(abort,errmsg)
*-------------------------------------------------------------------
* author: John Arrington
* created: 1/17/96
*
* g_analyze_misc takes the gen_decoded_misc common block and
*   generates decoded bpm/raster information.
*
* $Log$
* Revision 1.9.20.4.2.8  2010/02/23 14:51:30  jones
* Remove "dbg slow raster=" write statements
*
* Revision 1.9.20.4.2.7  2009/06/05 17:47:35  jones
*  Changed gsrx_raw_adc = gmisc_dec_data(3,2) to   gmisc_dec_data(4,2)
*       gsry_raw_adc = gmisc_dec_data(4,2) to   gmisc_dec_data(3,2)
*
* Revision 1.9.20.4.2.6  2009/01/16 18:47:12  cdaq
* *** empty log message ***
*
* Revision 1.9.20.4.2.5  2008/11/05 15:41:54  cdaq
* Set variables gsrx_adc and gsry_adc
*
* Revision 1.9.20.4.2.4  2008/10/28 20:55:21  cdaq
* Changed raster channels
*
* Revision 1.9.20.4.2.3  2008/10/19 21:49:24  cdaq
* slow raster
*
* Revision 1.9.20.4.2.2  2008/10/11 15:03:34  cdaq
* slow raster
*
* Revision 1.9.20.4.2.1  2008/09/26 21:03:49  cdaq
* *** empty log message ***
*
* Revision 1.9.20.4  2007/10/20 19:55:06  cdaq
* Added more helicity analysis
*
* Revision 1.9.20.3  2007/10/17 19:30:14  cdaq
* changed cutoffs for h+ and h- signals: >8000 for ON, <2000 for OFF
*
* Revision 1.9.20.2  2007/10/17 16:12:38  cdaq
* Added handling of helicity ADC
*
* Revision 1.9.20.1  2007/10/17 15:52:54  cdaq
* Added helicity stuff
*
* Revision 1.9  2003/09/05 15:17:37  jones
* Merge in online03 changes (mkj)
*
* Revision 1.8.2.1  2003/08/14 00:23:36  cdaq
* Get bpm3 x and y position data from correct part of  gmisc_dec_data  array (mkj)
*
* Revision 1.8  2002/12/27 21:57:50  jones
*     a. delete variable n_use_bpm and only use variable n_use_bpms
*     b. Comment out forced setting of guse_bpm_in_recon,gusefr,guse_frdefault
*     c. only set xp(3),yp(3),xm(3),ym(3) when n_use_bpms .eq. 3
*     d. gbpm_kappa is an array
*     e. JRA added check of fasraster pedestals
*
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
      include 'gep_hist_id.cmn'

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

      integer*4 numfr
      real*4 sumfry,sumfrx,avefry,avefrx

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

      n_use_bpms = 3
      if (guse_bpmc .ne. 1) n_use_bpms = 2

*     csa 2/2/99 -- Until we understand the bpms we will not use the
*     info in the analyzer. Once someone has done a reasonable analysis
*     that convinces us we understand what we are getting, these defeats
*     can be removed.

* mkj 11/21/2001 Let the user have some intelligence to set these.
*mkj      if(guse_bpm_in_recon .ne. 0) then
*mkj         write(6,*)' g_analyze_misc: forcing guse_bpm_in_recon to 0'
*mkj         guse_bpm_in_recon = 0
*mkj      endif

*mkj      if(gusefr .ne. 0) then
*mkj         write(6,*)' g_analyze_misc: forcing gusefr to 0 (NO Fast Raster corrections)'
*mkj         gusefr = 0
*mkj      endif

*mkj      if(guse_frdefault .ne. 1) then
*mkj         write(6,*)' g_analyze_misc: forcing guse_frdefault to 1'
*mkj         guse_frdefault = 1
*mkj      endif

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

      if (n_use_bpms .eq. 3) then
      xp(3) = gmisc_dec_data(17,2) - gbpm_xp_ped(3)
      xm(3) = gmisc_dec_data(18,2) - gbpm_xm_ped(3)
      yp(3) = gmisc_dec_data(19,2) - gbpm_yp_ped(3)
      ym(3) = gmisc_dec_data(20,2) - gbpm_ym_ped(3)
      endif

* calibration constants are set in replay/PARAM/gbeam.param.* 

      do ibpm = 1,n_use_bpms
         gbpm_yprime(ibpm) =  gbpm_kappa(ibpm)*
     &        (xp(ibpm)-gbpm_alpha_x(ibpm)*xm(ibpm))/
     &        (xp(ibpm)+gbpm_alpha_x(ibpm)*xm(ibpm)+small)
         gbpm_xprime(ibpm) = -gbpm_kappa(ibpm)*
     &        (yp(ibpm)-gbpm_alpha_y(ibpm)*ym(ibpm))/
     &        (yp(ibpm)+gbpm_alpha_y(ibpm)*ym(ibpm)+small)
         gbpm_x(ibpm) = ( gbpm_xprime(ibpm)+gbpm_yprime(ibpm))/sqrt(2.)+
     &        gbpm_x_off(ibpm)
         gbpm_y(ibpm) = (-gbpm_xprime(ibpm)+gbpm_yprime(ibpm))/sqrt(2.)+
     &        gbpm_y_off(ibpm)
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

* JRA: Code to check FR pedestals.  Since the raster is a fixed frequency
* and the pedestals come at a fixed rate, it is possible to keep getting
* the same value for each pedestal event, and get the wrong zero value.
* (see HCLOG #28325).  So calculate pedestal from first 1000 REAL
* events and compare to value from pedestal events.  Error on each
* measurement is RMS/sqrt(1000), error on diff is *sqrt(2), so 3 sigma
* check is 3*sqrt(2)*RMS/sqrt(1000) = .13*RMS
!
! Can't use RMS, since taking sum of pedestal**2 for these signals
! gives rollover for integer*4.  Just assume signal is +/-2000
! channels, gives sigma of 100 channels, so check for diff>130.
! 
* Note: this is (for some reason) called for pedestal events as well,
* so we need to start counting only after gfrx_adc_ped is set.

      if (numfr.lt.1000 .and. gfrx_adc_ped.gt.1.0) then
        numfr = numfr + 1
        sumfrx = sumfrx + gfrx_raw_adc
        sumfry = sumfry + gfry_raw_adc

        if (numfr.eq.1000) then
          avefrx = sumfrx / float(numfr)
          avefry = sumfry / float(numfr)
          if (abs(avefrx-gfrx_adc_ped).gt.130.) then
            write(6,*) 'FRPED: peds give <frx>=',gfrx_adc_ped,
     $          '  realevents give <frx>=',avefrx
          endif
          if (abs(avefry-gfry_adc_ped).gt.130.) then
            write(6,*) 'FRPED: peds give <fry>=',gfry_adc_ped,
     $          '  realevents give <fry>=',avefry
          endif
        endif

      endif

* calculate raster position from ADC value.

      gfrx_adc = gfrx_raw_adc - gfrx_adc_ped
      gfry_adc = gfry_raw_adc - gfry_adc_ped
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

c     figure out helicity from ADC signals:
      if(gmisc_dec_data(1,2).gt.8000.and.gmisc_dec_data(2,2).lt.2000) then
         gbeam_helicity_ADC = 1
      else if(gmisc_dec_data(2,2).ge.8000.and.gmisc_dec_data(1,2).lt.2000) then
         gbeam_helicity_ADC = -1
      else
         gbeam_helicity_ADC = 0
      endif 
c      write(*,*)gmisc_dec_data(1,2),gmisc_dec_data(2,2),
c     ,     gmisc_dec_data(2,2),gmisc_dec_data(1,2),gbeam_helicity_ADC
 

c     for now just trust the trigger supervisor more than the ADC which can be noisy

      gbeam_helicity = gbeam_helicity_TS

c      write(*,*) 'h+ signal = ',gmisc_dec_data(1,2)
c      write(*,*) 'h- signal = ',gmisc_dec_data(2,2)


*     Slow Raster Signals:  !!!!!! SLOTS NEED TO BE DETERMINED
*     ===================
      gsrx_raw_adc = gmisc_dec_data(4,2)   ! raw info matching MAP (reversed order)!
      gsry_raw_adc = gmisc_dec_data(3,2)

! 2nd copy of slow raster read out in Hall C (for use
! when HMS and BETA re  running stand-alone)
      gsrx_raw_adc2 = gmisc_dec_data(24,2)   ! raw info matching MAP (reversed order)!
      gsry_raw_adc2 = gmisc_dec_data(26,2)

c histrogram
      if(gepid_slowrastx.gt.0)
     >   call hf1(gepid_slowrastx,gsrx_raw_adc,1.)
      if(gepid_slowrasty.gt.0)
     >   call hf1(gepid_slowrasty,gsry_raw_adc,1.)
      if(gepid_slowrastxy.gt.0)
     >   call hf2(gepid_slowrastxy,
     >   gsrx_raw_adc,gsry_raw_adc,1.)
      if(gepid_slowrastxy2.gt.0)
     >   call hf2(gepid_slowrastxy2,
     >   gsrx_raw_adc2,gsry_raw_adc2,1.)

      gsrx_adc = gsrx_raw_adc    ! we do not want peds subtracted
      gsry_adc = gsry_raw_adc   
      
c      gsrx_adc = gsrx_raw_adc - gsrx_adc_ped
c      gsry_adc = gsry_raw_adc - gsry_adc_ped

c commented this out, becuse (3,2) is used for slow raster, and
c (5,2) is 
c      gsrx_sync =  gmisc_dec_data(3,2)! - gsrx_sync_mean
c      gsry_sync =  gmisc_dec_data(5,2)! - gsry_sync_mean


      return
      end
