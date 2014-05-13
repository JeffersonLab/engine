       SUBROUTINE H_TOF(ABORT,errmsg)
*--------------------------------------------------------
*-
*-   Purpose and Methods : Analyze scintillator information for each track 
*-
*-      Required Input BANKS     HMS_RAW_SCIN
*-                               HMS_DECODED_SCIN
*-                               HMS_FOCAL_PLANE
*-
*-      Output BANKS             HMS_TRACK_TESTS
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
* author: John Arrington
* created: 2/22/94
*
* h_tof finds the time of flight for a particle from
* the hodoscope TDC information.  It corrects for pulse
* height walk, time lag from the hit to the pmt signal,
* and time offsets for each signal.  It requires the
* hodoscope ADC and TDC information, the track, and
* the correction parameters.
*
* $Log: h_tof.f,v $
* Revision 1.18  1999/06/10 16:52:12  csa
* (JRA) Cosmetic changes
*
* Revision 1.17  1997/03/19 18:43:45  saw
* (JRA) Don't neglect negative side of hodoscopes
*
* Revision 1.16  1996/09/04 13:36:00  saw
* (JRA) Include actual beta in calculation of focal plane time.
*
* Revision 1.15  1996/01/16 22:00:15  cdaq
* (JRA)
*
* Revision 1.14  1995/05/22 19:39:29  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.13  1995/02/10  18:59:41  cdaq
* (JRA) Add track index to hgood_plane_time, hgood_scin_time, hgood_tdc_pos,
* and  hgood_tdc_neg
*
* Revision 1.12  1995/02/02  16:35:25  cdaq
* (JRA) Zero out some variables at start, minph variables now per pmt,
*       hscin_adc_pos/neg change to floats.
*
* Revision 1.11  1995/01/31  21:49:32  cdaq
* (JRA) Added count of pmt's firing and cosmetic changes.
*
* Revision 1.10  1995/01/30  22:09:24  cdaq
* (JRA) Cosmetic changes.  Remove commented out code to dump time of
*       flight fitting data.
*
* Revision 1.9  1995/01/27  19:26:13  cdaq
* (JRA) Add calculation of time for each plane.  Add commented out
*       code to dump time of flight fitting data.
*
* Revision 1.8  1995/01/18  16:26:48  cdaq
* (SAW) Catch negative ADC values in argument of square root
*
* Revision 1.7  1994/09/13  21:25:35  cdaq
* (JRA) save extra diagnostic variables, require 2 hits/counter, add dedx
*
* Revision 1.6  1994/08/02  20:11:47  cdaq
* (JRA) Some hacks
*
* Revision 1.5  1994/07/21  13:29:45  cdaq
* (JRA) Correct sign on a time correction
*
* Revision 1.4  1994/07/08  19:43:53  cdaq
* (JRA) Keep list of wether hits are on track or not
*
* Revision 1.3  1994/05/13  02:36:30  cdaq
* (DFG) remove h_prt_track_tests call
*
* Revision 1.2  1994/04/13  16:28:53  cdaq
* (DFG) Add check for zero track
*
* Revision 1.1  1994/02/21  16:06:29  cdaq
* Initial revision
*
*--------------------------------------------------------
      IMPLICIT NONE
*
      character*50 here
      parameter (here= 'H_TOF')
*
      logical ABORT
      character*(*) errmsg
*
      INCLUDE 'hms_data_structures.cmn'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      include 'hms_scin_parms.cmn'
      include 'hms_scin_tof.cmn'
      integer*4 hit, trk
      integer*4 plane,ind,counter
      integer*4 hntof_pairs
      real*4 adc_ph                     !pulse height (channels)
      real*4 xhit_coord,yhit_coord
      real*4 time
      real*4 p,betap         !momentum and velocity from momentum, assuming desired mass
      real*4 path
      real*4 sum_fp_time,sum_plane_time(hnum_scin_planes)
      integer*4 num_fp_time,num_plane_time(hnum_scin_planes)
      save
*
*--------------------------------------------------------
*
      ABORT= .FALSE.
      errmsg = ' '

      if(hntracks_fp.le.0 .or. hscin_tot_hits.le.0) then
        do trk = 1 , hntracks_fp
          hnum_scin_hit(trk) = 0
          hnum_pmt_hit(trk) = 0
          hbeta(trk) = 0
          hbeta_chisq(trk) = -3
          htime_at_fp(trk) = 0
        enddo
        goto 666
      endif
 
**MAIN LOOP:  Loop over all tracks and get corrected time, tof, beta...
      do trk = 1 , hntracks_fp

** Initialize counter,flags...
        hntof = 0
        hntof_pairs = 0
        sum_fp_time = 0.
        num_fp_time = 0
        hnum_scin_hit(trk) = 0
        hnum_pmt_hit(trk) = 0
        p = hp_tar(trk)
        betap = p/sqrt(p*p+hpartmass*hpartmass)

        do plane = 1 , hnum_scin_planes
          hgood_plane_time(trk,plane) = .false.
          sum_plane_time(plane) = 0.
          num_plane_time(plane) = 0
        enddo

        do hit = 1 , hscin_tot_hits
          hgood_scin_time(trk,hit) = .false.
          hgood_tdc_pos(trk,hit) = .false.
          hgood_tdc_neg(trk,hit) = .false.
          hgood_tdc_pos_pmtcut(trk,hit) = .false.
          hgood_tdc_neg_pmtcut(trk,hit) = .false.
          hscin_time(hit) = 0
          hscin_sigma(hit) = 0
        enddo

        do hit = 1 , hscin_tot_hits
          plane = hscin_plane_num(hit)

          counter = hscin_counter_num(hit)           

**    Find hit position
          xhit_coord = hx_fp(trk) + hxp_fp(trk)*hscin_zpos(hit)
          yhit_coord = hy_fp(trk) + hyp_fp(trk)*hscin_zpos(hit)
          if (plane.eq.1 .or. plane.eq.3) then !x plane
            hscin_trans_coord(hit) = xhit_coord
            hscin_long_coord(hit) = yhit_coord
          else if (plane.eq.2 .or. plane.eq.4) then !y plane
            hscin_trans_coord(hit) = yhit_coord
            hscin_long_coord(hit) = xhit_coord
          else                          !bad plane #.
            abort = .true.
            write(errmsg,*) 'hscin_plane_num(',hit,') = ',plane
            call g_prepend(here,errmsg)
            return
          endif

**    Check if scin is on track
          if (abs(hscin_center_coord(hit)-hscin_trans_coord(hit))
     &         .gt.(hscin_width(hit)/2.+hscin_slop(hit))) then

            hscin_on_track(trk,hit) = .false.
          else
            hscin_on_track(trk,hit) = .true.
**    Check for good TDC
            if (hscin_tdc_pos(hit) .ge. hscin_tdc_min .and.  
     1          hscin_tdc_pos(hit) .le. hscin_tdc_max ) then

              hgood_tdc_pos(trk,hit) = .true.

**    Calculate time for each tube with a good tdc. 'pos' side first.
!     for PMTs that are not excluded.

              if(.not. (plane.eq.4.and.(counter.eq.5.or.
     >           counter.eq.4.or.counter.eq.6.or.counter.eq.2))) then

                  hgood_tdc_pos_pmtcut(trk,hit) = .true.
                  hntof = hntof + 1
                  adc_ph = hscin_adc_pos(hit)
                  path = hscin_pos_coord(hit) - hscin_long_coord(hit)

*     Convert TDC value to time, do pulse height correction, correction for
*     propogation of light thru scintillator, and offset.
                  time = hscin_tdc_pos(hit) * hscin_tdc_to_time
                  time = time - hscin_pos_phc_coeff(hit) *
     &                 sqrt(max(0.,(adc_ph/hscin_pos_minph(hit)-1.)))
                  time = time - path/hscin_vel_light(hit)
                  hscin_pos_time(hit) = time - hscin_pos_time_offset(hit)
              endif
            endif

**    Repeat for pmts on 'negative' side
            if (hscin_tdc_neg(hit).ge.hscin_tdc_min .and. !good tdc
     1           hscin_tdc_neg(hit).le.hscin_tdc_max) then

              hgood_tdc_neg(trk,hit) = .true.

              if(.not. (plane.eq.4.and.(counter.eq.5.or.
     >            counter.eq.4.or.counter.eq.6.or.counter.eq.2))) then

                  hgood_tdc_neg_pmtcut(trk,hit) = .true.
                  hntof = hntof + 1
                  adc_ph = hscin_adc_neg(hit)
                  path = hscin_long_coord(hit) - hscin_neg_coord(hit)
                  time = hscin_tdc_neg(hit) * hscin_tdc_to_time
                  time = time - hscin_neg_phc_coeff(hit) *
     &                 sqrt(max(0.,(adc_ph/hscin_neg_minph(hit)-1.)))
                  time = time - path/hscin_vel_light(hit)
                  hscin_neg_time(hit) = time - hscin_neg_time_offset(hit)
              endif
            endif

**    Calculate ave time for scintillator and error.
            if (hgood_tdc_pos_pmtcut(trk,hit)) then
              if (hgood_tdc_neg_pmtcut(trk,hit)) then
                hscin_time(hit) = (hscin_neg_time(hit) + hscin_pos_time(hit))/2.
                hscin_sigma(hit) = sqrt(hscin_neg_sigma(hit)**2 + 
     1               hscin_pos_sigma(hit)**2)/2.
                hgood_scin_time(trk,hit) = .true.
                hntof_pairs = hntof_pairs + 1
              else
                hscin_time(hit) = hscin_pos_time(hit)
                hscin_sigma(hit) = hscin_pos_sigma(hit)
                hgood_scin_time(trk,hit) = .true.
*                hgood_scin_time(trk,hit) = .false.
              endif
            else                        ! if hgood_tdc_neg = .false.
              if (hgood_tdc_neg_pmtcut(trk,hit)) then
                hscin_time(hit) = hscin_neg_time(hit)
                hscin_sigma(hit) = hscin_neg_sigma(hit)
                hgood_scin_time(trk,hit) = .true.
*                hgood_scin_time(trk,hit) = .false.
              endif
            endif
c     Get time at focal plane
            if (hgood_scin_time(trk,hit)) then
              hscin_time_fp(hit) = hscin_time(hit)
     &             - (hscin_zpos(hit)/(29.979*betap) *
     &             sqrt(1.+hxp_fp(trk)*hxp_fp(trk)+hyp_fp(trk)*hyp_fp(trk)))
              sum_fp_time = sum_fp_time + hscin_time_fp(hit)
              num_fp_time = num_fp_time + 1
              sum_plane_time(plane)=sum_plane_time(plane)
     &             +hscin_time_fp(hit)
              num_plane_time(plane)=num_plane_time(plane)+1
              hnum_scin_hit(trk) = hnum_scin_hit(trk) + 1
              hscin_hit(trk,hnum_scin_hit(trk)) = hit
              hscin_fptime(trk,hnum_scin_hit(trk)) = hscin_time_fp(hit)

              if (hgood_tdc_pos_pmtcut(trk,hit) .and. hgood_tdc_neg_pmtcut(trk,hit)) then
                hnum_pmt_hit(trk) = hnum_pmt_hit(trk) + 2
              else
                hnum_pmt_hit(trk) = hnum_pmt_hit(trk) + 1
              endif
              if (hgood_tdc_pos_pmtcut(trk,hit)) then
                if (hgood_tdc_neg_pmtcut(trk,hit)) then
                  hdedx(trk,hnum_scin_hit(trk)) = sqrt(max(0.,
     &                 hscin_adc_pos(hit)*hscin_adc_neg(hit)))
                else
                  hdedx(trk,hnum_scin_hit(trk))=max(0.,hscin_adc_pos(hit))
                endif
              else
                if (hgood_tdc_neg_pmtcut(trk,hit)) then
                  hdedx(trk,hnum_scin_hit(trk))=max(0.,hscin_adc_neg(hit))
                else
                  hdedx(trk,hnum_scin_hit(trk)) = 0.
                endif
              endif
            endif
            
          endif                 !end of 'if scintillator was on the track'
          
**    See if there are any good time measurements in the plane.
          if (hgood_scin_time(trk,hit)) then
            hgood_plane_time(trk,plane) = .true. !still in loop over hits.
          endif
          
        enddo                           !end of loop over hit scintillators

**    Fit beta if there are enough time measurements (one upper, one lower)
        if ((hgood_plane_time(trk,1) .or. hgood_plane_time(trk,2)) .and.
     1       (hgood_plane_time(trk,3) .or. hgood_plane_time(trk,4))) then
          call h_tof_fit(abort,errmsg,trk) !fit velocity of particle
          if (abort) then
            call g_prepend(here,errmsg)
            return
          endif
        else             !cannot fit beta from given time measurements
          hbeta(trk) = 0.
          hbeta_chisq(trk) = -1.
        endif
        
        if (num_fp_time .ne. 0) then
          htime_at_fp(trk) = sum_fp_time / float(num_fp_time)
        endif
        
        do ind=1,4
          if (num_plane_time(ind) .ne. 0) then
            h_fptime(ind)=sum_plane_time(ind)/float(num_plane_time(ind))
          else
            h_fptime(ind)=1000.*ind
          endif
        enddo

        h_fptimedif(1)=h_fptime(1)-h_fptime(2)
        h_fptimedif(2)=h_fptime(1)-h_fptime(3)
        h_fptimedif(3)=h_fptime(1)-h_fptime(4)
        h_fptimedif(4)=h_fptime(2)-h_fptime(3)
        h_fptimedif(5)=h_fptime(2)-h_fptime(4)
        h_fptimedif(6)=h_fptime(3)-h_fptime(4)   
*     
*     Dump tof common blocks if (hdebugprinttoftracks is set

        if(hdebugprinttoftracks.ne.0 ) call h_prt_tof(trk)

      enddo                             !end of loop over tracks

 666  continue

      RETURN
      END

