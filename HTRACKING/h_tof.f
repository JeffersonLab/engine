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
* $Log$
* Revision 1.2  1994/04/13 16:28:53  cdaq
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
      INCLUDE 'gen_data_structures.cmn'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      include 'hms_scin_parms.cmn'
      include 'hms_scin_tof.cmn'
      integer*4 hit, trk
      integer*4 plane
      real*4 adc_ph                     !pulse height (channels)
      real*4 xhit_coord,yhit_coord
      real*4 time
      real*4 path
      save
*
*--------------------------------------------------------
*
      ABORT= .FALSE.
      errmsg = ' '
**MAIN LOOP:  Loop over all tracks and get corrected time, tof, beta...
      if(hntracks_fp.gt.0 .and. hscin_tot_hits.gt.0) then
         do trk = 1 , hntracks_fp

** Initialize counter,flags...
            hntof = 0

            do plane = 1 , hnum_scin_planes
               hgood_plane_time(plane) = .false.
            enddo

            do hit = 1 , hscin_tot_hits
               hgood_scin_time(hit) = .false.
               hgood_tdc_pos(hit) = .false.
               hgood_tdc_neg(hit) = .false.
               hscin_time(hit) = 0
               hscin_sigma(hit) = 0
            enddo

            do hit = 1 , hscin_tot_hits
               plane = hscin_plane_num(hit)

** Find hit position
               xhit_coord = hx_fp(trk) + hxp_fp(trk)*hscin_zpos(hit)
               yhit_coord = hy_fp(trk) + hyp_fp(trk)*hscin_zpos(hit)
               if (plane.eq.1 .or. plane.eq.3) then !x plane
                  hscin_trans_coord(hit) = xhit_coord
                  hscin_long_coord(hit) = yhit_coord
               else if (plane.eq.2 .or. plane.eq.4) then !y plane
                  hscin_trans_coord(hit) = yhit_coord
                  hscin_long_coord(hit) = xhit_coord
               else                     !bad plane #.
                  abort = .true.
                  write(errmsg,*) 'hscin_plane_num(',hit,') = ',plane
                  call g_prepend(here,errmsg)
                  return
               endif

** Check if scin is on track
               if (abs(hscin_center_coord(hit)-hscin_trans_coord(hit))
     $              .le.(hscin_width(hit)/2.+hscin_slop(hit))) then

**  Check for good TDC
                  if (hscin_tdc_pos(hit) .ge. hscin_tdc_min .and.  
     1                 hscin_tdc_pos(hit) .le. hscin_tdc_max) then

** Calculate time for each tube with a good tdc. 'pos' side first.
                     hgood_tdc_pos(hit) = .true.
                     hntof = hntof + 1
                     adc_ph = float(hscin_adc_pos(hit))
                     path = hscin_pos_coord(hit) - hscin_long_coord(hit)

* Convert TDC value to time, do pulse height correction, correction for
* propogation of light thru scintillator, and offset.
                     time = hscin_tdc_pos(hit) * hscin_tdc_to_time
                     time = time +
     1                    hscin_pos_phc_coeff(hit) *
     $                    sqrt(max(adc_ph,hscin_minph))
                     time = time - path/hscin_vel_light(hit)
                     hscin_pos_time(hit) = time
     $                    - hscin_pos_time_offset(hit)
                  endif

** Repeat for pmts on 'negative' side
                  if (hscin_tdc_neg(hit).ge.hscin_tdc_min .and. !good tdc
     1                 hscin_tdc_neg(hit).le.hscin_tdc_max) then

                     hgood_tdc_neg(hit) = .true.
                     hntof = hntof + 1
                     adc_ph = hscin_adc_neg(hit)
                     path = hscin_long_coord(hit) - hscin_neg_coord(hit)
                     time = hscin_tdc_neg(hit) * hscin_tdc_to_time
                     time = time + 
     1                    hscin_neg_phc_coeff(hit) *
     $                    sqrt(max(adc_ph,hscin_minph))
                     time = time - path/hscin_vel_light(hit)
                     hscin_neg_time(hit) = time
     $                    - hscin_neg_time_offset(hit)
                  endif

** Calculate ave time for scintillator and error.
                  if (hgood_tdc_pos(hit)) then
                     if (hgood_tdc_neg(hit)) then
                        hscin_time(hit) = 
     1                       (hscin_neg_time(hit) + hscin_pos_time(hit))/2.
                        hscin_sigma(hit) = sqrt(hscin_neg_sigma(hit)**2 + 
     1                       hscin_pos_sigma(hit)**2)/2.
                        hgood_scin_time(hit) = .true.
                     else
                        hscin_time(hit) = hscin_pos_time(hit)
                        hscin_sigma(hit) = hscin_pos_sigma(hit)
                        hgood_scin_time(hit) = .true.
                     endif
                  else                  ! if hgood_tdc_neg = .false.
                     if (hgood_tdc_neg(hit)) then
                        hscin_time(hit) = hscin_neg_time(hit)
                        hscin_sigma(hit) = hscin_neg_sigma(hit)
                        hgood_scin_time(hit) = .true.
                     endif
                  endif

               endif                    !end of 'if scintillator was on the track'

** See if there are any good time measurements in the plane.
               if (hgood_scin_time(hit)) then
                  hgood_plane_time(plane) = .true. !still in loop over hits.
               endif

            enddo                       !end of loop over hit scintillators


** Fit beta if there are enough time measurements (one upper, one lower)
            if ((hgood_plane_time(1) .or. hgood_plane_time(2)) .and.
     1           (hgood_plane_time(3) .or. hgood_plane_time(4))) then
               call h_tof_fit(abort,errmsg,trk) !fit velocity of particle
               if (abort) then
                  call g_prepend(here,errmsg)
                  return
               endif
            else                        !cannot fit beta from given time measurements
               hbeta(trk) = 0.
               hbeta_chisq(trk) = -1.
            endif
*
*     Dump tof common blocks if (hdebugprinttoftracks is set
            if(hdebugprinttoftracks.ne.0 ) then 
               call h_prt_tof(trk)
            endif
*
*     Dump HMS_TRACK_TESTS if hdebugprinttracktests is set
            if( hdebugprinttracktests .ne. 0 ) then
               call h_prt_track_tests
            endif
*
         enddo                          !end of loop over tracks
*
      endif                             ! end test on zero tracks
      RETURN
      END
