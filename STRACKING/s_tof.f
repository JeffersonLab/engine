       SUBROUTINE S_TOF(ABORT,errmsg)
*--------------------------------------------------------
*-
*-   Purpose and Methods : Analyze SOS scintillator information for each track 
*-
*-      Required Input BANKS     SOS_RAW_SCIN
*-                               SOS_DECODED_SCIN
*-                               SOS_FOCAL_PLANE
*-
*-      Output BANKS             SOS_TRACK_TESTS
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
*-   Created 22-FEB-1994   John Arrington
*
* $Log$
* Revision 1.3  1994/04/13 05:34:55  cdaq
* (SAW) Fix typo
*
* Revision 1.2  1994/04/13  05:30:02  cdaq
* Put in arrington code
* (DFG) Add check for zero track
*       Add calls to print routines
*       Add check for zero hits
*
* Revision 1.1  1994/02/21  16:41:51  cdaq
* Initial revision
*
* s_tof finds the time of flight for a particle from
* the hodoscope TDC information.  It corrects for pulse
* height walk, time lag from the hit to the pmt signal,
* and time offsets for each signal.  It requires the
* hodoscope ADC and TDC information, the track, and
* the correction parameters.
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*50 here
      parameter (here= 'S_TOF')
*
      logical ABORT
      character*(*) errmsg
*
      INCLUDE 'gen_data_structures.cmn'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      include 'sos_scin_parms.cmn'
      include 'sos_scin_tof.cmn'
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
**    MAIN LOOP:  Loop over all tracks and get corrected time, tof, beta...
      if(sntracks_fp.gt.0 .and. sscin_tot_hits.gt.0) then
         do trk = 1 , sntracks_fp

**    Initialize counter,flags...
            sntof = 0

            do plane = 1 , snum_scin_planes
               sgood_plane_time(plane) = .false.
            enddo

            do hit = 1 , sscin_tot_hits
               sgood_scin_time(hit) = .false.
               sgood_tdc_pos(hit) = .false.
               sgood_tdc_neg(hit) = .false.
               sscin_time(hit) = 0
               sscin_sigma(hit) = 0
            enddo


            do hit = 1 , sscin_tot_hits
               plane = sscin_plane_num(hit)

**    Find hit position
               xhit_coord = sx_fp(trk) + sxp_fp(trk)*sscin_zpos(hit)
               yhit_coord = sy_fp(trk) + syp_fp(trk)*sscin_zpos(hit)
               if (plane.eq.1 .or. plane.eq.3) then !x plane
                  sscin_trans_coord(hit) = xhit_coord
                  sscin_long_coord(hit) = yhit_coord
               else if (plane.eq.2 .or. plane.eq.4) then !y plane
                  sscin_trans_coord(hit) = yhit_coord
                  sscin_long_coord(hit) = xhit_coord
               else                     !bad plane #.
                  abort = .true.
                  write(errmsg,*) 'sscin_plane_num(',hit,') = ',plane
                  call g_prepend(here,errmsg)
                  return
               endif

**    Check if scin is on track
               if (abs(sscin_center_coord(hit)-sscin_trans_coord(hit)) .le
     $              .(sscin_width(hit)/2.+sscin_slop(hit))) then

**    Check for good TDC
                  if (sscin_tdc_pos(hit) .ge. sscin_tdc_min .and.  
     1                 sscin_tdc_pos(hit) .le. sscin_tdc_max) then

**    Calculate time for each tube with a good tdc. 'pos' side first.
                     sgood_tdc_pos(hit) = .true.
                     sntof = sntof + 1
                     adc_ph = float(sscin_adc_pos(hit))
                     path = sscin_pos_coord(hit) - sscin_long_coord(hit)

*     Convert TDC value to time, do pulse height correction, correction for
*     propogation of light thru scintillator, and offset.
                     time = sscin_tdc_pos(hit) * sscin_tdc_to_time
                     time = time +
     1                    sscin_pos_phc_coeff(hit) * sqrt(max(adc_ph
     $                    ,sscin_minph))
                     time = time - path/sscin_vel_light(hit)
                     sscin_pos_time(hit) = time - sscin_pos_time_offset(hit
     $                    )
                  endif

**    Repeat for pmts on 'negative' side
                  if (sscin_tdc_neg(hit).ge.sscin_tdc_min .and. !good tdc
     1                 sscin_tdc_neg(hit).le.sscin_tdc_max) then

                     sgood_tdc_neg(hit) = .true.
                     sntof = sntof + 1
                     adc_ph = sscin_adc_neg(hit)
                     path = sscin_long_coord(hit) - sscin_neg_coord(hit)
                     time = sscin_tdc_neg(hit) * sscin_tdc_to_time
                     time = time + 
     1                    sscin_neg_phc_coeff(hit) * sqrt(max(adc_ph
     $                    ,sscin_minph))
                     time = time - path/sscin_vel_light(hit)
                     sscin_neg_time(hit) = time - sscin_neg_time_offset(hit
     $                    )
                  endif

**    Calculate ave time for scintillator and error.
                  if (sgood_tdc_pos(hit)) then
                     if (sgood_tdc_neg(hit)) then
                        sscin_time(hit) = 
     1                       (sscin_neg_time(hit) + sscin_pos_time(hit))/2.
                        sscin_sigma(hit) = sqrt(sscin_neg_sigma(hit)**2 + 
     1                       sscin_pos_sigma(hit)**2)/2.
                        sgood_scin_time(hit) = .true.
                     else
                        sscin_time(hit) = sscin_pos_time(hit)
                        sscin_sigma(hit) = sscin_pos_sigma(hit)
                        sgood_scin_time(hit) = .true.
                     endif
                  else                  ! if sgood_tdc_neg = .false.
                     if (sgood_tdc_neg(hit)) then
                        sscin_time(hit) = sscin_neg_time(hit)
                        sscin_sigma(hit) = sscin_neg_sigma(hit)
                        sgood_scin_time(hit) = .true.
                     endif
                  endif

               endif                    !end of 'if scintillator was on the track'

**    See if there are any good time measurements in the plane.
               if (sgood_scin_time(hit)) then
                  sgood_plane_time(plane) = .true. !still in loop over hits.
               endif

            enddo                       !end of loop over hit scintillators


**    Fit beta if there are enough time measurements (one upper, one lower)
            if ((sgood_plane_time(1) .or. sgood_plane_time(2)) .and.
     1           (sgood_plane_time(3) .or. sgood_plane_time(4))) then
               call s_tof_fit(abort,errmsg,trk) !fit velocity of particle
               if (abort) then
                  call g_prepend(here,errmsg)
                  return
               endif
            else                        !cannot fit beta from given time measurements
               sbeta(trk) = 0.
               sbeta_chisq(trk) = -1.
            endif
*     
*     Dump tof common blocks if (sdebugprinttoftracks is set
            if(sdebugprinttoftracks.ne.0 ) then 
               call s_prt_tof(trk)
            endif
*     
*     Dump SOS_TRACK_TESTS if sdebugprinttracktests is set
            if( sdebugprinttracktests .ne. 0 ) then
               call s_prt_track_tests
            endif
*     
         enddo                          !end of loop over tracks
*     
      endif                             ! end test on zero tracks
      RETURN
      END
