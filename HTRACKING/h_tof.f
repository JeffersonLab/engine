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
* Revision 1.9  1995/01/27 19:26:13  cdaq
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
      INCLUDE 'gen_data_structures.cmn'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      include 'hms_scin_parms.cmn'
      include 'hms_scin_tof.cmn'
      integer*4 hit, trk
      integer*4 plane,ind
      integer*4 numplanes
      integer*4 pmt,cnt,lay,dir,ihit
      integer*4 hntof_pairs
      real*4 ph,tim,betap
      real*4 adc_ph                     !pulse height (channels)
      real*4 xhit_coord,yhit_coord
      real*4 time
      real*4 path
      real*4 sum_fp_time,sum_plane_time(hnum_scin_planes)
      integer*4 num_fp_time,num_plane_time(hnum_scin_planes)
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
          hntof_pairs = 0
          sum_fp_time = 0.
          num_fp_time = 0
          hnum_scin_hit(trk) = 0

          do plane = 1 , hnum_scin_planes
            hgood_plane_time(plane) = .false.
            sum_plane_time(plane) = 0.
            num_plane_time(plane) = 0
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

**    Find hit position
            xhit_coord = hx_fp(trk) + hxp_fp(trk)*hscin_zpos(hit)
            yhit_coord = hy_fp(trk) + hyp_fp(trk)*hscin_zpos(hit)
            if (plane.eq.1 .or. plane.eq.3) then !x plane
              hscin_trans_coord(hit) = xhit_coord
              hscin_long_coord(hit) = yhit_coord
            else if (plane.eq.2 .or. plane.eq.4) then !y plane
              hscin_trans_coord(hit) = yhit_coord
              hscin_long_coord(hit) = xhit_coord
            else                        !bad plane #.
              abort = .true.
              write(errmsg,*) 'hscin_plane_num(',hit,') = ',plane
              call g_prepend(here,errmsg)
              return
            endif

**    Check if scin is on track
            if (abs(hscin_center_coord(hit)-hscin_trans_coord(hit))
     $           .gt.(hscin_width(hit)/2.+hscin_slop(hit))) then

              hscin_on_track(trk,hit) = .false.
            else
              hscin_on_track(trk,hit) = .true.
**    Check for good TDC
              if (hscin_tdc_pos(hit) .ge. hscin_tdc_min .and.  
     1             hscin_tdc_pos(hit) .le. hscin_tdc_max) then

**    Calculate time for each tube with a good tdc. 'pos' side first.
                hgood_tdc_pos(hit) = .true.
                hntof = hntof + 1
                adc_ph = float(hscin_adc_pos(hit))
                path = hscin_pos_coord(hit) - hscin_long_coord(hit)

*     Convert TDC value to time, do pulse height correction, correction for
*     propogation of light thru scintillator, and offset.
                time = hscin_tdc_pos(hit) * hscin_tdc_to_time
                time = time -
     1               hscin_pos_phc_coeff(hit) *
     $               sqrt(max(0.,(adc_ph/hscin_minph-1.)))
                time = time - path/hscin_vel_light(hit)
                hscin_pos_time(hit) = time
     $               - hscin_pos_time_offset(hit)
              endif

**    Repeat for pmts on 'negative' side
              if (hscin_tdc_neg(hit).ge.hscin_tdc_min .and. !good tdc
     1             hscin_tdc_neg(hit).le.hscin_tdc_max) then

                hgood_tdc_neg(hit) = .true.
                hntof = hntof + 1
                adc_ph = hscin_adc_neg(hit)
                path = hscin_long_coord(hit) - hscin_neg_coord(hit)
                time = hscin_tdc_neg(hit) * hscin_tdc_to_time
                time = time - 
     1               hscin_neg_phc_coeff(hit) *
     $               sqrt(max(0.,(adc_ph/hscin_minph-1.)))
                time = time - path/hscin_vel_light(hit)
                hscin_neg_time(hit) = time
     $               - hscin_neg_time_offset(hit)
              endif

**    Calculate ave time for scintillator and error.
              if (hgood_tdc_pos(hit)) then
                if (hgood_tdc_neg(hit)) then
                  hscin_time(hit) = 
     1                 (hscin_neg_time(hit) + hscin_pos_time(hit))/2.
                  hscin_sigma(hit) = sqrt(hscin_neg_sigma(hit)**2 + 
     1                 hscin_pos_sigma(hit)**2)/2.
                  hgood_scin_time(hit) = .true.
                  hntof_pairs = hntof_pairs + 1
                else
                  hscin_time(hit) = hscin_pos_time(hit)
                  hscin_sigma(hit) = hscin_pos_sigma(hit)
*     hgood_scin_time(hit) = .true.
                  hgood_scin_time(hit) = .false.
                endif
              else                      ! if hgood_tdc_neg = .false.
                if (hgood_tdc_neg(hit)) then
                  hscin_time(hit) = hscin_neg_time(hit)
                  hscin_sigma(hit) = hscin_neg_sigma(hit)
*     hgood_scin_time(hit) = .true.
                  hgood_scin_time(hit) = .false.
                endif
              endif
c     Get time at focal plane
              if (hgood_scin_time(hit)) then
* for electrons:
                hscin_time_fp(hit) = hscin_time(hit) -
     &               (hscin_zpos(hit)/30.) * sqrt(1. + hxp_fp(trk)
     $               *hxp_fp(trk) +hyp_fp(trk)*hyp_fp(trk))
                sum_fp_time = sum_fp_time + hscin_time_fp(hit)
                num_fp_time = num_fp_time + 1
                sum_plane_time(plane)=sum_plane_time(plane)+hscin_time_fp(hit)
                num_plane_time(plane)=num_plane_time(plane)+1
                hnum_scin_hit(trk) = hnum_scin_hit(trk) + 1
                hscin_hit(trk,hnum_scin_hit(trk)) = hit
                if (hgood_tdc_pos(hit)) then
                  if (hgood_tdc_neg(hit)) then
ccc The following sometimes results in square roots of negative numbers
ccc Supposedly, no one uses this right now (SAW 1/17/95), but it is used
ccc in hphysics in the "goodtrack" figurer outer.
                    if(hscin_adc_pos(hit) .ge. 0.0 .and.
     $                   hscin_adc_neg(hit) .ge. 0.0) then
                      hdedx(trk,hnum_scin_hit(trk)) = 
     &                     sqrt(float(hscin_adc_pos(hit)*hscin_adc_neg(hit)
     $                     ))
                    else
                      hdedx(trk,hnum_scin_hit(trk))= 0.0
                    endif
                  else
                    hdedx(trk,hnum_scin_hit(trk))=float(hscin_adc_pos(hit
     $                   ))
                  endif
                else
                  if (hgood_tdc_neg(hit)) then
                    hdedx(trk,hnum_scin_hit(trk))=float(hscin_adc_neg(hit
     $                   ))
                  else
                    hdedx(trk,hnum_scin_hit(trk)) = 0
                  endif
                endif
              endif

            endif                       !end of 'if scintillator was on the track'

**    See if there are any good time measurements in the plane.
            if (hgood_scin_time(hit)) then
              hgood_plane_time(plane) = .true. !still in loop over hits.
            endif

          enddo                         !end of loop over hit scintillators


**    Fit beta if there are enough time measurements (one upper, one lower)
***   if ((hgood_plane_time(1) .or. hgood_plane_time(2)) .and.
***   1           (hgood_plane_time(3) .or. hgood_plane_time(4))) then

**    For now, require at least 3 planes, to avoid bad fits.
          numplanes=0
          if (hgood_plane_time(1)) numplanes=numplanes+1
          if (hgood_plane_time(2)) numplanes=numplanes+1
          if (hgood_plane_time(3)) numplanes=numplanes+1
          if (hgood_plane_time(4)) numplanes=numplanes+1
          if (numplanes.ge.3) then
            call h_tof_fit(abort,errmsg,trk) !fit velocity of particle
            if (abort) then
              call g_prepend(here,errmsg)
              return
            endif
          else                          !cannot fit beta from given time measurements
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

*
*  Write out TOF fitting data.
*
c        if (trk.eq.1 .and. numplanes.eq.4 .and. hntof_pairs.le.5) then
* NEW TEST!
c         if (hcer_adc(1) .ge. 565 .or. hcer_adc(2) .ge. 520) then
c          betap=1.
c          write(37,111) hntof_pairs*2,hx_fp(trk),hxp_fp(trk),
c     &                        hy_fp(trk),hyp_fp(trk),betap
c111       format(i4,5f10.5)
c          do ihit = 1, hscin_tot_hits
c            if (hgood_scin_time(ihit)) then   !scin. was on track
c              cnt=hscin_counter_num(ihit)
c              lay=int((hscin_plane_num(ihit)+1)/2)
c              dir=mod(hscin_plane_num(ihit)+1,2)+1
c              pmt=1
c              tim=hscin_tdc_pos(ihit)*hscin_tdc_to_time
c              ph=hscin_adc_pos(ihit)
c              write(37,112) pmt,cnt,lay,dir,ph,tim
c              pmt=2
c              tim=hscin_tdc_neg(ihit)*hscin_tdc_to_time
c              ph=hscin_adc_neg(ihit)
c              write(37,112) pmt,cnt,lay,dir,ph,tim
c112           format(4i4,2f12.6)
c            endif
c          enddo
c         endif
c        endif
*
        enddo                           !end of loop over tracks
      endif                             ! end test on zero tracks
      RETURN
      END
