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
* Revision 1.8  1995/05/17 16:46:14  cdaq
* (JRA) Add sum_plane_time and num_plane_time
*
* Revision 1.7  1995/02/23  15:47:24  cdaq
* (JRA) Catch up to HMS.  Add track index to hgood_plane_time,
* hgood_scin_time, hgood_tdc_pos, and hgood_tdc_neg.  Zero out some
* variables at start, minph variables now per pmt, hscin_adc_pos/neg
* change to floats.  Added count of pmt's firing and cosmetic changes.
* Cosmetic changes.  Remove commented out code to dump time of light
* fitting data.  Add calculation of time for each plane.  Add commented
* out code to dump time of flight fitting data.
*
* Revision 1.6  1995/01/18  20:41:48  cdaq
* (SAW) Catch negative ADC values in argument of square root
*
* Revision 1.5  1994/11/23  14:15:23  cdaq
* (SPB) Recopied from hms file and modified names for SOS
*
* Revision 1.4  1994/05/13  03:55:14  cdaq
* (DFG) Remove s_prt_track_tests call
*
* Revision 1.3  1994/04/13  05:34:55  cdaq
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
      integer*4 plane,ind
      integer*4 numplanes
      integer*4 sntof_pairs
      real*4 adc_ph                     !pulse height (channels)
      real*4 xhit_coord,yhit_coord
      real*4 time
      real*4 path
      real*4 sum_fp_time,sum_plane_time(snum_scin_planes)
      integer*4 num_fp_time,num_plane_time(snum_scin_planes)
      save
*     
*--------------------------------------------------------
*     
      ABORT= .FALSE.
      errmsg = ' '

      if(sntracks_fp.le.0 .or. sscin_tot_hits.le.0) then
        do trk = 1 , sntracks_fp
          snum_scin_hit(trk) = 0
          snum_pmt_hit(trk) = 0
          sbeta(trk) = 0
          sbeta_chisq(trk) = -2
          stime_at_fp(trk) = 0
        enddo
        goto 666
      endif

**    MAIN LOOP:  Loop over all tracks and get corrected time, tof, beta...
      do trk = 1 , sntracks_fp

**    Initialize counter,flags...
        sntof = 0
        sntof_pairs = 0
        sum_fp_time = 0.
        num_fp_time = 0
        snum_scin_hit(trk) = 0
        snum_pmt_hit(trk) = 0

        do plane = 1 , snum_scin_planes
          sgood_plane_time(trk,plane) = .false.
          sum_plane_time(plane) = 0.
          num_plane_time(plane) = 0
        enddo

        do hit = 1 , sscin_tot_hits
          sgood_scin_time(trk,hit) = .false.
          sgood_tdc_pos(trk,hit) = .false.
          sgood_tdc_neg(trk,hit) = .false.
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
          else                          !bad plane #.
            abort = .true.
            write(errmsg,*) 'sscin_plane_num(',hit,') = ',plane
            call g_prepend(here,errmsg)
            return
          endif

**    Check if scin is on track
          if (abs(sscin_center_coord(hit)-sscin_trans_coord(hit)) .gt
     $         .(sscin_width(hit)/2.+sscin_slop(hit))) then

            sscin_on_track(trk,hit) = .false.
          else
            sscin_on_track(trk,hit) = .true.
***   Check for good TDC
            if (sscin_tdc_pos(hit) .ge. sscin_tdc_min .and.  
     1           sscin_tdc_pos(hit) .le. sscin_tdc_max) then

**    Calculate time for each tube with a good tdc. 'pos' side first.
              sgood_tdc_pos(trk,hit) = .true.
              sntof = sntof + 1
              adc_ph = sscin_adc_pos(hit)
              path = sscin_pos_coord(hit) - sscin_long_coord(hit)

*     Convert TDC value to time, do pulse height correction, correction for
*     propogation of light thru scintillator, and offset.
              time = sscin_tdc_pos(hit) * sscin_tdc_to_time
              time = time -
     1             sscin_pos_phc_coeff(hit) *
     $             sqrt(max(0.,(adc_ph/sscin_pos_minph(hit)-1.)))
              time = time - path/sscin_vel_light(hit)
              sscin_pos_time(hit) = time - sscin_pos_time_offset(hit)
            endif

**    Repeat for pmts on 'negative' side
            if (sscin_tdc_neg(hit).ge.sscin_tdc_min .and. !good tdc
     1           sscin_tdc_neg(hit).le.sscin_tdc_max) then

              sgood_tdc_neg(trk,hit) = .true.
              sntof = sntof + 1
              adc_ph = sscin_adc_neg(hit)
              path = sscin_long_coord(hit) - sscin_neg_coord(hit)
              time = sscin_tdc_neg(hit) * sscin_tdc_to_time
              time = time -
     1             sscin_neg_phc_coeff(hit) *
     $             sqrt(max(0.,(adc_ph/sscin_neg_minph(hit)-1.)))
              time = time - path/sscin_vel_light(hit)
              sscin_neg_time(hit) = time - sscin_neg_time_offset(hit)
            endif

**    Calculate ave time for scintillator and error.
            if (sgood_tdc_pos(trk,hit)) then
              if (sgood_tdc_neg(trk,hit)) then
                sscin_time(hit) = 
     1               (sscin_neg_time(hit) + sscin_pos_time(hit))/2.
                sscin_sigma(hit) = sqrt(sscin_neg_sigma(hit)**2 + 
     1               sscin_pos_sigma(hit)**2)/2.
                sgood_scin_time(trk,hit) = .true.
                sntof_pairs = sntof_pairs + 1
              else
                sscin_time(hit) = sscin_pos_time(hit)
                sscin_sigma(hit) = sscin_pos_sigma(hit)
*                sgood_scin_time(trk,hit) = .false.
                sgood_scin_time(trk,hit) = .true.
              endif
            else                        ! if sgood_tdc_neg = .false.
              if (sgood_tdc_neg(trk,hit)) then
                sscin_time(hit) = sscin_neg_time(hit)
                sscin_sigma(hit) = sscin_neg_sigma(hit)
*                sgood_scin_time(trk,hit) = .true.
                sgood_scin_time(trk,hit) = .false.
              endif
            endif
c     Get time at focal plane
            if (sgood_scin_time(trk,hit)) then
* for electrons:
              sscin_time_fp(hit) = sscin_time(hit) -
     &             (sscin_zpos(hit)/30.) * sqrt(1. + sxp_fp(trk)
     $             *sxp_fp(trk) +syp_fp(trk)*syp_fp(trk))
              sum_fp_time = sum_fp_time + sscin_time_fp(hit)
              num_fp_time = num_fp_time + 1
              sum_plane_time(plane)=sum_plane_time(plane)
     $             +sscin_time_fp(hit)
              num_plane_time(plane)=num_plane_time(plane)+1
              snum_scin_hit(trk) = snum_scin_hit(trk) + 1
              sscin_hit(trk,snum_scin_hit(trk)) = hit
              if (sgood_tdc_pos(trk,hit) .and. sgood_tdc_neg(trk,hit)) then
                snum_pmt_hit(trk) = snum_pmt_hit(trk) + 2
              else
                snum_pmt_hit(trk) = snum_pmt_hit(trk) + 1
              endif
              if (sgood_tdc_pos(trk,hit)) then
                if (sgood_tdc_neg(trk,hit)) then
                  sdedx(trk,snum_scin_hit(trk)) = sqrt(max(0.,
     &                 sscin_adc_pos(hit)*sscin_adc_neg(hit)))
                else
                  sdedx(trk,snum_scin_hit(trk))=max(0.,sscin_adc_pos(hit))
                endif
              else
                if(sgood_tdc_neg(trk,hit)) then
                  sdedx(trk,snum_scin_hit(trk))=max(0.,sscin_adc_neg(hit))
                else
                  sdedx(trk,snum_scin_hit(trk))=0.
                endif
              endif
            endif
            
          endif                         !end of 'if scintillator was on the track'

**    See if there are any good time measurements in the plane.
          if (sgood_scin_time(trk,hit)) then
            sgood_plane_time(trk,plane) = .true. !still in loop over hits.
          endif

        enddo                           !end of loop over hit scintillators

c**    For now, require at least 3 planes, to avoid bad fits.
c        numplanes=0
c        if (sgood_plane_time(trk,1)) numplanes=numplanes+1
c        if (sgood_plane_time(trk,2)) numplanes=numplanes+1
c        if (sgood_plane_time(trk,3)) numplanes=numplanes+1
c        if (sgood_plane_time(trk,4)) numplanes=numplanes+1
c        if (numplanes.ge.3) then

**    Fit beta if there are enough time measurements (one upper, one lower)
        if ((sgood_plane_time(trk,1) .or. sgood_plane_time(trk,2)) .and.
     1       (sgood_plane_time(trk,3) .or. sgood_plane_time(trk,4))) then
          call s_tof_fit(abort,errmsg,trk) !fit velocity of particle
          if (abort) then
            call g_prepend(here,errmsg)
            return
          endif
        else                            !cannot fit beta from given time measurements
          sbeta(trk) = 0.
          sbeta_chisq(trk) = -1.
        endif

        if (num_fp_time .ne. 0) then
          stime_at_fp(trk) = sum_fp_time / float(num_fp_time)
        endif

        do ind=1,4
          if (num_plane_time(ind) .ne. 0) then
            s_fptime(ind)=sum_plane_time(ind)/float(num_plane_time(ind))
          else
            s_fptime(ind)=1000.*ind
          endif
        enddo

        s_fptimedif(1)=s_fptime(1)-s_fptime(2)
        s_fptimedif(2)=s_fptime(1)-s_fptime(3)
        s_fptimedif(3)=s_fptime(1)-s_fptime(4)
        s_fptimedif(4)=s_fptime(2)-s_fptime(3)
        s_fptimedif(5)=s_fptime(2)-s_fptime(4)
        s_fptimedif(6)=s_fptime(3)-s_fptime(4)   
*
*     Dump tof common blocks if (sdebugprinttoftracks is set

        if(sdebugprinttoftracks.ne.0 ) call s_prt_tof(trk)
      enddo                             !end of loop over tracks
*     
 666  continue

      RETURN
      END
