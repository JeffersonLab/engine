      subroutine s_trans_scin(abort,errmsg)
*--------------------------------------------------------
* author: John Arrington
* created: 2/22/94
*
* s_trans_scin fills the sos_decoded_scin common block
* with track independant corrections and parameters
* needed for the drift chamber and tof analysis.
*
* $Log$
* Revision 1.4  1994/11/23 15:08:24  cdaq
* * (SPB) Recopied from hms file and modified names for SOS
*
* Revision 1.3  1994/04/13  20:07:02  cdaq
* (SAW) Fix a typo
*
* Revision 1.2  1994/04/13  19:00:06  cdaq
* (DFG) 3/24  Add s_prt_scin_raw    raw bank dump routine
*             Add s_prt_scin_dec    decoded print routine
*             Add test for zero hits and skip all but initialization
*             Commented out setting abort = .true.
*             Add ABORT and errmsg to arguements
* (DFG) 4/5   Move prt_scin_raw to s_raw_dump_all routine
* (DFG) 4/12  Add call to s_fill_scin_raw_hist
*
* Revision 1.1  1994/02/21  16:43:53  cdaq
* Initial revision
*
*--------------------------------------------------------

      implicit none

      include 'gen_data_structures.cmn'
      include 'sos_scin_parms.cmn'
      include 'sos_scin_tof.cmn'

      logical abort
      character*1024 errmsg
      character*20 here
      parameter (here = 's_trans_scin')

      integer*4 ihit
      integer*4 time_num
      real*4 time_sum
      real*4 fptime
      real*4 scint_center
      real*4 hit_position
      real*4 dist_from_center
      real*4 pos_path, neg_path
      real*4 pos_ph(smax_scin_hits)     !pulse height (channels)
      real*4 neg_ph(smax_scin_hits)
      real*4 postime(smax_scin_hits)
      real*4 negtime(smax_scin_hits)

      save
 
      abort = .false.

**    Find scintillators with real hits (good TDC values)
      call s_strip_scin(abort,errmsg)
      if (abort) then
        call g_prepend(here,errmsg)
        return
      endif
      
** Initialize track-independant quantaties.
      call s_tof_init(abort,errmsg)
      if (abort) then
        call g_prepend(here,errmsg)
        return
      endif

      sgood_start_time = .false.
      if( sscin_tot_hits .gt. 0)  then
** Histogram raw scin
        call s_fill_scin_raw_hist(abort,errmsg)
        if (abort) then
          call g_prepend(here,errmsg)
          return
        endif
      endif
     
** Return if no valid hits.
      if( sscin_tot_hits .le. 0) return

        do ihit = 1 , sscin_tot_hits
          stwo_good_times(ihit) = .false.
        enddo

** Check for two good TDC values.
        do ihit = 1 , sscin_tot_hits
            if ((sscin_tdc_pos(ihit) .ge. sscin_tdc_min) .and.
     1      (sscin_tdc_pos(ihit) .le. sscin_tdc_max) .and.
     2      (sscin_tdc_neg(ihit) .ge. sscin_tdc_min) .and.
     3      (sscin_tdc_neg(ihit) .le. sscin_tdc_max)) then
              stwo_good_times(ihit) = .true.
            endif

        enddo                       !end of loop that finds tube setting time.


** Get corrected time/adc for each scintillator hit
        do ihit = 1 , sscin_tot_hits
          if (stwo_good_times(ihit)) then !both tubes fired

*  Correct time for everything except veloc. correction in order to
*  find hit location from difference in tdc.
            pos_ph(ihit) = float(sscin_adc_pos(ihit))
            postime(ihit) = sscin_tdc_pos(ihit) * sscin_tdc_to_time
            postime(ihit) = postime(ihit) - sscin_pos_phc_coeff(ihit) * 
     1           sqrt(max(0.,(pos_ph(ihit)/sscin_minph-1.)))
            postime(ihit) = postime(ihit) - sscin_pos_time_offset(ihit)
            
            neg_ph(ihit) = float(sscin_adc_neg(ihit))
            negtime(ihit) = sscin_tdc_neg(ihit) * sscin_tdc_to_time
            negtime(ihit) = negtime(ihit) - sscin_neg_phc_coeff(ihit) * 
     1           sqrt(max(0.,(neg_ph(ihit)/sscin_minph-1.)))
            negtime(ihit) = negtime(ihit) - sscin_neg_time_offset(ihit)

*  Find hit position.  If postime larger, then hit was nearer negative side.
            dist_from_center = 0.5*(negtime(ihit) - postime(ihit))
     1           * sscin_vel_light(ihit)
            scint_center = (sscin_pos_coord(ihit)+sscin_neg_coord(ihit))
     $           /2.
            hit_position = scint_center + dist_from_center
            sscin_dec_hit_coord(ihit) = hit_position
 
*     Get corrected time.
            pos_path = abs(sscin_pos_coord(ihit) - hit_position)
            neg_path = abs(sscin_neg_coord(ihit) - hit_position)
            postime(ihit) = postime(ihit) - pos_path
     $           /sscin_vel_light(ihit)
            negtime(ihit) = negtime(ihit) - neg_path
     $           /sscin_vel_light(ihit)

            sscin_cor_time(ihit) = ( postime(ihit) + negtime(ihit) )/2.
            sscin_cor_adc(ihit) = sqrt( neg_ph(ihit) * pos_ph(ihit) )

          else                          !only 1 tube fired
            sscin_cor_adc(ihit) = 0.
            sscin_cor_time(ihit) = 0.   !not a very good 'flag', but there is
                                        ! the logical stwo_good_hits.
          endif
        enddo                           !loop over hits to find ave time,adc.

* TEMPORARY START TIME CALCULATION.  ASSUME XP=YP=0 RADIANS.  PROJECT ALL
*     TIME VALUES TO FOCAL PLANE.  USE AVERAGE FOR START TIME.
        time_num = 0
        time_sum = 0.
        do ihit = 1 , sscin_tot_hits
          if (stwo_good_times(ihit)) then
            fptime  = sscin_cor_time(ihit) - sscin_zpos(ihit)/29.989
            if (abs(fptime-67.).le.10) then
              time_sum = time_sum + fptime
              time_num = time_num + 1
            endif
          endif
        enddo
        if (time_num.eq.0) then
          sgood_start_time = .false.
          sstart_time = 150.		!150 ns is a rough average of time dif between trig
                                        ! and wire firing.
        else
          sgood_start_time = .true.
          sstart_time = time_sum / float(time_num)
        endif


*     Dump decoded bank if sdebugprintscindec is set
      if( sdebugprintscindec .ne. 0) call s_prt_dec_scin(ABORT,errmsg)
      return
      end
