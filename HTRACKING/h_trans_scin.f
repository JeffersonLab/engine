      subroutine h_trans_scin(abort,errmsg)
*-------------------------------------------------------------------
* author: John Arrington
* created: 2/22/94
*
* h_trans_scin fills the hms_decoded_scin common block
* with track independant corrections and parameters
* needed for the drift chamber and tof analysis.
*
* modifications:
* $Log$
* Revision 1.7  1994/08/19 03:41:21  cdaq
* (SAW) Remove a debugging statement that was left in (type *,fptime)
*
* Revision 1.6  1994/08/03  14:42:39  cdaq
* (JRA) Remove outliers from start time calculation
*
* Revision 1.5  1994/08/02  20:34:00  cdaq
* (JRA) Some hacks
*
* Revision 1.4  1994/07/27  19:25:56  cdaq
* ??
*
* Revision 1.3  1994/06/29  03:43:27  cdaq
* (JRA) Add call to h_strip_scin to get good hits from HSCIN_ALL arrays
*
* Revision 1.2  1994/04/13  18:03:14  cdaq
* (DFG) 4/6       Add call to h_fill_scin_raw_hist
* (DFG) 4/5       Move call to h_prt_raw_scin to h_dump_all_raw
* (DFG) 3/24      Add h_prt_scin_raw    raw bank dump routine
*                 Add h_prt_scin_dec    decoded print routine
*                 Add test for zero hits and skip all but initialization
*                 Commented out setting abort = .true.
*                 Add ABORT and errmsg to arguements
*
* Revision 1.1  1994/02/19  06:21:37  cdaq
* Initial revision
*
*--------------------------------------------------------

      implicit none

      include 'gen_data_structures.cmn'
      include 'hms_scin_parms.cmn'
      include 'hms_scin_tof.cmn'

      logical abort
      character*1024 errmsg
      character*20 here
      parameter (here = 'h_trans_scin')

      integer*4 ihit, iset
      integer*4 setside
      integer*4 time_num
      real*4 time_sum
      real*4 fptime
      real*4 scint_center
      real*4 hit_position
      real*4 dist_from_center
      real*4 pos_path, neg_path
      real*4 pos_ph(hmax_scin_hits)     !pulse height (channels)
      real*4 neg_ph(hmax_scin_hits)
      real*4 postime(hmax_scin_hits)
      real*4 negtime(hmax_scin_hits)
      real*4 mintime

      save
      
      abort = .false.

**    Find scintillators with real hits (good TDC values)
      call h_strip_scin(abort,errmsg)
      
**    Initialize track-independant quantaties.
      call h_tof_init(abort,errmsg)
*     
*     
      if (abort) then
        call g_prepend(here,errmsg)
        return
      endif
      hgood_start_time = .false.
      hgood_start_plane = .false.
      if( hscin_tot_hits .gt. 0)  then
*     histogram raw scin
        call h_fill_scin_raw_hist(abort,errmsg)
        if (abort) then
          call g_prepend(here,errmsg)
          return
        endif
      endif        
*     
*     test for at least one valid hit
      if( hscin_tot_hits .gt. 0)  then
        do ihit = 1 , hscin_tot_hits
          htwo_good_times(ihit) = .false.
        enddo
*     
**    Find tube that gave the trigger.
*     For now, just take lowest TDC value (good enough for GEANT
*     simulation).  Probably want to convert to time and correct
*     for tube offset to find tube that defined timing.  Don't
*     want to do pulse height correction or position correction,
*     so don't need to do these corrections first.

        mintime = 1.0e+20               !initialize to large value.
        do ihit = 1 , hscin_tot_hits

*     make sure have good TDC value before checking time.
          if ((hscin_tdc_pos(ihit) .ge. hscin_tdc_min) .and.
     1         (hscin_tdc_pos(ihit) .le. hscin_tdc_max)) then !good tdc

            postime(ihit) = hscin_tdc_pos(ihit) * hscin_tdc_to_time
            postime(ihit) = postime(ihit) - hscin_pos_time_offset(ihit)
            if (postime(ihit).lt.mintime) then
              mintime = postime(ihit)
              iset = ihit
              setside = 1               !1 is pos side. Bad notation. Sorry.
            endif
          endif

*     make sure have good TDC value before checking negative time.
          if ((hscin_tdc_neg(ihit) .ge. hscin_tdc_min) .and.
     1         (hscin_tdc_neg(ihit) .le. hscin_tdc_max)) then !good tdc

            negtime(ihit) = hscin_tdc_neg(ihit) * hscin_tdc_to_time
            negtime(ihit) = negtime(ihit) - hscin_neg_time_offset(ihit)

*     see if both sides had good tdc.
            if ((hscin_tdc_pos(ihit) .ge. hscin_tdc_min) .and.
     1           (hscin_tdc_pos(ihit) .le. hscin_tdc_max)) then
              htwo_good_times(ihit) = .true.
            endif

            if (negtime(ihit).lt.mintime) then
              mintime = negtime(ihit)
              iset = ihit
              setside = 2               !2 is neg side. Bad notation. Sorry.
            endif
          endif

        enddo                           !end of loop that finds tube setting time.


**    Get corrected time/adc for each scintillator hit
        do ihit = 1 , hscin_tot_hits
          if (htwo_good_times(ihit)) then !both tubes fired

*     Correct time for everything except veloc. correction in order to
*     find hit location from difference in tdc.
            pos_ph(ihit) = float(hscin_adc_pos(ihit))
            postime(ihit) = hscin_tdc_pos(ihit) * hscin_tdc_to_time
            postime(ihit) = postime(ihit) - hscin_pos_phc_coeff(ihit) * 
     1           sqrt(max(0.,(pos_ph(ihit)/hscin_minph-1.)))
            postime(ihit) = postime(ihit) - hscin_pos_time_offset(ihit)

            neg_ph(ihit) = float(hscin_adc_neg(ihit))
            negtime(ihit) = hscin_tdc_neg(ihit) * hscin_tdc_to_time
            negtime(ihit) = negtime(ihit) - hscin_neg_phc_coeff(ihit) * 
     1           sqrt(max(0.,(neg_ph(ihit)/hscin_minph-1.)))
            negtime(ihit) = negtime(ihit) - hscin_neg_time_offset(ihit)

*     Find hit position.  If postime larger, then hit was nearer negative
*     side.
            dist_from_center = 0.5*(negtime(ihit) - postime(ihit))
     1           / hscin_vel_light(ihit)
            scint_center = (hscin_pos_coord(ihit)+hscin_neg_coord(ihit))
     $           /2.
            hit_position = scint_center + dist_from_center
            
*     get corrected time.
            pos_path = abs(hscin_pos_coord(ihit) - hit_position)
            neg_path = abs(hscin_neg_coord(ihit) - hit_position)
            postime(ihit) = postime(ihit) - pos_path
     $           /hscin_vel_light(ihit)
            negtime(ihit) = negtime(ihit) - neg_path
     $           /hscin_vel_light(ihit)

            hscin_cor_time(ihit) = ( postime(ihit) + negtime(ihit) )/2.
            hscin_cor_adc(ihit) = sqrt( neg_ph(ihit) * pos_ph(ihit) )

          else                          !only 1 tube fired
            hscin_cor_adc(ihit) = 0.
            hscin_cor_time(ihit) = 0.   !not a very good 'flag', but there is
                                        ! the logical htwo_good_hits.
          endif
        enddo                           !loop over hits to find ave time,adc.

*     TEMPORARY START TIME CALCULATION.  ASSUME XP=YP=0 RADIANS.  PROJECT
*     ALL CORRECTED
*     TIME VALUES TO FOCAL PLANE.  USE AVERAGE FOR START TIME.
        time_num = 0
        time_sum = 0.
        do ihit = 1 , hscin_tot_hits
          if (htwo_good_times(ihit)) then
            fptime  = hscin_cor_time(ihit) - hscin_zpos(ihit)/29.989
***            type *,fptime
            if (abs(fptime-67.).le.10) then
              time_sum = time_sum + fptime
              time_num = time_num + 1
            endif
          endif
        enddo
        if (time_num.eq.0) then
          hgood_start_time = .false.
          hstart_time = 150.		!150 ns is a rough average of time dif between trig
                                        ! and wire firing.
        else
          hgood_start_time = .true.
          hstart_time = time_sum / float(time_num)
        endif

*     
*     Dump decoded bank if hdebugprintscindec is set
        if( hdebugprintscindec .ne. 0) then
          call h_prt_dec_scin(ABORT,errmsg)
        endif
      endif                             ! end major test on  hscin_tot_hits > 0     
      return
      end
