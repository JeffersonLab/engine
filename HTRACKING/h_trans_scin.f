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
* Added corrections for trigger time and rollover
* for F1 TDCs (should still work if go back to FASTBUS)
* 2008/09/30 P. Bosted
* $Log$
* Revision 1.21.8.6  2009/09/01 19:23:24  jones
* Initialize hbeta_notrk and hbeta(1) = -100 for every event.
*
* Revision 1.21.8.5  2008/11/17 15:59:18  cdaq
* Changed from old to new tof varaibles
*
* Revision 1.21.8.3  2008/10/28 20:57:10  cdaq
* Changed tdc_offset
*
* Revision 1.21.8.2  2008/10/27 16:34:47  cdaq
* changes for F1 TDCs
*
* Revision 1.21.8.1  2008/10/02 17:13:47  cdaq
* Added F1trig subraction
*
* Revision 1.21  2005/03/15 21:08:08  jones
* Add code to filter the scintillator tdc hits and group them by time. ( P. Bosted)
*
* Revision 1.20  2002/10/02 13:42:43  saw
* Check that user hists are defined before filling
*
* Revision 1.19  1999/06/10 16:53:04  csa
* (JRA) Cosmetic changes
*
* Revision 1.18  1996/04/30 12:46:50  saw
* (JRA) Clean up
*
* Revision 1.17  1996/01/16 21:35:37  cdaq
* (JRA) Misc. fixes.
*
* Revision 1.16  1995/05/22 19:39:33  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.15  1995/05/17  14:12:13  cdaq
* (JRA) Add hscintimes user histogram
*
* Revision 1.14  1995/05/11  19:11:45  cdaq
* (JRA) Replace hardwired TDC offsets with ctp variables.
*
* Revision 1.13  1995/02/23  13:23:49  cdaq
* (JRA) Add a calculation of beta without finding a track
*
* Revision 1.12  1995/02/02  16:36:22  cdaq
* (JRA) minph variables now per pmt, hscin_adc_pos/neg change to floats
*
* Revision 1.11  1995/01/31  21:51:13  cdaq
* (JRA) Put hit in center of scint if only one tube fired
*
* Revision 1.10  1995/01/27  19:28:48  cdaq
* (JRA) Adjust start time cut to be hardwired for December 94 run.  Need a
*       better way to do this eventually.
*
* Revision 1.9  1995/01/18  16:28:08  cdaq
* (SAW) Catch negative ADC values in argument of square root
*
* Revision 1.8  1994/09/13  21:40:06  cdaq
* (JRA) remove obsolete code, fix check for 2 hits, fix hit position
*
* Revision 1.7  1994/08/19  03:41:21  cdaq
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

      include 'hms_data_structures.cmn'
      include 'hms_scin_parms.cmn'
      include 'hms_scin_tof.cmn'
      include 'hms_id_histid.cmn'
      include 'f1trigger_data_structures.cmn'

      logical abort
      character*1024 errmsg
      character*20 here
      parameter (here = 'h_trans_scin')

      integer*4 dumtrk
      parameter (dumtrk=1)
      integer*4 ihit, plane
      integer*4 time_num
      real*4 time_sum
      real*4 fptime
      real*4 scint_center
      real*4 hit_position
      real*4 dist_from_center,hscin_vel_light
      real*4 pos_path, neg_path
      real*4 pos_ph(hmax_scin_hits)     !pulse height (channels)
      real*4 neg_ph(hmax_scin_hits)
      real*4 postime(hmax_scin_hits)
      real*4 negtime(hmax_scin_hits)
      logical goodtime(hnum_scin_planes)
      integer timehist(200),i,j,jmax,maxhit,nfound
      real*4 time_pos(1000),time_neg(1000),tmin,time_tolerance
      logical keep_pos(1000),keep_neg(1000),first/.true./
      integer rawtime, corrtime
      save
      
      abort = .false.
        hbeta_notrk = -100.
        hbeta(1) = -100.


! Correct for trigger time.
! If NOT using F1 TDC's, comment this section out
c      write(37,'(/1x,''alltothits='',i3)') 
c     >  hscin_all_tot_hits
      do ihit = 1 , hscin_all_tot_hits 
       rawtime = hscin_all_tdc_pos(ihit)
c       postime(ihit) = rawtime
       if(rawtime.ge.0) then
        call CORRECT_RAW_TIME_HMS(rawtime,corrtime)
        hscin_all_tdc_pos(ihit) = corrtime
       endif
       rawtime = hscin_all_tdc_neg(ihit)
c       negtime(ihit) = rawtime
       if(rawtime.ge.0) then
        call CORRECT_RAW_TIME_HMS(rawtime,corrtime)
        hscin_all_tdc_neg(ihit) = corrtime
       endif
c       if(postime(ihit).ge.0. .or.
c     >    negtime(ihit).ge.0.) 
c     >  write(37,'(2i3,f6.0,i5,f6.0,3i5)') 
c     >   hscin_all_plane_num(ihit),
c     >   hscin_all_counter_num(ihit),
c     >   postime(ihit),hscin_all_tdc_pos(ihit),
c     >   postime(ihit),hscin_all_tdc_neg(ihit),
c     >   hscin_all_adc_pos(ihit),
c     >   hscin_all_adc_neg(ihit)
      enddo


**    Find scintillators with real hits (good TDC values)
      call h_strip_scin(abort,errmsg)
      if (abort) then
        call g_prepend(here,errmsg)
        return
      endif
      
**    Initialize track-independant quantaties.
      call h_tof_init(abort,errmsg)
      if (abort) then
        call g_prepend(here,errmsg)
        return
      endif

      hgood_start_time = .false.
      if( hscin_tot_hits .gt. 0)  then
** Histogram raw scin
        call h_fill_scin_raw_hist(abort,errmsg)
        if (abort) then
          call g_prepend(here,errmsg)
          return
        endif
      endif        
     
** Return if no valid hits.
      if( hscin_tot_hits .le. 0) return

! Calculate all corrected hit times and histogram
! This uses a copy of code below. Results are save in time_pos,neg
! including the z-pos. correction assuming nominal value of betap
! Code is currently hard-wired to look for a peak in the
! range of 0 to 100 nsec, with a group of times that all
! agree withing a time_tolerance of time_tolerance nsec. The normal
! peak position appears to be around 35 nsec (SOS0 or 31 nsec (HMS)
! NOTE: if want to find farticles with beta different than
!       reference particle, need to make sure this is big enough
!       to accomodate difference in TOF for other particles
! Default value in case user hasnt definedd something reasonable
      time_tolerance=3.0
      if(htof_tolerance.gt.0.5.and.htof_tolerance.lt.10000.) then
         time_tolerance=htof_tolerance
      endif
      if(first) then
         first=.false.
         write(*,'(//1x,''USING '',f8.2,'' NSEC WINDOW FOR'',
     >        ''  HMS FP NO_TRACK  CALCULATIONS'')') time_tolerance
         write(*,'(//)')
      endif
      nfound = 0
      do j=1,200
         timehist(j)=0
      enddo
      do ihit = 1 , hscin_tot_hits
        i=min(1000,ihit)
        time_pos(i)=-99.
        time_neg(i)=-99.
        keep_pos(i)=.false.
        keep_neg(i)=.false.
        if ((hscin_tdc_pos(ihit) .ge. hscin_tdc_min) .and.
     1      (hscin_tdc_pos(ihit) .le. hscin_tdc_max) .and.
     2      (hscin_tdc_neg(ihit) .ge. hscin_tdc_min) .and.
     3      (hscin_tdc_neg(ihit) .le. hscin_tdc_max)) then

          pos_ph(ihit) = hscin_adc_pos(ihit)
          postime(ihit) = hscin_tdc_pos(ihit) * hscin_tdc_to_time
          postime(ihit) = postime(ihit) - 
     >        hscin_pos_invadc_offset(ihit) -
     >        hscin_pos_invadc_adc(ihit)/
     >        sqrt(max(20.,pos_ph(ihit)))
          neg_ph(ihit) = hscin_adc_neg(ihit)
          negtime(ihit) = hscin_tdc_neg(ihit) * hscin_tdc_to_time
          negtime(ihit) = negtime(ihit) - 
     >        hscin_neg_invadc_offset(ihit) -
     >        hscin_neg_invadc_adc(ihit)/
     >        sqrt(max(20.,neg_ph(ihit)))
          
* Find hit position.  If postime larger, then hit was nearer negative side.
c changed to here use a fixed velocity of 15 cm/nsec
          hscin_vel_light=15.
          dist_from_center = 0.5*(negtime(ihit) - postime(ihit))
     1         * hscin_vel_light
          scint_center = (hscin_pos_coord(ihit)+hscin_neg_coord(ihit))/2.
          hit_position = scint_center + dist_from_center
          hit_position = min(hscin_pos_coord(ihit),hit_position)
          hit_position = max(hscin_neg_coord(ihit),hit_position)
          hscin_dec_hit_coord(ihit) = hit_position

*     Get corrected time.
          pos_path = hscin_pos_coord(ihit) - hit_position
          neg_path = hit_position - hscin_neg_coord(ihit)
          postime(ihit) = postime(ihit) - 
     >        pos_path/hscin_pos_invadc_linear(ihit)
            negtime(ihit) = negtime(ihit) - 
     >        neg_path/hscin_neg_invadc_linear(ihit)
          time_pos(i)  = postime(ihit) - 
     >        hscin_zpos(ihit) / (29.979*hbeta_pcent)
          time_neg(i)  = negtime(ihit) - 
     >        hscin_zpos(ihit) / (29.979*hbeta_pcent)
          nfound = nfound + 1
          do j=1,200
            tmin = 0.5*float(j)                
            if(time_pos(i) .gt. tmin .and.
     >         time_pos(i) .lt. tmin + time_tolerance) 
     >         timehist(j) = timehist(j) + 1
          enddo
          nfound = nfound + 1
          do j=1,200
            tmin = 0.5*float(j)                
            if(time_neg(i) .gt. tmin .and.
     >         time_neg(i) .lt. tmin + time_tolerance) 
     >         timehist(j) = timehist(j) + 1
          enddo
        endif
      enddo
! Find bin with most hits
        jmax=0
        maxhit=0
        do j=1,200
          if(timehist(j) .gt. maxhit) then
            jmax = j
            maxhit = timehist(j)
          endif
        enddo
        if(jmax.gt.0) then
          tmin = 0.5*float(jmax) 
          do ihit = 1 , hscin_tot_hits
            i=min(1000,ihit)
            if(time_pos(i) .gt. tmin .and.
     >         time_pos(i) .lt. tmin + time_tolerance) then
               keep_pos(i) = .true.
            endif
            if(time_neg(i) .gt. tmin .and.
     >         time_neg(i) .lt. tmin + time_tolerance) then
               keep_neg(i) = .true.
            endif
          enddo
        endif

! Resume regular tof code, now using time filer from above
** Check for two good TDC values.
      do ihit = 1 , hscin_tot_hits
        if ((hscin_tdc_pos(ihit) .ge. hscin_tdc_min) .and.
     1       (hscin_tdc_pos(ihit) .le. hscin_tdc_max) .and.
     2       (hscin_tdc_neg(ihit) .ge. hscin_tdc_min) .and.
     3       (hscin_tdc_neg(ihit) .le. hscin_tdc_max).and.
     4       keep_pos(ihit).and.keep_neg(ihit)) then
          htwo_good_times(ihit) = .true.
        else
          htwo_good_times(ihit) = .false.
        endif
      enddo                             !end of loop that finds tube setting time.

**    Get corrected time/adc for each scintillator hit
      do ihit = 1 , hscin_tot_hits
        if (htwo_good_times(ihit)) then !both tubes fired

*     Correct time for everything except veloc. correction in order to
*     find hit location from difference in tdc.
          pos_ph(ihit) = hscin_adc_pos(ihit)
          postime(ihit) = hscin_tdc_pos(ihit) * hscin_tdc_to_time
          postime(ihit) = postime(ihit) - 
     >        hscin_pos_invadc_offset(ihit) -
     >        hscin_pos_invadc_adc(ihit)/
     >        sqrt(max(20.,pos_ph(ihit)))
          neg_ph(ihit) = hscin_adc_neg(ihit)
          negtime(ihit) = hscin_tdc_neg(ihit) * hscin_tdc_to_time
          negtime(ihit) = negtime(ihit) - 
     >        hscin_neg_invadc_offset(ihit) -
     >        hscin_neg_invadc_adc(ihit)/
     >        sqrt(max(20.,neg_ph(ihit)))
* Find hit position.  If postime larger, then hit was nearer negative side.
          dist_from_center = 0.5*(negtime(ihit) - postime(ihit))
     1         * hscin_vel_light
          scint_center = (hscin_pos_coord(ihit)+hscin_neg_coord(ihit))/2.
          hit_position = scint_center + dist_from_center
          hit_position = min(hscin_pos_coord(ihit),hit_position)
          hit_position = max(hscin_neg_coord(ihit),hit_position)
          hscin_dec_hit_coord(ihit) = hit_position

*     Get corrected time.
          pos_path = hscin_pos_coord(ihit) - hit_position
          neg_path = hit_position - hscin_neg_coord(ihit)
          postime(ihit) = postime(ihit) - 
     >        pos_path/hscin_pos_invadc_linear(ihit)
          negtime(ihit) = negtime(ihit) - 
     >        neg_path/hscin_neg_invadc_linear(ihit)
          hscin_cor_time(ihit) = ( postime(ihit) + negtime(ihit) )/2.

        else                            !only 1 tube fired
          hscin_dec_hit_coord(ihit) = 0.
          hscin_cor_time(ihit) = 0.     !not a very good 'flag', but there is
                                        ! the logical htwo_good_hits.
        endif
      enddo                             !loop over hits to find ave time,adc.

* start time calculation.  assume xp=yp=0 radians.  project all
* time values to focal plane.  use average for start time.
      time_num = 0
      time_sum = 0.
      do ihit = 1 , hscin_tot_hits
        if (htwo_good_times(ihit)) then
          fptime  = hscin_cor_time(ihit) - hscin_zpos(ihit)/(29.979*hbeta_pcent)
          if(hidscinalltimes.gt.0) call hf1(hidscinalltimes,fptime,1.)
          if (abs(fptime-hstart_time_center).le.hstart_time_slop) then
            time_sum = time_sum + fptime
            time_num = time_num + 1
          endif
        endif
      enddo
      if (time_num.eq.0) then
        hgood_start_time = .false.
        hstart_time = hstart_time_center
      else
        hgood_start_time = .true.
        hstart_time = time_sum / float(time_num)
      endif


*     Dump decoded bank if hdebugprintscindec is set
      if( hdebugprintscindec .ne. 0) call h_prt_dec_scin(ABORT,errmsg)

*    Calculate beta without finding track (to reject cosmics for efficiencies)
*    using tube only if both pmts fired since the velocity correction is
*    position (track) dependant.
*    Fitting routine fills variables assuming track=1.

      do plane = 1 , hnum_scin_planes
        goodtime(plane)=.false.
      enddo

      do ihit = 1 , hscin_tot_hits
        hgood_scin_time(dumtrk,ihit)=.false.
        if (htwo_good_times(ihit)) then !require 2 tubes to be track indep.
          if (abs(fptime-hstart_time_center).le.hstart_time_slop) then ! throw out outliers.
            hgood_scin_time(dumtrk,ihit)=.true.
            hscin_time(ihit)=hscin_cor_time(ihit)
            hscin_sigma(ihit)=sqrt(hscin_neg_sigma(ihit)**2 +
     &           hscin_pos_sigma(ihit)**2)/2.
            goodtime(hscin_plane_num(ihit))=.true.
          endif
        endif
      enddo
      

*    Fit beta if there are enough time measurements (one upper, one lower)
      if ((goodtime(1) .or. goodtime(2)) .and.
     1    (goodtime(3) .or. goodtime(4))) then

        hxp_fp(dumtrk)=0.0
        hyp_fp(dumtrk)=0.0
        call h_tof_fit(abort,errmsg,dumtrk) !fit velocity of particle
        if (abort) then
          call g_prepend(here,errmsg)
          return
        endif
        hbeta_notrk = hbeta(dumtrk)
        hbeta_chisq_notrk = hbeta_chisq(dumtrk)
      else
        hbeta_notrk = 0.
        hbeta_chisq_notrk = -1.
      endif

      return
      end

c subtract trigger times
      SUBROUTINE CORRECT_RAW_TIME_HMS(RAW_TDC,CORRECTED_TDC)
      IMPLICIT NONE
      include 'hms_data_structures.cmn'
      include 'f1trigger_data_structures.cmn'
c
c     Function arguments are RAW_TDC -raw TDC value
c     and CORRECTED_TDC -Corrected by Trigger time and rolover time 
c     MAKE SURE TO Include correct parameter files
c
c
      integer*4 RAW_TDC, CORRECTED_TDC,tdc_offset
      integer*4 f1trigmax,nprint
      logical roll
      save

c hard-wired to make TDC spectra centered on 1500
c or so. This is for F1 TDCs
      tdc_offset=1200

C correct for trigger time. If using F1 TDC's, make
C sure rolloever values are defined in parameter file
      if(HMS_TRIGGER_COUNTER.eq.0.or.
     >   HMS_TRIGGER_COUNTER.gt.10) then
        write(6,'(''error, HMS_TRIGGER_COUNTER='',i4)')
     >    HMS_TRIGGER_COUNTER
c set to useful default
        HMS_TRIGGER_COUNTER=1
      endif
      if(HMS_TRIGGER_WINDOW.lt.100) then
        write(6,'(''error, HMS_TRIGGER_WINDOW='',i5)')
     >   HMS_TRIGGER_WINDOW
         HMS_TRIGGER_WINDOW = 2000
      endif

c find largest value of trigger time, to check rollover
      if(TRIGGER_F1_START_TDC_COUNTER(
     >        HMS_TRIGGER_COUNTER) .gt.f1trigmax) then
        write(6,'('' HMS trigger time max='',i8)')
     >  TRIGGER_F1_START_TDC_COUNTER(
     >        HMS_TRIGGER_COUNTER)
        f1trigmax = 
     >  TRIGGER_F1_START_TDC_COUNTER(
     >        HMS_TRIGGER_COUNTER)
      endif

c subtract trigger time
c and add a constant so generally in range
c of 1000 to 3000
      CORRECTED_TDC =  RAW_TDC - 
     >  TRIGGER_F1_START_TDC_COUNTER(HMS_TRIGGER_COUNTER)
     >  + tdc_offset
c
c     Taking care of ROLOVER For 
c     
      roll = .false.
c This happens if scin. TDC rolled over
      if(CORRECTED_TDC.lt.-30000) then
        CORRECTED_TDC = CORRECTED_TDC + 
     >   TRIGGER_F1_ROLOVER(HMS_TRIGGER_COUNTER)
        roll = .true.
      endif

c This happens if trigger TDC rolled over
      if(CORRECTED_TDC.gt.30000) then
        CORRECTED_TDC = CORRECTED_TDC - 
     >   TRIGGER_F1_ROLOVER(HMS_TRIGGER_COUNTER)
        roll=.true.
      endif

      if(nprint.lt.0.and.roll) then
       write(6,'(''dbg hscin'',4i8)') 
     >  RAW_TDC, CORRECTED_TDC,
     >  TRIGGER_F1_START_TDC_COUNTER(
     >  HMS_TRIGGER_COUNTER)
       nprint = nprint+1
      endif

      return
      end
