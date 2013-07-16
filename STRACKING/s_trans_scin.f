      subroutine s_trans_scin(abort,errmsg)
*--------------------------------------------------------
* author: John Arrington
* created: 2/22/94
*
* s_trans_scin fills the sos_decoded_scin common block
* with track independant corrections and parameters
* needed for the drift chamber and tof analysis.
*
* $Log: s_trans_scin.f,v $
* Revision 1.13.16.1  2005/03/15 21:19:27  jones
* Add code to filter the scintillator tdc hits and group them by time. ( P. Bosted)
*
* Revision 1.14  2005/03/15 21:13:09  jones
* Add code to filter the scintillator tdc hits and group them by time. ( P. Bosted)
*
* Revision 1.13  1999/06/10 16:57:58  csa
* (JRA) Cosmetic changes
*
* Revision 1.12  1996/04/30 17:15:39  saw
* (JRA) Cleanup
*
* Revision 1.11  1996/01/17 18:21:56  cdaq
* (JRA) Misc. fixes.
*
* Revision 1.10  1995/05/22 19:46:03  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.9  1995/05/17  16:48:22  cdaq
* (JRA) Add hscintimes user histogram
*
* Revision 1.8  1995/05/11  15:10:59  cdaq
* (JRA) Replace hardwired TDC offsets with ctp variables.  Fix latent hmsism.
*
* Revision 1.7  1995/04/06  19:52:59  cdaq
* (JRA) Change hardwired TDC offset to 100
*
* Revision 1.6  1995/02/23  13:25:28  cdaq
* (JRA) Add a calculation of beta without finding a track
*
* Revision 1.5  1995/01/18  21:00:24  cdaq
* (SAW) Catch negative ADC values in argument of square root
*
* Revision 1.4  1994/11/23  15:08:24  cdaq
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

      include 'sos_data_structures.cmn'
      include 'sos_scin_parms.cmn'
      include 'sos_scin_tof.cmn'
      include 'sos_id_histid.cmn'

      logical abort
      character*1024 errmsg
      character*20 here
      parameter (here = 's_trans_scin')

      integer*4 dumtrk
      parameter (dumtrk=1)
      integer*4 ihit, plane
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
      logical goodtime(snum_scin_planes)
      integer timehist(200),i,j,jmax,maxhit,nfound
      real*4 time_pos(1000),time_neg(1000),tmin,time_tolerance
      logical keep_pos(1000),keep_neg(1000),first/.true./
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
      if(stof_tolerance.gt.0.5.and.stof_tolerance.lt.10000.) then
         time_tolerance=stof_tolerance
      endif
      if(first) then
         first=.false.
         write(*,'(//1x,''USING '',f8.2,'' NSEC WINDOW FOR'',
     >        ''  SOS FP NO_TRACK  CALCULATIONS'')') time_tolerance
         write(*,'(//)')
      endif
      nfound = 0
      do j=1,200
         timehist(j)=0
      enddo
      do ihit = 1 , sscin_tot_hits
        i=min(1000,ihit)
        time_pos(i)=-99.
        time_neg(i)=-99.
        keep_pos(i)=.false.
        keep_neg(i)=.false.
        if ((sscin_tdc_pos(ihit) .ge. sscin_tdc_min) .and.
     1      (sscin_tdc_pos(ihit) .le. sscin_tdc_max) .and.
     2      (sscin_tdc_neg(ihit) .ge. sscin_tdc_min) .and.
     3      (sscin_tdc_neg(ihit) .le. sscin_tdc_max)) then

          pos_ph(ihit) = sscin_adc_pos(ihit)
          postime(ihit) = sscin_tdc_pos(ihit) * sscin_tdc_to_time
          postime(ihit) = postime(ihit) - sscin_pos_phc_coeff(ihit) * 
     1         sqrt(max(0.,(pos_ph(ihit)/sscin_pos_minph(ihit)-1.)))
          postime(ihit) = postime(ihit) - sscin_pos_time_offset(ihit)

          neg_ph(ihit) = sscin_adc_neg(ihit)
          negtime(ihit) = sscin_tdc_neg(ihit) * sscin_tdc_to_time
          negtime(ihit) = negtime(ihit) - sscin_neg_phc_coeff(ihit) * 
     1         sqrt(max(0.,(neg_ph(ihit)/sscin_neg_minph(ihit)-1.)))
          negtime(ihit) = negtime(ihit) - sscin_neg_time_offset(ihit)
          
* Find hit position.  If postime larger, then hit was nearer negative side.
          dist_from_center = 0.5*(negtime(ihit) - postime(ihit))
     1         * sscin_vel_light(ihit)
          scint_center = (sscin_pos_coord(ihit)+sscin_neg_coord(ihit))/2.
          hit_position = scint_center + dist_from_center
          hit_position = min(sscin_pos_coord(ihit),hit_position)
          hit_position = max(sscin_neg_coord(ihit),hit_position)
          sscin_dec_hit_coord(ihit) = hit_position

*     Get corrected time.
          pos_path = sscin_pos_coord(ihit) - hit_position
          neg_path = hit_position - sscin_neg_coord(ihit)
          postime(ihit) = postime(ihit) - pos_path/sscin_vel_light(ihit)
          negtime(ihit) = negtime(ihit) - neg_path/sscin_vel_light(ihit)
          time_pos(i)  = postime(ihit) - 
     >        sscin_zpos(ihit) / (29.979*sbeta_pcent)
          time_neg(i)  = negtime(ihit) - 
     >        sscin_zpos(ihit) / (29.979*sbeta_pcent)
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
          do ihit = 1 , sscin_tot_hits
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
      do ihit = 1 , sscin_tot_hits
        if ((sscin_tdc_pos(ihit) .ge. sscin_tdc_min) .and.
     1       (sscin_tdc_pos(ihit) .le. sscin_tdc_max) .and.
     2       (sscin_tdc_neg(ihit) .ge. sscin_tdc_min) .and.
     3       (sscin_tdc_neg(ihit) .le. sscin_tdc_max).and.
     4       keep_pos(ihit).and.keep_neg(ihit)) then
          stwo_good_times(ihit) = .true.
        else
          stwo_good_times(ihit) = .false.
        endif
      enddo                           !end of loop that finds tube setting time.

** Get corrected time/adc for each scintillator hit
      do ihit = 1 , sscin_tot_hits
        if (stwo_good_times(ihit)) then !both tubes fired

*  Correct time for everything except veloc. correction in order to
*  find hit location from difference in tdc.
          pos_ph(ihit) = sscin_adc_pos(ihit)
          postime(ihit) = sscin_tdc_pos(ihit) * sscin_tdc_to_time
          postime(ihit) = postime(ihit) - sscin_pos_phc_coeff(ihit) * 
     1           sqrt(max(0.,(pos_ph(ihit)/sscin_pos_minph(ihit)-1.)))
          postime(ihit) = postime(ihit) - sscin_pos_time_offset(ihit)
            
          neg_ph(ihit) = sscin_adc_neg(ihit)
          negtime(ihit) = sscin_tdc_neg(ihit) * sscin_tdc_to_time
          negtime(ihit) = negtime(ihit) - sscin_neg_phc_coeff(ihit) * 
     1           sqrt(max(0.,(neg_ph(ihit)/sscin_neg_minph(ihit)-1.)))
          negtime(ihit) = negtime(ihit) - sscin_neg_time_offset(ihit)

*  Find hit position.  If postime larger, then hit was nearer negative side.
          dist_from_center = 0.5*(negtime(ihit) - postime(ihit))
     1           * sscin_vel_light(ihit)
          scint_center = (sscin_pos_coord(ihit)+sscin_neg_coord(ihit))/2.
          hit_position = scint_center + dist_from_center
          hit_position = min(sscin_pos_coord(ihit),hit_position)
          hit_position = max(sscin_neg_coord(ihit),hit_position)
          sscin_dec_hit_coord(ihit) = hit_position
 
*     Get corrected time.
          pos_path = sscin_pos_coord(ihit) - hit_position
          neg_path = hit_position - sscin_neg_coord(ihit)
          postime(ihit) = postime(ihit) - pos_path/sscin_vel_light(ihit)
          negtime(ihit) = negtime(ihit) - neg_path/sscin_vel_light(ihit)
          sscin_cor_time(ihit) = ( postime(ihit) + negtime(ihit) )/2.

        else                          !only 1 tube fired
          sscin_dec_hit_coord(ihit) = 0.
          sscin_cor_time(ihit) = 0.   !not a very good 'flag', but there is
                                      ! the logical stwo_good_hits.
        endif
      enddo                           !loop over hits to find ave time,adc.

* start time calculation.  assume xp=yp=0 radians.  project all
* time values to focal plane.  use average for start time.
      time_num = 0
      time_sum = 0.
      do ihit = 1 , sscin_tot_hits
        if (stwo_good_times(ihit)) then
          fptime  = sscin_cor_time(ihit) - sscin_zpos(ihit)/(29.979*sbeta_pcent)
          call hf1(sidscinalltimes,fptime,1.)
          if (abs(fptime-sstart_time_center).le.sstart_time_slop) then
            time_sum = time_sum + fptime
            time_num = time_num + 1
          endif
        endif
      enddo
      if (time_num.eq.0) then
        sgood_start_time = .false.
        sstart_time = sstart_time_center
      else
        sgood_start_time = .true.
        sstart_time = time_sum / float(time_num)
      endif


*     Dump decoded bank if sdebugprintscindec is set
      if( sdebugprintscindec .ne. 0) call s_prt_dec_scin(ABORT,errmsg)

*    Calculate beta without finding track (to reject cosmics for efficiencies)
*    using tube only if both pmts fired since the velocity correction is
*    position (track) dependant.
*    Fitting routine fills variables assuming track=1.

      do plane = 1 , snum_scin_planes
        goodtime(plane)=.false.
      enddo

      do ihit = 1 , sscin_tot_hits
        sgood_scin_time(dumtrk,ihit)=.false.
        if (stwo_good_times(ihit)) then !require 2 tubes to be track indep.
          if (abs(fptime-sstart_time_center).le.sstart_time_slop) then !throw out outliers.
            sgood_scin_time(dumtrk,ihit)=.true.
            sscin_time(ihit)=sscin_cor_time(ihit)
            sscin_sigma(ihit)=sqrt(sscin_neg_sigma(ihit)**2 +
     &           sscin_pos_sigma(ihit)**2)/2.
            goodtime(sscin_plane_num(ihit))=.true.
          endif
        endif
      enddo


*    Fit beta if there are enough time measurements (one upper, one lower)
      if ((goodtime(1) .or. goodtime(2)) .and.
     1    (goodtime(3) .or. goodtime(4))) then

        sxp_fp(dumtrk)=0.0
        syp_fp(dumtrk)=0.0
        call s_tof_fit(abort,errmsg,dumtrk) !fit velocity of particle
        if (abort) then
          call g_prepend(here,errmsg)
          return
        endif
        sbeta_notrk = sbeta(dumtrk)
        sbeta_chisq_notrk = sbeta_chisq(dumtrk)
      else
        sbeta_notrk = 0.
        sbeta_chisq_notrk = -1.
      endif

      return
      end

