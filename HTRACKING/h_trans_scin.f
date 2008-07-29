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
* Revision 1.21.6.3  2008/07/29 16:24:52  puckett
* added correction of start time for trig. time upon failure to get start time from scint hits
*
* Revision 1.21.6.2  2008/04/23 18:02:36  cdaq
* *** empty log message ***
*
* Revision 1.21.6.1  2008/01/25 19:28:34  cdaq
* fixed HSTART calculation
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
* 1/23/08 pyb added "invadc" option, and also
*             don't use hits is sigma>2 nsec
*             (this is a way to turn off S0 in Gep-III)
*--------------------------------------------------------

      implicit none

      include 'hms_data_structures.cmn'
      include 'hms_scin_parms.cmn'
      include 'hms_scin_tof.cmn'
      include 'hms_id_histid.cmn'
      include 'gep_data_structures.cmn'
      include 'gen_event_info.cmn'

      logical abort
      character*1024 errmsg
      character*20 here
      parameter (here = 'h_trans_scin')

      integer*4 dumtrk
      parameter (dumtrk=1)
      integer*4 ihit, plane
      integer*4 time_num
*     ajp 4/11/08
      integer jhit,alli,allj,ajpxcntr,ajpycntr
      logical firstajp
      real*4 minfpdiff,fpdiff,ajpxcoord,ajpycoord,ajpmeanfptime,sigmahitpos1
      real*4 sigmahitpos2,sigmatdiff,sigmahitpos,sigmat1,sigmat2
      integer*4 nstart_plane(2) ! number of start time hits on S1X and S1Y, respectively
      real*4 ajpxcntrzpos,ajpycntrzpos
*     ajp 4/11/08
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
      logical goodtime(hnum_scin_planes)
      integer timehist(200),i,j,jmax,maxhit,nfound,ncall/0/
      real*4 time_pos(1000),time_neg(1000),tmin,time_tolerance
      logical keep_pos(1000),keep_neg(1000),first/.true./
      save
      
      abort = .false.

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
! 1/23/08 pyb increased default from 3.0 to 10.0
      time_tolerance=10.0
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
     3      (hscin_tdc_neg(ihit) .le. hscin_tdc_max) .and.   
c 1/23/08 pyb added these checks
     >      hscin_pos_sigma(ihit) .lt. 2.0 .and.
     >      hscin_neg_sigma(ihit) .lt. 2.0) then


          pos_ph(ihit) = hscin_adc_pos(ihit)
          postime(ihit) = hscin_tdc_pos(ihit) * hscin_tdc_to_time
c corrections for ADC and offsets
          if(htofusinginvadc.eq.1) then
            postime(ihit) = postime(ihit) - 
     >        hscin_pos_invadc_offset(ihit) -
     >        hscin_pos_invadc_adc(ihit)/
     >        sqrt(max(20.,pos_ph(ihit)))
          else
            postime(ihit) = postime(ihit) - 
     >        hscin_pos_phc_coeff(ihit) * 
     1        sqrt(max(0.,(pos_ph(ihit)/
     >          hscin_pos_minph(ihit)-1.)))
            postime(ihit) = postime(ihit) - 
     >        hscin_pos_time_offset(ihit)
          endif
          neg_ph(ihit) = hscin_adc_neg(ihit)
          negtime(ihit) = hscin_tdc_neg(ihit) * hscin_tdc_to_time
          if(htofusinginvadc.eq.1) then
            negtime(ihit) = negtime(ihit) - 
     >        hscin_neg_invadc_offset(ihit) -
     >        hscin_neg_invadc_adc(ihit)/
     >        sqrt(max(20.,neg_ph(ihit)))
          else
            negtime(ihit) = negtime(ihit) - 
     >         hscin_neg_phc_coeff(ihit) * 
     >         sqrt(max(0.,(neg_ph(ihit)/
     >      hscin_neg_minph(ihit)-1.)))
            negtime(ihit) = negtime(ihit) - 
     >      hscin_neg_time_offset(ihit)
          endif

* Find hit position.  If postime larger, then hit was nearer negative side.
          dist_from_center = 0.5*(negtime(ihit) - postime(ihit))
     1         * hscin_vel_light(ihit)
          scint_center = (hscin_pos_coord(ihit)+hscin_neg_coord(ihit))/2.
          hit_position = scint_center + dist_from_center
          hit_position = min(hscin_pos_coord(ihit),hit_position)
          hit_position = max(hscin_neg_coord(ihit),hit_position)
          hscin_dec_hit_coord(ihit) = hit_position

*     Get corrected time.
          pos_path = hscin_pos_coord(ihit) - hit_position
          neg_path = hit_position - hscin_neg_coord(ihit)
          if(htofusinginvadc.eq.1) then
            postime(ihit) = postime(ihit) - 
     >        pos_path/hscin_pos_invadc_linear(ihit)
            negtime(ihit) = negtime(ihit) - 
     >        neg_path/hscin_neg_invadc_linear(ihit)
          else
            postime(ihit) = postime(ihit) - 
     >        pos_path/hscin_vel_light(ihit)
            negtime(ihit) = negtime(ihit) - 
     >       neg_path/hscin_vel_light(ihit)
          endif
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
c 1/23/08 pyb added these checks
     >      hscin_pos_sigma(ihit) .lt. 2.0 .and.
     >      hscin_neg_sigma(ihit) .lt. 2.0 .and.
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
          if(htofusinginvadc.eq.1) then
            postime(ihit) = postime(ihit) - 
     >        hscin_pos_invadc_offset(ihit) -
     >        hscin_pos_invadc_adc(ihit)/
     >        sqrt(max(20.,pos_ph(ihit)))
          else
            postime(ihit) = postime(ihit) - 
     >        hscin_pos_phc_coeff(ihit) * 
     >        sqrt(max(0.,(pos_ph(ihit)/
     >        hscin_pos_minph(ihit)-1.)))
            postime(ihit) = postime(ihit) - 
     >      hscin_pos_time_offset(ihit)
          endif
          neg_ph(ihit) = hscin_adc_neg(ihit)
          negtime(ihit) = hscin_tdc_neg(ihit) * hscin_tdc_to_time
          if(htofusinginvadc.eq.1) then
            negtime(ihit) = negtime(ihit) - 
     >        hscin_neg_invadc_offset(ihit) -
     >        hscin_neg_invadc_adc(ihit)/
     >        sqrt(max(20.,neg_ph(ihit)))
          else
            negtime(ihit) = negtime(ihit) - 
     >        hscin_neg_phc_coeff(ihit) * 
     1        sqrt(max(0.,(neg_ph(ihit)/
     >        hscin_neg_minph(ihit)-1.)))
            negtime(ihit) = negtime(ihit) - 
     >      hscin_neg_time_offset(ihit)
          endif

* Find hit position.  If postime larger, then hit was nearer negative side.
          dist_from_center = 0.5*(negtime(ihit) - postime(ihit))
     1         * hscin_vel_light(ihit)
          scint_center = (hscin_pos_coord(ihit)+hscin_neg_coord(ihit))/2.
          hit_position = scint_center + dist_from_center
          hit_position = min(hscin_pos_coord(ihit),hit_position)
          hit_position = max(hscin_neg_coord(ihit),hit_position)
          hscin_dec_hit_coord(ihit) = hit_position

*     Get corrected time.
          pos_path = hscin_pos_coord(ihit) - hit_position
          neg_path = hit_position - hscin_neg_coord(ihit)
          if(htofusinginvadc.eq.1) then
            postime(ihit) = postime(ihit) - 
     >        pos_path/hscin_pos_invadc_linear(ihit)
            negtime(ihit) = negtime(ihit) - 
     >        neg_path/hscin_neg_invadc_linear(ihit)
          else
            postime(ihit) = postime(ihit) - 
     >        pos_path/hscin_vel_light(ihit)
            negtime(ihit) = negtime(ihit) - 
     >       neg_path/hscin_vel_light(ihit)
          endif
          hscin_cor_time(ihit) = ( postime(ihit) + 
     >      negtime(ihit) )/2.

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
* ajp 04/11/08
      nstart_plane(1) = 0
      nstart_plane(2) = 0
* ajp 04/11/08
      do ihit = 1 , hscin_tot_hits
        if (htwo_good_times(ihit)) then
          fptime  = hscin_cor_time(ihit) - 
     >      hscin_zpos(ihit)/(29.979*hbeta_pcent)
          if(hidscinalltimes.gt.0) call hf1(hidscinalltimes,fptime,1.)
          if (abs(fptime-hstart_time_center).le.hstart_time_slop) then
            time_sum = time_sum + fptime
            time_num = time_num + 1

*     ajp 4/11/08
            if(hscin_plane_num(ihit).eq.1.or.hscin_plane_num(ihit).eq.2) 
     $           then
               nstart_plane(hscin_plane_num(ihit)) = 
     $              nstart_plane(hscin_plane_num(ihit)) + 1
            endif
            
            hgood_start_time_hitnum(time_num) = ihit
*     ajp 04/11/08
          endif
        endif
      enddo
      ajpnstarttimehits = time_num
      if (time_num.eq.0) then
        hgood_start_time = .false.
        hstart_time = hstart_time_center

        if(h_correct_start_time.ne.0.and.gen_event_type.eq.6) then !correct start time for trigger time:
           if(gen_event_trigtype(4).eq.1.and.gen_event_trigtype(5).eq.0) 
     $          then
              if(ntrigh1.gt.0) then
                 hstart_time=hstart_time+gep_htrig_t0(1)-gep_h1time(1)
              endif
           endif
           
           if(gen_event_trigtype(5).eq.1)  then
              if(ntrigh2.gt.0) then
                 hstart_time=hstart_time+gep_htrig_t0(2)-gep_h2time(1)
              endif
           endif
        endif
      else
         hgood_start_time = .true.
         hstart_time = time_sum / float(time_num)
         if(ncall.lt.30) then
            write(*,'(''hstart'',i4,2f8.3)') time_num,
     >           hstart_time,  hstart_time_center
            ncall = ncall + 1
         endif
      endif

cajp071008 -- experiment with calculating hstart_time just from trigger time

c$$$      hstart_time = hstart_time_center
c$$$
c$$$      if(gen_event_trigtype(4).eq.1.and.gen_event_trigtype(5).eq.0) 
c$$$     $     then
c$$$         if(ntrigh1.gt.0) then
c$$$            hstart_time=hstart_time+gep_htrig_t0(1)-gep_h1time(1)
c$$$         endif
c$$$      endif
c$$$      
c$$$      if(gen_event_trigtype(5).eq.1)  then
c$$$         if(ntrigh2.gt.0) then
c$$$            hstart_time=hstart_time+gep_htrig_t0(2)-gep_h2time(1)
c$$$         endif
c$$$      endif
cajp071008


*     this code added by ajp 04/11/08 to calculate xy position of track from scintillators
*     BEFORE drift chamber tracking--to help with high rate for GEp-III
      
      if(time_num.ge.2.and.nstart_plane(1).ge.1.and.nstart_plane(2).ge.1
     $     ) then ! choose the one hit from S1X and one hit from S1Y that have the best 
*     agreement on focal plane time:
         firstajp = .true.
         do ihit=1,time_num
            alli = hgood_start_time_hitnum(ihit)
            if(hscin_plane_num(alli).eq.1.or.hscin_plane_num(alli).eq.2) 
     $           then
               do jhit=ihit+1,time_num
                  allj = hgood_start_time_hitnum(jhit)
                  if(hscin_plane_num(allj).eq.1.or.hscin_plane_num(allj)
     $                 .eq.2) then
                     if(hscin_plane_num(alli).ne.hscin_plane_num(allj))
     $                    then
                        fpdiff = abs( (hscin_cor_time(alli) - 
     $                       hscin_zpos(alli)/(29.979*hbeta_pcent) ) -
     $                       (hscin_cor_time(allj) - 
     $                       hscin_zpos(allj)/(29.979*hbeta_pcent) ) )
                        if(firstajp.or.fpdiff.lt.minfpdiff) then
                           minfpdiff = fpdiff
                           firstajp = .false.
                           if(hscin_plane_num(alli).eq.1) then ! i is S1X
                              ajpxcoord = hscin_dec_hit_coord(alli)
                              ajpycoord = hscin_dec_hit_coord(allj)
                              ajpxcntr = hscin_counter_num(alli)
                              ajpycntr = hscin_counter_num(allj)
                              ajpxcntrzpos = hscin_zpos(alli)
                              ajpycntrzpos = hscin_zpos(allj)
                           else
                              ajpxcoord = hscin_dec_hit_coord(allj)
                              ajpycoord = hscin_dec_hit_coord(alli)
                              ajpxcntr = hscin_counter_num(allj)
                              ajpycntr = hscin_counter_num(alli)
                              ajpxcntrzpos = hscin_zpos(allj)
                              ajpycntrzpos = hscin_zpos(alli)
                           endif
                           ajpmeanfptime = 0.5*((hscin_cor_time(alli) - 
     $                          hscin_zpos(alli)/(29.979*hbeta_pcent) )+
     $                          (hscin_cor_time(allj) - 
     $                          hscin_zpos(allj)/(29.979*hbeta_pcent) ))
                        endif
                     endif
                  endif
               enddo
            endif
         enddo
*     now calculate crude xy coordinates from S1X, S1Y, and crude fp time
*     set htwo_good_starttime_hits to true if the decoded hit positions of the chosen
*     hits agree with each other, i.e., the x position of S1Y hit agrees with the x position of
*     S1X counter and the y position of the S1X hit agrees with the y position of the S1Y counter
         hS1X_crude_track_coord(1) = ajpxcoord ! horizontal hit position (Y) along X paddle from hit times
         hS1Y_crude_track_coord(1) = ajpycoord ! vertical hit position (X) along Y paddle from hit times
         hS1X_crude_track_coord(2) = hhodo_center(2,ajpycntr) ! horizontal center of intersecting Y paddle
         hS1Y_crude_track_coord(2) = hhodo_center(1,ajpxcntr) ! vertical center of intersecting X paddle
*     what is the coordinate resolution of the corrected hit times? if sigma ~.5 ns, then .5 ns * speed of light cm / ns
         sigmahitpos = sqrt((hhodo_pos_sigma(2,ajpycntr)*
     $        hhodo_pos_invadc_linear(2,ajpycntr))**2 + 
     $        (hhodo_neg_sigma(2,ajpycntr)*
     $        hhodo_neg_invadc_linear(2,ajpycntr))**2)
         
         sigmahitpos1 = sigmahitpos
c     weighted average of hit position measured by S1X and coordinate of intersecting S1Y paddle
         hS1X_crude_track_coord(3) = (hS1X_crude_track_coord(1)/sigmahitpos 
     $        + hS1X_crude_track_coord(2)/(hscin_1y_size/sqrt(12.)) ) / 
     $        ( 1. / sigmahitpos + 1. / (hscin_1y_size/sqrt(12.)) )

         sigmahitpos = sqrt((hhodo_pos_sigma(1,ajpxcntr)*
     $        hhodo_pos_invadc_linear(1,ajpxcntr))**2 + 
     $        (hhodo_neg_sigma(1,ajpxcntr)*
     $        hhodo_neg_invadc_linear(1,ajpxcntr))**2)

         sigmahitpos2 = sigmahitpos
c     weighted average of hit position measured by S1Y and coordinate of intersecting S1X paddle         
         hS1Y_crude_track_coord(3) = (hS1Y_crude_track_coord(1)/sigmahitpos
     $        + hS1Y_crude_track_coord(2)/(hscin_1x_size/sqrt(12.)) ) /
     $        ( 1. / sigmahitpos + 1. / (hscin_1x_size/sqrt(12.)) )
         hS1XY_crude_fptime = ajpmeanfptime
         
         sigmatdiff = 0.3 

         sigmat1 = sqrt(hhodo_pos_sigma(1,ajpxcntr)**2 + 
     $        hhodo_neg_sigma(1,ajpxcntr)**2)
         sigmat2 = sqrt(hhodo_pos_sigma(2,ajpycntr)**2 + 
     $        hhodo_neg_sigma(2,ajpycntr)**2)
         sigmatdiff = sqrt(sigmat1**2 + sigmat2**2)
         
c$$$         sigmatdiff = sqrt( (sigmahitpos1/hhodo_vel_light(2,ajpycntr) )**2 + 
c$$$     $        (sigmahitpos2/hhodo_vel_light(1,ajpxcntr) )**2 )

c$$$         if( abs(hS1X_crude_track_coord(1)-hS1X_crude_track_coord(2))
c$$$     $        .le. max(hscin_1y_size/2.,2.5*sigmahitpos1) .and.
c$$$     $        abs(hS1Y_crude_track_coord(1)-hS1Y_crude_track_coord(2))
c$$$     $        .le. max(hscin_1x_size/2.,2.5*sigmahitpos2) .and.
c$$$     $        minfpdiff.le.2.5*sigmatdiff) then
         if(minfpdiff.le.2.5*sigmatdiff) then
            htwo_good_starttime_hits = .true.
            hS1XY_crude_fptime = ajpmeanfptime
            
            hS1_crude_xtrack = hS1y_crude_track_coord(2)
            hS1_crude_ytrack = hS1x_crude_track_coord(2)

c$$$            hS1_crude_xtrack = min( hS1y_crude_track_coord(2) - hscin_1x_size/2.,
c$$$     $           max(hS1y_crude_track_coord(3),hS1y_crude_track_coord(2) + 
c$$$     $           hscin_1x_size/2.) )
c$$$            hS1_crude_ytrack = min( hS1x_crude_track_coord(2) - hscin_1y_size/2.,
c$$$     $           max(hS1x_crude_track_coord(3),hS1x_crude_track_coord(2) + 
c$$$     $           hscin_1y_size/2.) )
            hS1_crude_ztrack(1) = ajpxcntrzpos !z position of S1X counter
            hS1_crude_ztrack(2) = ajpycntrzpos !z position of S1Y counter
         else
            htwo_good_starttime_hits = .false.
         endif
      endif
      

*     end special ajp code

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
