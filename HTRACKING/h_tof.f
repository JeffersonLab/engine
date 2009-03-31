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
* Revision 1.19.6.2.2.7  2009/03/31 19:33:00  cdaq
* *** empty log message ***
*
* Revision 1.19.6.2.2.6  2008/12/11 17:11:43  cdaq
* For gfortran compiler the arguements generic functions
* must be the same type.
*
* Revision 1.19.6.2.2.5  2008/11/19 12:46:50  cdaq
* Add line to reutrn if nparam < 1
*
* Revision 1.19.6.2.2.4  2008/11/17 16:03:47  cdaq
* Fixed minor bug in printing of ADCHIST
*
* Revision 1.19.6.2.2.3  2008/11/17 16:00:22  cdaq
* Major revision to do tof calibration internatlly
*
* Revision 1.19.6.2.2.2  2008/10/28 21:03:18  cdaq
* Changed default betap to 1
*
* Revision 1.19.6.2.2.1  2008/10/27 16:34:54  cdaq
* changes for F1 TDCs
*
* Revision 1.19.6.2  2007/10/26 16:44:55  pcarter
* made the arguments to max() match data types -- GCC 4 is picky about that
*
* Revision 1.19.6.1  2007/10/24 16:37:16  cdaq
* *** empty log message ***
*
* Revision 1.19.4.3  2007/05/16 19:50:02  cdaq
* P. Bosted fixed bug in new code to dump TOF data
*
* Revision 1.19.4.2  2007/05/10 21:15:10  cdaq
* changes for writing dump file for Peter's tof code
*
* Revision 1.19.4.1  2007/05/02 21:19:30  jones
* Add new code needed for  adjusting scintillator timing using P Bosted's method.
*
* Revision 1.19  2005/03/15 21:08:08  jones
* Add code to filter the scintillator tdc hits and group them by time. ( P. Bosted)
*
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
      include 'hms_tracking.cmn'
      integer*4 hit, trk
      integer*4 plane,ind
      integer*4 hntof_pairs
      real*4 adc_ph                     !pulse height (channels)
      real*4 xhit_coord,yhit_coord
      real*4 time
      real*4 p,betap         !momentum and velocity from momentum, assuming desired mass
      real*4 path,zcor,num_fp_time
      real*4 sum_fp_time,sum_plane_time(hnum_scin_planes)
      integer*4 num_plane_time(hnum_scin_planes)
      integer timehist(200),i,j,jmax,maxhit,nfound
      real*4 time_pos(1000),time_neg(1000),tmin,time_tolerance
      logical keep_pos(1000),keep_neg(1000),first/.true./
      integer nsv, idetsv(100)
      real*8 tr0sv(100),psv(100),zcsv(100)
      real*8 tc1sv(100),adcsv(100)
      save
*
*--------------------------------------------------------
*
      ABORT= .FALSE.
      errmsg = ' '

      nsv = 0

c      if(hdumptof.eq.1) write(37,
c     > '(''ntrk,tothits''2i4,f8.3)') 
c     >  hntracks_fp,hscin_tot_hits,hscin_tdc_to_time
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
        num_fp_time = 0.
        hnum_scin_hit(trk) = 0
        hnum_pmt_hit(trk) = 0
        p = hp_tar(trk)
c if p=0, assume electrons
        if(abs(p).lt.0.1) then
          betap = 1.0
        else
          betap = p/sqrt(p*p+hpartmass*hpartmass)
c put in check for reasonable
          betap = min(1., max(0.3, betap))
        endif
        do plane = 1 , hnum_scin_planes
          hgood_plane_time(trk,plane) = .false.
          sum_plane_time(plane) = 0.
          num_plane_time(plane) = 0
        enddo

! Calculate all corrected hit times and histogram
! This uses a copy of code below. Results are save in time_pos,neg
! including the z-pos. correction assuming nominal value of betap
! Code is currently hard-wired to look for a peak in the
! range of 0 to 100 nsec, with a group of times that all
! agree withing a time_tolerance of time_tolerance nsec. The normal
! peak position appears to be around 35 nsec.
! NOTE: if want to find farticles with beta different than
!       reference particle, need to make sure this is big enough
!       to accomodate difference in TOF for other particles
! Default value in case user hasnt definedd something reasonable
        time_tolerance=20.0
        if(htof_tolerance.gt.0.5.and.htof_tolerance.lt.10000.) then
          time_tolerance=htof_tolerance
        endif
! Use wide window if dumping events for fitting
cc        if(hdumptof.eq.1) time_tolerance=25.0
        if(first) then
           first=.false.
           write(*,'(1x,''Using '',f8.2,'' nsec window for'',
     >     '' hms tof and fp calculations'')') time_tolerance
           if(hdumptof.eq.1) 
     >       write(*,'(/1x,''TOF calibration being done:'',
     >         ''  see output in HTOFCAL directory'')')
        endif
        nfound = 0
        do j=1,200
          timehist(j)=0
        enddo
        do hit = 1 , hscin_tot_hits
          i=min(1000,hit)
          time_pos(i)=-99.
          time_neg(i)=-99.
          keep_pos(i)=.false.
          keep_neg(i)=.false.
          plane = hscin_plane_num(hit)
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
          if (abs(hscin_center_coord(hit)-hscin_trans_coord(hit))
     &         .lt.(hscin_width(hit)/2.+hscin_slop(hit))) then
            if(hscin_tdc_pos(hit) .ge. hscin_tdc_min .and.  
     &          hscin_tdc_pos(hit) .le. hscin_tdc_max) then
              adc_ph = hscin_adc_pos(hit)
              path = hscin_pos_coord(hit) - hscin_long_coord(hit)
              time = hscin_tdc_pos(hit) * hscin_tdc_to_time
              time = time - (hscin_zpos(hit)/(29.979*betap) *
     &               sqrt(1. + hxp_fp(trk)**2 + hyp_fp(trk)**2))
              time_pos(i) = time - hscin_pos_invadc_offset(hit) -
     >            path / hscin_pos_invadc_linear(hit) -
     >            hscin_pos_invadc_adc(hit)/sqrt(max(20.,adc_ph))
              nfound = nfound + 1
              do j=1,200
                tmin = 0.5*float(j)                
                if(time_pos(i) .gt. tmin .and.
     >             time_pos(i) .lt. tmin + time_tolerance) 
     >            timehist(j) = timehist(j) + 1
              enddo
            endif
            if (hscin_tdc_neg(hit).ge.hscin_tdc_min .and. !good tdc
     1           hscin_tdc_neg(hit).le.hscin_tdc_max) then
              adc_ph = hscin_adc_neg(hit)
              path = hscin_long_coord(hit) - hscin_neg_coord(hit)
              time = hscin_tdc_neg(hit) * hscin_tdc_to_time
              time = time - (hscin_zpos(hit)/(29.979*betap) *
     &               sqrt(1. + hxp_fp(trk)**2 + hyp_fp(trk)**2))
              time_neg(i) = time - hscin_neg_invadc_offset(hit) -
     >            path / hscin_neg_invadc_linear(hit) -
     >            hscin_neg_invadc_adc(hit)/sqrt(max(20.,adc_ph))
              nfound = nfound + 1
              do j=1,200
                tmin = 0.5*float(j)                
                if(time_neg(i) .gt. tmin .and.
     >             time_neg(i) .lt. tmin + time_tolerance) 
     >            timehist(j) = timehist(j)+1
             enddo
            endif
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
c$$$        if(hdumptof.eq.1) then
c$$$          write(37,'(''trk='',2i3,8f8.3)') trk,jmax,
c$$$     >      hx_fp(trk),hxp_fp(trk),hy_fp(trk),hyp_fp(trk),
c$$$     >      hp_tar(trk)
c$$$        endif
        if(jmax.gt.0) then
          tmin = 0.5*float(jmax) 
          do hit = 1 , hscin_tot_hits
            i=min(1000,hit)
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
        do hit = 1 , hscin_tot_hits
          hgood_scin_time(trk,hit) = .false.
          hgood_tdc_pos(trk,hit) = .false.
          hgood_tdc_neg(trk,hit) = .false.
          hscin_time(hit) = 0
          hscin_sigma(hit) = 100.
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
     &          hscin_tdc_pos(hit) .le. hscin_tdc_max .and.
     >          keep_pos(hit)) then

**    Calculate time for each tube with a good tdc. 'pos' side first.
              hgood_tdc_pos(trk,hit) = .true.
              hntof = hntof + 1
              adc_ph = hscin_adc_pos(hit)
              path = hscin_pos_coord(hit) - hscin_long_coord(hit)

*     Convert TDC value to time, do pulse height correction, correction for
*     propogation of light thru scintillator, and offset.
              time = hscin_tdc_pos(hit) * hscin_tdc_to_time
              hscin_pos_time(hit)=time - hscin_pos_invadc_offset(hit) -
     >            path / hscin_pos_invadc_linear(hit) -
     >            hscin_pos_invadc_adc(hit)/sqrt(max(20.,adc_ph))
              zcor =  (hscin_zpos(hit)/(29.979*betap) * sqrt(1.+
     >               hxp_fp(trk)*hxp_fp(trk)+hyp_fp(trk)*hyp_fp(trk)))
              if(hdumptof.eq.1) then
c$$$                write(37,'(1x,''1'',2i3,5f10.3)') 
c$$$     >             hscin_plane_num(hit),
c$$$     >             hscin_counter_num(hit),
c$$$     >             hscin_tdc_pos(hit) * hscin_tdc_to_time,
c$$$     >             path,zcor,
c$$$     >             hscin_pos_time(hit)-zcor,adc_ph
                nsv = min(100, nsv + 1)
                idetsv(nsv) = 20 * (hscin_plane_num(hit)-1) +
     >            hscin_counter_num(hit)
                tr0sv(nsv) = hscin_tdc_pos(hit) * 
     >            hscin_tdc_to_time
                psv(nsv) = path
                zcsv(nsv) = zcor
                tc1sv(nsv) = hscin_pos_time(hit)-zcor
                adcsv(nsv) = adc_ph
              endif
            endif

**    Repeat for pmts on 'negative' side
            if (hscin_tdc_neg(hit).ge.hscin_tdc_min .and. !good tdc
     1           hscin_tdc_neg(hit).le.hscin_tdc_max.and.
     >           keep_neg(hit)) then

              hgood_tdc_neg(trk,hit) = .true.
              hntof = hntof + 1
              adc_ph = hscin_adc_neg(hit)
              path = hscin_long_coord(hit) - hscin_neg_coord(hit)
              time = hscin_tdc_neg(hit) * hscin_tdc_to_time
              hscin_neg_time(hit)=time - hscin_neg_invadc_offset(hit) -
     >            path / hscin_neg_invadc_linear(hit) -
     >            hscin_neg_invadc_adc(hit)/sqrt(max(20.,adc_ph))
              zcor =  (hscin_zpos(hit)/(29.979*betap) * sqrt(1.+
     >               hxp_fp(trk)*hxp_fp(trk)+hyp_fp(trk)*hyp_fp(trk)))
c$$$              if(hdumptof.eq.1.and.hntracks_fp.eq.1.and.
c$$$     $             timehist(max(1,jmax)).gt.6) then
               if(hdumptof.eq.1) then
c$$$                write(37,'(1x,''2'',2i3,5f10.3)') 
c$$$     >             hscin_plane_num(hit),
c$$$     >             hscin_counter_num(hit),
c$$$     >             hscin_tdc_neg(hit) * hscin_tdc_to_time,
c$$$     >             path,zcor,
c$$$     >             hscin_neg_time(hit)-zcor,adc_ph
                nsv = min(100, nsv + 1)
                idetsv(nsv) = 20 * (hscin_plane_num(hit)-1) +
     >            hscin_counter_num(hit) + 100
                tr0sv(nsv) = hscin_tdc_neg(hit) * 
     >            hscin_tdc_to_time
                psv(nsv) = path
                zcsv(nsv) = zcor
                tc1sv(nsv) = hscin_neg_time(hit)-zcor
                adcsv(nsv) = adc_ph
              endif
            endif

**    Calculate ave time for scintillator and error.
            if (hgood_tdc_pos(trk,hit)) then
              if (hgood_tdc_neg(trk,hit)) then
                hscin_time(hit) = (hscin_neg_time(hit) + hscin_pos_time(hit))/2.
                hscin_sigma(hit) = max(0.1,sqrt(hscin_neg_sigma(hit)**2 + 
     1               hscin_pos_sigma(hit)**2)/2.)
                hgood_scin_time(trk,hit) = .true.
                hntof_pairs = hntof_pairs + 1
              else
                hscin_time(hit) = hscin_pos_time(hit)
                hscin_sigma(hit) = max(0.1,hscin_pos_sigma(hit))
                hgood_scin_time(trk,hit) = .true.
*                hgood_scin_time(trk,hit) = .false.
              endif
            else                        ! if hgood_tdc_neg = .false.
              if (hgood_tdc_neg(trk,hit)) then
                hscin_time(hit) = hscin_neg_time(hit)
                hscin_sigma(hit) = max(0.1,hscin_neg_sigma(hit))
                hgood_scin_time(trk,hit) = .true.
*                hgood_scin_time(trk,hit) = .false.
              endif
            endif
c     Get time at focal plane
            if (hgood_scin_time(trk,hit)) then
              hscin_time_fp(hit) = hscin_time(hit)
     &             - (hscin_zpos(hit)/(29.979*betap) *
     &             sqrt(1.+hxp_fp(trk)*hxp_fp(trk)+hyp_fp(trk)*hyp_fp(trk)))
              sum_fp_time = sum_fp_time + hscin_time_fp(hit) /
     >          hscin_sigma(hit)**2
              num_fp_time = num_fp_time + 1./hscin_sigma(hit)**2
              sum_plane_time(plane)=sum_plane_time(plane)
     &             +hscin_time_fp(hit)
              num_plane_time(plane)=num_plane_time(plane)+1
              hnum_scin_hit(trk) = hnum_scin_hit(trk) + 1
              hscin_hit(trk,hnum_scin_hit(trk)) = hit
              hscin_fptime(trk,hnum_scin_hit(trk)) = hscin_time_fp(hit)

              if (hgood_tdc_pos(trk,hit) .and. hgood_tdc_neg(trk,hit)) then
                hnum_pmt_hit(trk) = hnum_pmt_hit(trk) + 2
              else
                hnum_pmt_hit(trk) = hnum_pmt_hit(trk) + 1
              endif
              if (hgood_tdc_pos(trk,hit)) then
                if (hgood_tdc_neg(trk,hit)) then
                  hdedx(trk,hnum_scin_hit(trk)) = sqrt(max(0.,
     &                 hscin_adc_pos(hit)*hscin_adc_neg(hit)))
                else
                  hdedx(trk,hnum_scin_hit(trk))=max(0.,hscin_adc_pos(hit))
                endif
              else
                if (hgood_tdc_neg(trk,hit)) then
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
          htime_at_fp(trk) = sum_fp_time / num_fp_time
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


        if(hntracks_fp.gt.1000) then
          if(trk.eq.1) write(*,'(/1x,''hms tol='',f8.2)') time_tolerance
          write(*,'(5i3,4L2,7f7.2)') trk,nfound,jmax,timehist(max(1,jmax)),
     >      hnum_pmt_hit(trk),
     >      hgood_plane_time(trk,1),hgood_plane_time(trk,3),
     >      hgood_plane_time(trk,2),hgood_plane_time(trk,4),
     >      htime_at_fp(trk),hbeta(trk),hbeta_chisq(trk),
     >      hdelta_tar(trk),hy_tar(trk),hxp_tar(trk),hyp_tar(trk)
        endif
        if(hntracks_fp.eq.1.and.
     >    hdumptof.eq.1.and.
     >     timehist(max(1,jmax)).gt.6) then
c        if(hdumptof.eq.1) then
           write(37,'(1x,''0'',2i3,5f10.3)') trk,hntracks_fp,
     >      p, betap
          call h_tofcal_fill(nsv,idetsv,tr0sv,psv,zcsv,
     >       tc1sv,adcsv)
        endif
      enddo                             !end of loop over tracks

 666  continue

      RETURN
      END



! Fit TOF for Hall C HMS with the form for each PMT:
! tcorr = time - offset - path * velocity - adccor / sqrt(ADC)
! where offset, velocity, and adccor are parameters
! September 20085 P. Bosted
! Modified to run automatically during replay: no longer
! any need to dump large text files
! To activate, set hdumptof = 1 in hdebug.param in
! the PARAM directory (can also do from command line)
! The output parameters will be in tof/hodoxxxx.param, where
! xxxxx is the run number
! Normal values of invadc_offset are between -50 and 50,
! normal values of invadc_velocity are between 12 and 17 (50 is
! default if not enough data for the fit), and normal values of
! shodo_pos_invadc_adc are 20 to 50. Normal values of the sigmas
! are 0.3 to 0.8 nsec. 

      subroutine h_tofcal_init
! initialize common block variables at begin run
      implicit none
      integer i,j
! common block variables
      integer thist(200,10),adchist(200,18),phist(200,18)
      integer nhit(200),ip1(200),ip2(200),ip3(200)
      integer ipdet(600),iptyp(600),nparam
      real*8 ax(1000,1000),bx(1000),avtime,ntime,avsig(200)
      common/htofcal/ ax,bx,thist,adchist,phist,nhit,
     >   ip1,ip2,ip3,ipdet,iptyp,avtime,ntime,avsig,nparam

      nparam=0
      do i=1,200
        nhit(i)=0
        ip1(i)=0
        ip2(i)=0
        ip3(i)=0
        avsig(i)=0.
        do j=1,10
          thist(i,j)=0
        enddo
        do j=1,18
          adchist(i,j)=0
          phist(i,j)=0
        enddo
      enddo
      do i=1,600
        ipdet(i)=0
        iptyp(i)=0
      enddo

! Initialize the fitting arrays
      do i=1,1000
        bx(i)=0.
        do j=1,1000
          ax(i,j)=0.
        enddo
      enddo

      avtime = 0.
      ntime = 0.

      return
      end

      subroutine h_tofcal_fill(n,idet,tr0,p,zc,tc1,adc)
! Fill in the arrays for HMS tof cal
! Inputs are:
! n  number of PMTs n
! idet detector code (from 1 to 200)
! tr0 TDCtime
! p path length
! zc time correction due to z 
! tc1 corrected time using current variables
! adc ADC

      implicit none

! local and input variables
      integer i,j,k,n,idt,idet(100)
      integer k1,k2,k3,k4,k5,k6
      real*8 tr0(100),p(100),zc(100),tc1(100),p2(100),adc(100),tr(100)
      real*8 dt,avval
      logical first_time/.true./
! common block variables
      integer thist(200,10),adchist(200,18),phist(200,18)
      integer nhit(200),ip1(200),ip2(200),ip3(200)
      integer ipdet(600),iptyp(600),nparam
      real*8 ax(1000,1000),bx(1000),avtime,ntime,avsig(200)
      common/htofcal/ ax,bx,thist,adchist,phist,nhit,
     >   ip1,ip2,ip3,ipdet,iptyp,avtime,ntime,avsig,nparam

      if(first_time) then
        first_time = .false.
        call h_tofcal_init
      endif

! need at least 6 PMTs for fitting
      if(n.le.5) return

c      write(6,'(/i3,10f6.1)') n,(tc1(i),i=1,min(10,n))
c      write(6,'(i3,10i6)') n,(idet(i),i=1,min(10,n))
! Loop over all PMTs
      avval = 0. 
      do i=1,n
! Check for valid detector code
        if(idet(i).le.0.or.idet(i).gt.200) then
          write(6,'(''ERROR, in h_tofcal, idet='',2i8)') i,idet(i)
          return
        endif

! Fill in ADC histograms
        k = min(18., max(1., (adc(i)/20.)+1))
        adchist(idet(i), k) = adchist(idet(i), k) + 1 

! correct raw times for zpos using betap
        tr(i) = tr0(i)- zc(i)

! Put 1./sqrt(ADC) in p2 variable
        p2(i) = 1./sqrt(max(20., adc(i)))

! Histogram path length variable
        k = min(18, max(1, int(p(i)/7.)+1))
        phist(idet(i), k) = phist(idet(i), k) + 1 
! average time
        avval = avval + tc1(i)
      enddo
      avval = avval / float(n)

! Loop over PMTS again
! Get average h_start_time and sigmas for each PMT
! THESE SHOULD BE DONE on a 2nd iteration of the
! TOF calibration for a given run, setting
! h_tof_tolerance to something small like 3 nsec
      do j=1,n
        nhit(idet(j))=nhit(idet(j))+1
        avsig(idet(j)) = avsig(idet(j)) + 
     >    (tc1(j) - avval)**2

! If first time detector used, assign corresponding parameters
! Note that detector 4 had has a fixed time offset (ip1) of zero
! since all times are relative. 
        if(nhit(idet(j)).eq.1) then
          if(idet(j).eq.4) then
            ip1(idet(j))=0
          else
! fixed time offsets
            nparam=nparam+1
            ip1(idet(j))=nparam
            ipdet(nparam)=idet(j)
            iptyp(nparam)=1
          endif

! linear term in path
! Changed 11/08 to make same for both pos. and neg. ends! 
          if(idet(j).le.100) then
           nparam=nparam+1
           ip2(idet(j))=nparam
           ip2(idet(j)+100)=nparam
           ipdet(nparam)=idet(j)
           iptyp(nparam)=2
          endif

! 1/sqrt(adc) terms (or could be path length**2 if desired)
          nparam=nparam+1
          ip3(idet(j))=nparam
          ipdet(nparam)=idet(j)
          iptyp(nparam)=3
          k=idet(j)
c          write(6,'(''h_tofcal_fill'',i3,4i5)') 
c     >      k,nhit(k),ip1(k),ip2(k),ip3(k)
        endif
        avtime = avtime + tc1(j)
        ntime = ntime + 1.
       enddo ! loop over n

! now loop over all pairs in fill in the matrix
! Also, histogram time differences using current corrections
       do j=1,n-1
          do k=j+1,n
            if(ip2(idet(j)).gt.0.and.ip2(idet(k)).gt.0) then
              dt = tc1(j)-tc1(k)
              idt = min(10,max(1,int((dt+5.))+1))
              thist(idet(j),idt) = thist(idet(j),idt) + 1
              dt = tc1(k)-tc1(j)
              idt = min(10,max(1,int((dt+5.))+1))
              thist(idet(k),idt) = thist(idet(k),idt) + 1
              k1 = idet(j)
              k2 = idet(k)
              k1=ip1(idet(j))
              k2=ip1(idet(k))
              k3=ip2(idet(j))
              k4=ip2(idet(k))
              k5=ip3(idet(j))
              k6=ip3(idet(k))
              if(k1.gt.0) then
                bx(k1) = bx(k1) - (tr(j)-tr(k))
                ax(k1,k1) = ax(k1,k1) + 1.
                ax(k1,k3) = ax(k1,k3) + p(j)
                ax(k1,k4) = ax(k1,k4) - p(k)
                ax(k1,k5) = ax(k1,k5) + p2(j)
                ax(k1,k6) = ax(k1,k6) - p2(k)
                ax(k3,k1) = ax(k3,k1) + p(j)
                ax(k4,k1) = ax(k4,k1) - p(k)
                ax(k5,k1) = ax(k5,k1) + p2(j)
                ax(k6,k1) = ax(k6,k1) - p2(k)
              endif
              if(k1.gt.0.and.k2.gt.0) then
                ax(k1,k2) = ax(k1,k2) - 1.
                ax(k2,k1) = ax(k2,k1) - 1.
              endif
              if(k2.gt.0) then
                bx(k2) = bx(k2) + (tr(j)-tr(k))
                ax(k2,k2) = ax(k2,k2) + 1.
                ax(k2,k3) = ax(k2,k3) - p(j)
                ax(k2,k4) = ax(k2,k4) + p(k)
                ax(k2,k5) = ax(k2,k5) - p2(j)
                ax(k2,k6) = ax(k2,k6) + p2(k)
                ax(k3,k2) = ax(k3,k2) - p(j)    
                ax(k4,k2) = ax(k4,k2) + p(k)    
                ax(k5,k2) = ax(k5,k2) - p2(j)    
                ax(k6,k2) = ax(k6,k2) + p2(k)    
              endif
              bx(k3) = bx(k3) - (tr(j)-tr(k)) * p(j)
              bx(k4) = bx(k4) + (tr(j)-tr(k)) * p(k)
              bx(k5) = bx(k5) - (tr(j)-tr(k)) * p2(j)
              bx(k6) = bx(k6) + (tr(j)-tr(k)) * p2(k)
              ax(k3,k3) = ax(k3,k3) + p(j)*p(j)
              ax(k3,k4) = ax(k3,k4) - p(k)*p(j)
              ax(k3,k5) = ax(k3,k5) + p2(j)*p(j)
              ax(k3,k6) = ax(k3,k6) - p2(k)*p(j)
              ax(k4,k3) = ax(k4,k3) - p(j)*p(k)
              ax(k4,k4) = ax(k4,k4) + p(k)*p(k)
              ax(k4,k5) = ax(k4,k5) - p2(j)*p(k)
              ax(k4,k6) = ax(k4,k6) + p2(k)*p(k)
              ax(k5,k3) = ax(k5,k3) + p(j)*p2(j)
              ax(k5,k4) = ax(k5,k4) - p(k)*p2(j)
              ax(k5,k5) = ax(k5,k5) + p2(j)*p2(j)
              ax(k5,k6) = ax(k5,k6) - p2(k)*p2(j)
              ax(k6,k3) = ax(k6,k3) - p(j)*p2(k)
              ax(k6,k4) = ax(k6,k4) + p(k)*p2(k)
              ax(k6,k5) = ax(k6,k5) - p2(j)*p2(k)
              ax(k6,k6) = ax(k6,k6) + p2(k)*p2(k)
           endif
         enddo
      enddo


      return
      end


      subroutine h_tofcal_endrun(runno)
! Solve simultaneous linear equations for best values of
! tof parameters, and write to file
! local and input variables
      implicit none

      INCLUDE 'hms_data_structures.cmn'
      include 'hms_scin_parms.cmn'
      include 'hms_scin_tof.cmn'
      integer i,j,runno,iwork(1000),ifail
      real*8 toff(200),vel(200),quad(200)
      character*80 fname
! common block variables
      integer thist(200,10),adchist(200,18),phist(200,18)
      integer nhit(200),ip1(200),ip2(200),ip3(200)
      integer ipdet(600),iptyp(600),nparam
      real*8 ax(1000,1000),bx(1000),avtime,ntime,avsig(200)
      common/htofcal/ ax,bx,thist,adchist,phist,nhit,
     >   ip1,ip2,ip3,ipdet,iptyp,avtime,ntime,avsig,nparam


! find the solutions
      if (nparam .lt. 1) return
      call deqn (nparam,ax,1000,iwork,ifail,1,bx)

! association of parameters with detectors
      do i=1,200
        toff(i)=0
        vel(i)=0.
        quad(i)=0.
      enddo

      do i=1,nparam
        if(iptyp(i).eq.1) toff(ipdet(i))=bx(i)
        if(iptyp(i).eq.2)  vel(ipdet(i))=bx(i)
        if(iptyp(i).eq.2)  vel(ipdet(i)+100)=bx(i)
        if(iptyp(i).eq.3) quad(ipdet(i))=bx(i)
      enddo


! write solutions
      write(fname,'(''HTOFCAL/htofcal'',i5.5,''.param'')') 
     >  runno

      open(unit=10,file=fname)

      write(10,'(''; This parameter determines how close'',
     >  '' in time the ''/
     >  ''; corrected scint. have to be to each other. '',
     >  '' For initial calibrations, use 50.  ''/
     >  ''; For final calibration,use 3. For regular '',
     >  '' running, use about 10.''/
     >  ''; Used in h_trans_scin.f and h_tof.f''/
     >  ''   htof_tolerance = '',f6.1)') 
     >  htof_tolerance

      write(10,'(/''; This is default (average) value'',
     >  '' of start time for ''/
     >  ''; drift chambers. It is used in h_trans_scin.f''/
     >  ''   hstart_time_center = '',f6.1)') avtime/
     >  max(1.,ntime)

! copied from previous hhodo.param
      write(10,'(/''; This is 1/2 width of winow'',
     >  '' on hstart_time_center''/
     >  ''; it is used in h_trans_scin.f''/
     >  ''   hstart_time_slop = '',f6.1)') 
     >  hstart_time_slop

      write(10,'(/''; Minimum and Maximum raw TDC'',
     >  '' that will be used ''/
     >  ''; Check raw TDC spectra to make sure ok''/
     >  ''   hscin_tdc_min = '',i6/ 
     >  ''   hscin_tdc_max = '',i6)') 
     >  int(hscin_tdc_min),int(hscin_tdc_max)

      write(10,'(/''; TDC time in nsec per channel''/
     >  ''   hscin_tdc_to_time = '',f8.4)') 
     >  hscin_tdc_to_time

      write(10,'(/''; Position tolerance in cm'',
     >  '' used in efficiency calcultions ''/
     >  ''; used in hms_scin_eff.f''/
     >  ''   hhodo_slop = '',
     >  f5.0,'','',f5.0,'','',f5.0,'','',f5.0)') 
     >  (hhodo_slop(i),i=1,4)

      write(10,'(/''hhodo_pos_invadc_offset ='',3(f8.2,'',''),
     >  f8.2)') (-1.0*toff(i),i=1,80,20)
      do j=2,16
       write(10,'(1x,''                        '',3(f8.2,'',''),
     >  f8.2)')(-1.0*toff(i),i=j,79+j,20)
      enddo

      write(10,'(/''hhodo_neg_invadc_offset ='',3(f8.2,'',''),
     >  f8.2)')(-1.0*toff(i),i=101,180,20)
      do j=2,16
       write(10,'(1x,''                        '',3(f8.2,'',''),
     >  f8.2)')(-1.0*toff(i),i=100+j,179+j,20)
      enddo

      write(10,'(/''hhodo_pos_invadc_linear ='',3(f8.2,'',''),
     >  f8.2)')( -1./min(-0.02,vel(i)),i=1,80,20)
      do j=2,16
       write(10,'(1x,''                        '',3(f8.2,'',''),
     >  f8.2)')(-1./min(-0.02,vel(i)),i=j,79+j,20)
      enddo

      write(10,'(/''hhodo_neg_invadc_linear ='',3(f8.2,'',''),
     >  f8.2)')( -1./min(-0.02,vel(i)),i=101,180,20)
      do j=2,16
       write(10,'(1x,''                        '',3(f8.2,'',''),
     >  f8.2)')(-1./min(-0.02,vel(i)),i=100+j,179+j,20)
      enddo

      write(10,'(/''hhodo_pos_invadc_adc='',3(f9.2,'',''),
     >  f9.2)')(-1.*quad(i),i=1,80,20)
      do j=2,16
       write(10,'(1x,''                   '',3(f9.2,'',''),
     >  f9.2)')(-1.*quad(i),i=j,79+j,20)
      enddo

      write(10,'(/''hhodo_neg_invadc_adc='',3(f9.2,'',''),
     >  f9.2)')(-1.0*quad(i),i=101,180,20)
      do j=2,16
       write(10,'(1x,''                   '',3(f9.2,'',''),
     >  f9.2)')(-1.*quad(i),i=100+j,179+j,20)
      enddo

      do i=1,200
        avsig(i) = avsig(i) / max(1,nhit(i))
      enddo

      write(10,'(/''hhodo_pos_sigma ='',3(f8.2,'',''),
     >  f8.2)') (max(0.1,min(100.,avsig(i))),i=1,80,20)
      do j=2,16
       write(10,'(1x,''               '',3(f8.2,'',''),
     >  f8.2)')(max(0.1,min(100.,avsig(i))),i=j,79+j,20)
      enddo

      write(10,'(/''hhodo_neg_sigma ='',3(f8.2,'',''),
     >  f8.2)') (max(0.3,min(100.,avsig(i))),i=101,180,20)
      do j=2,16
       write(10,'(1x,''               '',3(f8.2,'',''),
     >  f8.2)')(max(0.3,min(100.,avsig(i))),i=100+j,179+j,20)
      enddo

      write(10,'(/''hhodo_pos_ped_limit = 1000,'',
     >  ''1000,1000,1000,1000,1000,1000,1000''/
     >  22x,''1000,1000,1000,1000,1000,1000,1000,1000''/   
     >  22x,''1000,1000,1000,1000,1000,1000,1000,1000''/   
     >  22x,''1000,1000,1000,1000,1000,1000,1000,1000''/   
     >  22x,''1000,1000,1000,1000,1000,1000,1000,1000''/   
     >  22x,''1000,1000,1000,1000,1000,1000,1000,1000''/   
     >  22x,''1000,1000,1000,1000,1000,1000,1000,1000''/   
     >  22x,''1000,1000,1000,1000,1000,1000,1000,1000'')')

      write(10,'(/''hhodo_neg_ped_limit = 1000,'',
     >  ''1000,1000,1000,1000,1000,1000,1000''/
     >  22x,''1000,1000,1000,1000,1000,1000,1000,1000''/   
     >  22x,''1000,1000,1000,1000,1000,1000,1000,1000''/   
     >  22x,''1000,1000,1000,1000,1000,1000,1000,1000''/   
     >  22x,''1000,1000,1000,1000,1000,1000,1000,1000''/   
     >  22x,''1000,1000,1000,1000,1000,1000,1000,1000''/   
     >  22x,''1000,1000,1000,1000,1000,1000,1000,1000''/   
     >  22x,''1000,1000,1000,1000,1000,1000,1000,1000'')')

      close(unit=10)

! Diagnositc information 
      write(fname,'(''HTOFCAL/htofcal'',i5.5,''.diag'')') 
     >  runno
      open(unit=10,file=fname)

      write(10,'(1x,''ifail='',i10,
     >  '' (desired value is 0 if fit worked)'')') ifail

      do i=1,100
       if(nhit(i).gt.0 .or.nhit(100+i).gt.0.) then
        write(10,'(i3,2i6,6f7.1)') i,nhit(i),nhit(100+i),
     >    -toff(i),-toff(100+i),-1./vel(i),-1./vel(100+i),
     >    -quad(i),-quad(100+i) 
       endif
      enddo
      write(10,'(''ADCHIST'')')
      do i=1,200
        if(nhit(i).gt.0) write(10,'(i4,16i4)') 
     >    i,(adchist(i,j)/10,j=1,16)
      enddo
      write(10,'(''THIST'')')
      do i=1,200
        if(nhit(i).gt.0) write(10,'(i4,10i5)') 
     >    i,(thist(i,j)/100,j=1,10)
      enddo
      write(10,'('' PHIST'')')
      do i=1,200
        if(nhit(i).gt.0) write(10,'(i4,16i4)') 
     >    i,(phist(i,j)/20,j=1,16)
      enddo

      close(unit=10)

      return
      end

