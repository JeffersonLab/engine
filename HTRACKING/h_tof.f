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
* Revision 1.19.4.2  2007/05/10 21:15:10  cdaq
* changes for writing dump file for Peter's tof code
*
* Revision 1.19.4.1  2007/05/02 21:19:30  jones
* Add new code needed for  adjusting scintillator timing using P Bosted's method.
* Only used when flag  htofusinginvadc.eq.1 .
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
      integer ndumped(4,16,2),ndumpmax
      logical oktodump
      save
*
*--------------------------------------------------------
*
      ABORT= .FALSE.
      errmsg = ' '

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
        betap = p/sqrt(p*p+hpartmass*hpartmass)

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
        time_tolerance=3.0
        if(htof_tolerance.gt.0.5.and.htof_tolerance.lt.10000.) then
          time_tolerance=htof_tolerance
        endif
! Use wide window if dumping events for fitting
        if(hdumptof.eq.1) time_tolerance=50.0
        if(first) then
           first=.false.
           write(*,'(/1x,''USING '',f8.2,'' NSEC WINDOW FOR'',
     >     ''  HMS TOF AND FP CALCULATIONS'')') time_tolerance
           if(htofusinginvadc.eq.1) then
             write(*,'(/1x,''TOF using 1/sqrt(ADC), separate '',
     >         ''velocities for pos and neg tubes'')')
           else
             write(*,'(/1x,''TOF using ADC for slewing correction'',
     >         ''  and same vecolicty for pos and neg tubes'')')
           endif
           ndumpmax = 1000.
           if(hdumptof.eq.1) 
     >       write(*,'(/1x,''Dumping TDC, ADC to fort.37 for'',
     >         ''  TOF calibration, ndumpmax='',i5)') ndumpmax
           write(*,'(/)')
           do i=1,4
            do j=1,16
             ndumped(i,j,2)= 0.
            enddo
           enddo
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
              if(htofusinginvadc.eq.1) then
                time_pos(i) = time - hscin_pos_invadc_offset(hit) -
     >            path / hscin_pos_invadc_linear(hit) -
     >            hscin_pos_invadc_adc(hit)/sqrt(max(20,adc_ph))
              else
                time = time - hscin_pos_phc_coeff(hit) *
     &               sqrt(max(0.,(adc_ph/hscin_pos_minph(hit)-1.)))
                time = time - path/hscin_vel_light(hit)
                time_pos(i) = time - hscin_pos_time_offset(hit)
              endif
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
              if(htofusinginvadc.eq.1) then
                time_neg(i) = time + hscin_neg_invadc_offset(hit) -
     >            path / hscin_neg_invadc_linear(hit) -
     >            hscin_neg_invadc_adc(hit)/sqrt(max(20,adc_ph))
              else
                time = time - hscin_neg_phc_coeff(hit) *
     &               sqrt(max(0.,(adc_ph/hscin_neg_minph(hit)-1.)))
                time = time - path/hscin_vel_light(hit)
                time_neg(i) = time - hscin_neg_time_offset(hit)
              endif
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
        oktodump = .false.
        do j=1,200
          if(timehist(j) .gt. maxhit) then
            jmax = j
            maxhit = timehist(j)
          endif
        enddo
        if(jmax.gt.0) then
          tmin = 0.5*float(jmax) 
          do hit = 1 , hscin_tot_hits
            i=min(1000,hit)
            if(time_pos(i) .gt. tmin .and.
     >         time_pos(i) .lt. tmin + time_tolerance) then
              keep_pos(i) = .true.
              ndumped(hscin_plane_num(hit),hscin_plane_num(hit),1) =  
     >        ndumped(hscin_plane_num(hit),hscin_plane_num(hit),1) + 1
              if(ndumped(hscin_plane_num(hit),
     >          hscin_plane_num(hit),1).lt.ndumpmax) oktodump=.true.
            endif
            if(time_neg(i) .gt. tmin .and.
     >         time_neg(i) .lt. tmin + time_tolerance) then
              keep_neg(i) = .true.
              ndumped(hscin_plane_num(hit),hscin_plane_num(hit),2) =  
     >        ndumped(hscin_plane_num(hit),hscin_plane_num(hit),2) + 1
              if(ndumped(hscin_plane_num(hit),
     >          hscin_plane_num(hit),2).lt.ndumpmax) oktodump=.true.
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
              if(htofusinginvadc.eq.1) then
                hscin_pos_time(hit)=time - hscin_pos_invadc_offset(hit) -
     >            path / hscin_pos_invadc_linear(hit) -
     >            hscin_pos_invadc_adc(hit)/sqrt(max(20,adc_ph))
              else
                time = time - hscin_pos_phc_coeff(hit) *
     &               sqrt(max(0.,(adc_ph/hscin_pos_minph(hit)-1.)))
                time = time - path/hscin_vel_light(hit)
                hscin_pos_time(hit) = time - hscin_pos_time_offset(hit)
              endif
              zcor =  (hscin_zpos(hit)/(29.979*betap) * sqrt(1.+
     >               hxp_fp(trk)*hxp_fp(trk)+hyp_fp(trk)*hyp_fp(trk)))
              if(hntracks_fp.eq.1.and.
     >          hdumptof.eq.1.and.
     >          oktodump.and.
     >          timehist(max(1,jmax)).gt.6) then
                write(37,'(1x,''1'',2i3,5f10.3)') 
     >             hscin_plane_num(hit),
     >             hscin_counter_num(hit),
     >             hscin_tdc_pos(hit) * hscin_tdc_to_time,
     >             path,zcor,
     >             hscin_pos_time(hit)-zcor,adc_ph
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
              if(htofusinginvadc.eq.1) then
                hscin_neg_time(hit)=time - hscin_neg_invadc_offset(hit) -
     >            path / hscin_neg_invadc_linear(hit) -
     >            hscin_neg_invadc_adc(hit)/sqrt(max(20,adc_ph))
              else
                time = time - hscin_neg_phc_coeff(hit) *
     &               sqrt(max(0.,(adc_ph/hscin_neg_minph(hit)-1.)))
                time = time - path/hscin_vel_light(hit)
                hscin_neg_time(hit) = time - hscin_neg_time_offset(hit)
              endif
              zcor =  (hscin_zpos(hit)/(29.979*betap) * sqrt(1.+
     >               hxp_fp(trk)*hxp_fp(trk)+hyp_fp(trk)*hyp_fp(trk)))
              if(hntracks_fp.eq.1.and.
     >          hdumptof.eq.1.and.
     >          oktodump.and.
     >          timehist(max(1,jmax)).gt.6) then
                write(37,'(1x,''2'',2i3,5f10.3)') 
     >             hscin_plane_num(hit),
     >             hscin_counter_num(hit),
     >             hscin_tdc_neg(hit) * hscin_tdc_to_time,
     >             path,zcor,
     >             hscin_neg_time(hit)-zcor,adc_ph
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
           write(37,'(1x,''0'')') 
        endif
      enddo                             !end of loop over tracks

 666  continue

      RETURN
      END

