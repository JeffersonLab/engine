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
* Revision 1.2  1994/04/13 19:00:06  cdaq
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

        integer*4 ihit, iset
        integer*4 setside
        real*4 scint_center
        real*4 hit_position
        real*4 dist_from_center
        real*4 pos_path, neg_path
        real*4 pos_ph(smax_scin_hits)          !pulse height (channels)
        real*4 neg_ph(smax_scin_hits)
        real*4 postime(smax_scin_hits)
        real*4 negtime(smax_scin_hits)
        real*4 mintime

        save
 
        abort = .false.

** Initialize track-independant quantaties.
        call s_tof_init(abort,errmsg)
*
*
        if (abort) then
          call g_prepend(here,errmsg)
          return
        endif
        sgood_start_time = .false.
        sgood_start_plane = .false.
      if( hscin_tot_hits .gt. 0)  then
* histogram raw scin
        call h_fill_scin_raw_hist(abort,errmsg)
        if (abort) then
          call g_prepend(here,errmsg)
          return
        endif
      endif
*
*       
*       test for at least one valid hit
      if( sscin_tot_hits .gt. 0)  then
        do ihit = 1 , sscin_tot_hits
          stwo_good_times(ihit) = .false.
        enddo

** Find tube that gave the trigger.
*    For now, just take lowest TDC value (good enough for GEANT
*    simulation).  Probably want to convert to time and correct
*    for tube offset to find tube that defined timing.  Don't
*    want to do pulse height correction or position correction,
*    so don't need to do these corrections first.

        mintime = 1.0e+20       !initialize to large value.
        do ihit = 1 , sscin_tot_hits

*  make sure have good TDC value before checking time.
          if ((sscin_tdc_pos(ihit) .ge. sscin_tdc_min) .and.
     1        (sscin_tdc_pos(ihit) .le. sscin_tdc_max)) then  !good tdc

            postime(ihit) = sscin_tdc_pos(ihit) * sscin_tdc_to_time
            postime(ihit) = postime(ihit) - sscin_pos_time_offset(ihit)
            if (postime(ihit).lt.mintime) then
              mintime = postime(ihit)
              iset = ihit
              setside = 1             !1 is pos side. Bad notation. Sorry.
            endif
          endif

*  make sure have good TDC value before checking negative time.
          if ((sscin_tdc_neg(ihit) .ge. sscin_tdc_min) .and.
     1        (sscin_tdc_neg(ihit) .le. sscin_tdc_max)) then  !good tdc

            negtime(ihit) = sscin_tdc_neg(ihit) * sscin_tdc_to_time
            negtime(ihit) = negtime(ihit) - sscin_neg_time_offset(ihit)

*  see if both sides had good tdc.
            if ((sscin_tdc_pos(ihit) .ge. sscin_tdc_min) .and.
     1          (sscin_tdc_pos(ihit) .le. sscin_tdc_max)) then
              stwo_good_times(ihit) = .true.
            endif

            if (negtime(ihit).lt.mintime) then
              mintime = negtime(ihit)
              iset = ihit
              setside = 2             !2 is neg side. Bad notation. Sorry.
            endif
          endif

        enddo                       !end of loop that finds tube setting time.


** Get corrected time/adc for each scintillator hit
        do ihit = 1 , sscin_tot_hits
          if (stwo_good_times(ihit)) then  !both tubes fired

*  Correct time for everything except veloc. correction in order to
*  find hit location from difference in tdc.
            pos_ph(ihit) = float(sscin_adc_pos(ihit))
            postime(ihit) = sscin_tdc_pos(ihit) * sscin_tdc_to_time
            postime(ihit) = postime(ihit) + sscin_pos_phc_coeff(ihit) * 
     1             sqrt(max(pos_ph(ihit),sscin_minph))
            postime(ihit) = postime(ihit) - sscin_pos_time_offset(ihit)

            neg_ph(ihit) = float(sscin_adc_neg(ihit))
            negtime(ihit) = sscin_tdc_neg(ihit) * sscin_tdc_to_time
            negtime(ihit) = negtime(ihit) + sscin_neg_phc_coeff(ihit) * 
     1             sqrt(max(neg_ph(ihit),sscin_minph))
            negtime(ihit) = negtime(ihit) - sscin_neg_time_offset(ihit)

*  Find hit position.  If postime larger, then hit was nearer negative side.
            dist_from_center = 0.5*(negtime(ihit) - postime(ihit))
     1                        / sscin_vel_light(ihit)
            scint_center = (sscin_pos_coord(ihit)+sscin_neg_coord(ihit))/2.
            hit_position = scint_center + dist_from_center
 
*  get corrected time.
            pos_path = abs(sscin_pos_coord(ihit) - hit_position)
            neg_path = abs(sscin_neg_coord(ihit) - hit_position)
            postime(ihit) = postime(ihit) - pos_path/sscin_vel_light(ihit)
            negtime(ihit) = negtime(ihit) - neg_path/sscin_vel_light(ihit)

            sscin_cor_time(ihit) = ( postime(ihit) + negtime(ihit) )/2.
            sscin_cor_adc(ihit) = sqrt( neg_ph(ihit) * pos_ph(ihit) )

          else          !only 1 tube fired
            sscin_cor_adc(ihit) = 0.
            sscin_cor_time(ihit) = 0. !not a very good 'flag', but there is
                                        ! the logical stwo_good_hits.
          endif
        enddo     !loop over hits to find ave time,adc.

** Now, get the corrected time for the tube that set the timing, if we
* can extract a good start time.

*  if trigger timing set by plane other than S1X, need to correct time
*  to time at first plane, which requires track and tof measurement.
*  For now, skip event and don't set hgood_start_plane flag true if
*  1st plane didn't set timing.
        if (sscin_plane_num(iset) .eq. 1) then
          sgood_start_plane = .true.
        else
          goto 100              !give up on finding start_time
        endif

* See if both tubes have good TDC value.
        if (.not.stwo_good_times(iset)) goto 100  !give up on start time.

* Get start time.
        if (setside.eq.1) then          !setside=1 means pos tube set time.
          sstart_time = postime(iset)
          sgood_start_time = .true.
          sstart_hitnum = iset
          sstart_hitside = setside
        else if (setside.eq.2) then     !setside=2 means neg tube set time.
          sstart_time = negtime(iset)
          sgood_start_time = .true.
          sstart_hitnum = iset
          sstart_hitside = setside
        else
*         we don't want to abort eveything if no scin fire
*         abort = .true.
*
          write(errmsg,*) 'variable SETSIDE (1=pos tube set time,2=neg side) ='
     1                       ,setside
          call g_prepend(here,errmsg)
          return
        endif

100     continue        !jump here if have error finding start_time
*
*  Dump decoded bank if sdebugprintscindec is set
        if( sdebugprintscindec .ne. 0) then
          call s_prt_dec_scin(ABORT,errmsg)
        endif
      endif                 ! end major test on  sscin_tot_hits > 0     
        return
        end
