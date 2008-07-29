      SUBROUTINE H_TRACK(ABORT,err)
*--------------------------------------------------------
*-
*-   Purpose and Methods :  Finds and fits tracks in HMS focal plane 
*-
*-      Required Input BANKS     HMS_DECODED_DC
*-
*-      Output BANKS             HMS_FOCAL_PLANE
*-                               HMS_DECODED_DC hit coordinates
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
*-   Created 19-JAN-1994   D. F. Geesaman
* $Log$
* Revision 1.5.24.1  2008/07/29 16:28:11  puckett
* added calls to space point prune routines only when we fail to find a track the usual way
*
* Revision 1.5  1996/09/04 13:37:02  saw
* (JRA) Initialize hstubmin variables
*
* Revision 1.4  1995/10/11 12:19:50  cdaq
* (JRA) Only call tracking routines when it is warranted
*
* Revision 1.3  1995/05/22 19:39:30  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.2  1994/04/13  17:07:57  cdaq
* (DFG) Added histograming call (h_fill_dc_fp_hist)
*
* Revision 1.1  1994/02/19  06:20:31  cdaq
* Initial revision
*
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*7 here
      parameter (here= 'H_TRACK')
*
      logical ABORT
      character*(*) err
      integer*4 ierr
      character*5  line_err

      real*4 storetest(4)

      logical secondtry

      integer*4 npassed
*
      INCLUDE 'hms_data_structures.cmn'
      INCLUDE 'hms_tracking.cmn'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      include 'gen_event_info.cmn'
*
*--------------------------------------------------------
*
*
      ABORT = .false.
      err = ' '

      if (hdc_tot_hits.ne.0) then
        call H_PATTERN_RECOGNITION(ABORT,err)
        if(ABORT) then
          call G_add_path(here,err)
          return
        endif
     
*
        if (hnspace_points_tot.ne.0) then 

           call H_LEFT_RIGHT(ABORT,err)
           if(ABORT) then
              call G_add_path(here,err)
              return
           endif
*
           
c           write(*,*) 'calling h_link_stubs'

c     ajp 05/16/08
           h_find_stub_t0 = .false.
           h_do_prune_stubs = .false.
           secondtry = .false.

 101       continue

           hstubminx = 999999.
           hstubminy = 999999.
           hstubminxp = 999999.
           hstubminyp = 999999.
c     ajp051608
           hstubmint0 = 999999.
c     ajp051608
           
           if(h_find_stub_t0) then
              call h_stub_t0_calc(ABORT,err)
           endif

           if(h_do_prune_stubs) then
              call h_prune_stubs(ABORT,err)
           endif

c     write(*,*) 'calling h_left_right, h_find_stub_t0=',h_find_stub_t0

           h_fail_too_many_tracks = .false.

           call H_LINK_STUBS(ABORT,err)
           if(ABORT) then
              call G_add_path(here,err)
              return
           endif

           if(.not.h_find_stub_t0.and.hntracks_fp.le.0.and.
     $          hbypass_stub_t0.eq.0) then
              h_find_stub_t0 = .true.
              goto 101
           endif

c     reset stub cuts if we changed them before:
           if(h_do_prune_stubs.and.secondtry) then
              hxt_track_criterion = storetest(1)
              hyt_track_criterion = storetest(2)
              hxpt_track_criterion = storetest(3)
              hypt_track_criterion = storetest(4)
           endif

c     if there are too many tracks, then we repeatedly pare down the space points
c     until we have an acceptable number of tracks:

           if(.not.h_do_prune_stubs.and.hntracks_fp.le.0.and.hstubtest.eq.1
     $          .and.hbypass_prune_stubs.eq.0) then
              h_do_prune_stubs = .true.
c     if we have "too many" tracks, then we should be able to bring the number of tracks down to size using 
c     tighter stub cuts:
              goto 101
           else if(hntracks_fp.le.0.and.hstubtest.eq.1.and.
     $             hbypass_prune_stubs.eq.0.and.h_do_prune_stubs) then
              if(secondtry) then
                 h_stub_prune_flags(7) = 1 ! this will guarantee the selection of exactly one space point per chamber--better than nothing
                 secondtry = .false.
              else
                 secondtry = .true.
                 
                 storetest(1) = hxt_track_criterion
                 storetest(2) = hyt_track_criterion
                 storetest(3) = hxpt_track_criterion
                 storetest(4) = hypt_track_criterion

                 hxt_track_criterion = max(hstubminx,hxt_track_criterion/2.)
                 hyt_track_criterion = max(hstubminy,hyt_track_criterion/2.)
                 hxpt_track_criterion = max(hstubminxp,hxpt_track_criterion/2.)
                 hypt_track_criterion = max(hstubminyp,hypt_track_criterion/2.)
                 
              endif
              goto 101
           endif
c     if no tracks were found passing the three stub linking criteria, but we have
c     space points in both chambers and we pass two of three stub linking criteria, 
c     build tracks from those stubs:
           if(hntracks_fp.le.0.and.hstubtest.eq.0.and.hnspace_points(1)
     $          .ge.1.and.hnspace_points(2).ge.1.and.
     $          hbypass_ajp_stublink.eq.0) then
              npassed = 0
              if(abs(hstubminx).lt.hxt_track_criterion)
     $             npassed = npassed + 1
              if(abs(hstubminy).lt.hyt_track_criterion)
     $             npassed = npassed + 1
              if(abs(hstubminxp).lt.hxpt_track_criterion)
     $             npassed = npassed + 1
              if(npassed.ge.2) then ! call my special "ajp" stub linking routine:
c                 write(*,*) 'calling AJP stub link routine event=',gen_event_id_number
                 call h_join_stubs_ajp(abort,err)
              endif
           endif

c           write(*,*) 'calling h_track_fit'
*
           if (hntracks_fp.ne.0) then
              call H_TRACK_FIT(ABORT,err,ierr)
              if(ABORT) then
                 call G_add_path(here,err)
                 return
              endif
              
*     Check for internal error in H_TRACK_FIT
              if(ierr.ne.0) then
                 line_err=' '
                 call CSETDI(ierr,line_err,1,5)
                 err='ERROR IN H_TRACK_FIT' // line_err
                 call G_add_path(here,err)
                 call G_LOG_MESSAGE(err)
              endif                    
*     histogram focal plane tracks
*     
              call h_fill_dc_fp_hist(ABORT,err)
              if(ABORT) then
                 call g_add_path(here,err)
                 return
              endif
*     
           endif                !(hntracks_fp.ne.0)
        endif                   !(hnspace_points_tot.ne.0)
      endif                     !(hdc_tot_hits.ne.0)
      
      return
      end




