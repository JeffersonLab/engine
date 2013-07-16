      subroutine S_LINK_STUBS(ABORT,err)
*     This subroutine compares all the space-point-stubs found in
*     S_LEFT_RIGHT.f and links together stubs to form tracks.
*     The criterion are that the stubs are in different chambers and
*     each of the four track parameters are within limit:
*     sxt_track_criterion      for x_t
*     syt_track_criterion      for y_t
*     stx_track_criterion      for t_x
*     sty_track_criterion      for t_y
*
*     d.f. geesaman           7-September 1993
* $Log: s_link_stubs.f,v $
* Revision 1.7  1996/09/05 19:55:23  saw
* (DVW) Added some track tests
*
* Revision 1.6  1996/01/17 19:01:38  cdaq
* (JRA)
*
* Revision 1.5  1995/08/31 18:44:51  cdaq
* (JRA) Calculate dpos (pos. track - pos. hit) variables
*
* Revision 1.4  1995/05/22  19:45:42  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.3  1995/04/01  20:42:57  cdaq
* (SAW) Fix typos
*
* Revision 1.2  1994/06/07  04:41:19  cdaq
* (DFG) Add switch to include single stub tracks
*
* Revision 1.1  1994/02/21  16:14:56  cdaq
* Initial revision
*
*
*     The logic is 1) loop over all space points as seeds  isp1
*                  2) Check if this space point is all ready in a track
*                  3) loop over all succeeding space pointss   isp2
*                  4) check if there is a track-criterion match
*                       either add to existing track
*                       or if there is another point in same chamber
*                          make a copy containing isp2 rather than 
*                            other point in same chamber
*                  5) If ssingle_stub is set, make a track of all single
*                     stubs.
*
      implicit none
      include 'sos_data_structures.cmn'
      include 'sos_tracking.cmn'
      include 'sos_id_histid.cmn'
      include 'sos_geometry.cmn'
      INCLUDE 'sos_track_histid.cmn'    ! TEMP. JUNK
*Derek added these next lines
      include 'sos_bypass_switches.cmn'
      INCLUDE 'gen_event_info.cmn'
      external s_chamnum
      integer*4 s_chamnum
      
*     local variables
*
      logical ABORT
      character*12 here
      parameter (here='S_LINK_STUBS')
      character*(*) err
      integer*4  isp1,isp2,isp       !  loop index on space points
      integer*4  ihit                !  loop index on hits
      integer*4  spindex,spoint,duppoint
      integer*4  sptracks            !  number of tracks with this seed
      integer*4  stub_tracks(SNTRACKS_MAX)
      integer*4  numhits
      integer*4  itrack              !  loop index on tracks
      integer*4  track
      integer*4  track_space_points(SNTRACKS_MAX,smax_space_points+1)
      integer*4  tryflag             ! flag to loop over rest of points
      integer*4  newtrack            ! make a new track
      real*4 dposx,dposy,dposxp,dposyp
      real*4 y1,y2
*Derek added these next lines
      if (sbypass_track_eff_files.eq.0) then
       open(unit=16,file='scalers/strackstubs.txt',status='unknown',
     $      access='append')
      endif
      sstubtest = 0
*
      ABORT= .FALSE.
      err=' '
      SNTRACKS_FP=0
      if(ssingle_stub.eq.0 ) then
*     loop over all pairs of space points
       if(snspace_points_tot.ge.2) then ! return if less than 2 space points
        do isp1=1,snspace_points_tot-1  ! loop over all points
*     is this point all ready associated with a track
         tryflag=1
         if(SNTRACKS_FP.gt.0) then
          do itrack=1,SNTRACKS_FP           
           if(track_space_points(itrack,1).gt.0) then
            do isp2=1,track_space_points(itrack,1)
             if(track_space_points(itrack,isp2+1).eq.isp1) then
              tryflag=0                 ! space point all ready in a track
             endif                      ! end test on found point
            enddo
           endif                        ! end test of >0  point
          enddo                         ! end loop over tracks
         endif
*     if space point not all ready part of a track then look for matches
         if( tryflag .eq.1) then
          newtrack=1
          do isp2=isp1+1,snspace_points_tot
*     are these stubs in the same chamber. If so then skip
           if(s_chamnum(isp1).ne.s_chamnum(isp2)) then
*     does this stub match

*     since single chamber angular resolution is ~50mr, and the maximum y'
*     angle is about 30mr, use difference between y AT CHAMBERS, rather than
*     at focal plane.  (project back to chamber, to take out y' uncertainty).
            dposx = sbeststub(isp2,1)-sbeststub(isp1,1)
            y1=sbeststub(isp1,2)+sdc_1_zpos*sbeststub(isp1,4)
            y2=sbeststub(isp2,2)+sdc_2_zpos*sbeststub(isp2,4)
            dposy=y2-y1
            dposxp= sbeststub(isp2,3)-sbeststub(isp1,3)
            dposyp= sbeststub(isp2,4)-sbeststub(isp1,4)
******************************************************
* Derek added this for track tests...
            if (abs(dposx).LT.abs(sstubminx)) sstubminx = dposx
            if (abs(dposy).LT.abs(sstubminy)) sstubminy = dposy
            if (abs(dposxp).LT.abs(sstubminxp)) sstubminxp = dposxp
            if (abs(dposyp).LT.abs(sstubminyp)) sstubminyp = dposyp
            if (sbypass_track_eff_files.eq.0) then
             if (abs(sstubminx) .gt. sxt_track_criterion) then
              write(16,*) 'event # ',gen_event_ID_number,
     $             ' sstubminx = ',sstubminx
             endif
             if (abs(sstubminy) .gt. syt_track_criterion) then
              write(16,*) 'event # ',gen_event_ID_number,
     $             '  sstubminy =            ',sstubminy
             endif
             if (abs(sstubminxp) .gt. sxpt_track_criterion) then
              write(16,*) 'event # ',gen_event_ID_number,
     $             ' sstubminxp =                      ',sstubminxp
             endif
             if (abs(sstubminyp) .gt. sypt_track_criterion) then
              write(16,*) 'event # ',gen_event_ID_number,
     $             ' sstubminyp =                                 ',sstubminyp
             endif
             close(16)
            endif
******************************************************

            if      (abs(dposx) .lt. sxt_track_criterion
     $           .and. abs(dposy) .lt. syt_track_criterion
     $           .and. abs(dposxp).lt. sxpt_track_criterion
     $           .and. abs(dposyp).lt. sypt_track_criterion) then
             if(newtrack.eq.1) then
*Derek add this next line
              sstubtest=1

*     make a new track
              if(SNTRACKS_FP.lt.SNTRACKS_MAX) then ! are there too many 
               SNTRACKS_FP=SNTRACKS_FP+1 ! increment the number of tracks
               sptracks=1               ! one track with this seed
               stub_tracks(1)=SNTRACKS_FP
               track_space_points(SNTRACKS_FP,1)=2
               track_space_points(SNTRACKS_FP,2)=isp1
               track_space_points(SNTRACKS_FP,3)=isp2
               sx_sp1(sntracks_fp)=sbeststub(isp1,1)
               sx_sp2(sntracks_fp)=sbeststub(isp2,1)
               sy_sp1(sntracks_fp)=sbeststub(isp1,2)
               sy_sp2(sntracks_fp)=sbeststub(isp2,2)
               sxp_sp1(sntracks_fp)=sbeststub(isp1,3)
               sxp_sp2(sntracks_fp)=sbeststub(isp2,3)
               newtrack=0               ! make no more track in this loop
              endif                     ! end test on too many tracks
             else
*     check if there is another space point in same chamber
              itrack=0
              do while (itrack.lt.sptracks)
               itrack=itrack+1
               track=stub_tracks(itrack)
               spoint=0
               duppoint=0
               do isp=1,track_space_points(track,1)
                if(s_chamnum(isp2).eq.
     &               s_chamnum(track_space_points(track,isp+1))) then
                 spoint=isp
                endif
                if(isp2.eq.track_space_points(track,isp+1)) then
                 duppoint=1
                endif                   
               enddo                    ! end loop over sp in tracks with isp1
*     if there is no other space point in this chamber
*     add this space point to current track(2)
               if(duppoint.eq.0) then
                if(spoint.eq.0) then
                 spindex=track_space_points(track,1)+1
                 track_space_points(track,1)= spindex
                 track_space_points(track,spindex+1)=isp2
*     if there is another point in the same chamber in this track
*     create a new track with all the same space points except spoint
                else
                 if(SNTRACKS_FP.lt.SNTRACKS_MAX) then ! are there too many 
                  SNTRACKS_FP=SNTRACKS_FP+1 ! increment the number of tracks
                  sptracks= sptracks+1  ! one track with this seed
                  stub_tracks(sptracks) = SNTRACKS_FP
                  track_space_points(SNTRACKS_FP,1)
     $                 =track_space_points(track,1)
                  do isp=1,track_space_points(track,1)
                   if(isp.ne.spoint) then
                    track_space_points(SNTRACKS_FP,isp+1)=
     &                   track_space_points(track,isp+1)
                   elseif(isp.eq.spoint) then
                    track_space_points(SNTRACKS_FP,isp+1)= isp2
                   endif                ! end check for dup on copy
                  enddo                 ! end copy of track
                 endif                  ! end if on too many tracks
                endif                   ! end if on same chamber
               endif                    ! end if on  duplicate point
              enddo                     ! end do while over tracks with isp1
             endif
            endif
           endif                        ! end test on same chamber
          enddo                         ! end loop over new space points
         endif                          ! end test on tryflag
        enddo                           ! end outer loop over space points
       endif                            ! end if on <2 space points
      else                              ! if ssingle_stub .ne. 0
*     when ssingle_stub is set, make each space point a track
*     This will have poor resolution but may be appropriate for debugging
*     
       do isp1=1,snspace_points_tot     ! loop over all points
        if(SNTRACKS_FP.lt.SNTRACKS_MAX) then ! are there too many
         SNTRACKS_FP=SNTRACKS_FP+1      ! increment the number of tracks
         track_space_points(SNTRACKS_FP,1)= 1
         track_space_points(SNTRACKS_FP,2)= isp1
        endif                           ! end if on too many tracks
       enddo                            ! end loop over all space points
      endif                             ! end test on ssingle_stub
*     
*     now list all hits on a track
      if(SNTRACKS_FP.gt.0)   then
       do itrack=1,SNTRACKS_FP          ! loop over all tracks
        SNTRACK_HITS(itrack,1)=0
        do isp1=1,track_space_points(itrack,1)
         spindex=track_space_points(itrack,isp1+1)
         numhits=sspace_point_hits(spindex,1)
         do ihit=1,numhits
          if(SNTRACK_HITS(itrack,1).lt.SNTRACKHITS_MAX) then 
           SNTRACK_HITS(itrack,1)=SNTRACK_HITS(itrack,1)+1
           SNTRACK_HITS(itrack,SNTRACK_HITS(itrack,1)+1)=
     &          sspace_point_hits(spindex,ihit+2)                
          endif                         ! end test on too many hits
         enddo                          ! end loop over space point hits
        enddo                           ! end loop over space points
       enddo                            ! end loop over all tracks
      endif
      if(sdebuglinkstubs.ne.0) then
       call s_print_links
      endif
      return
      end
*********
*     Local Variables:
*     mode: fortran
*     fortran-if-indent: 1
*     fortran-do-indent: 1
*     End:
