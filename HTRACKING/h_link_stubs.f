
      subroutine H_LINK_STUBS(ABORT,err)
*     This subroutine compares all the space-point-stubs found in
*     H_LEFT_RIGHT.f and links together stubs to form tracks.
*     The criterion are that the stubs are in different chambers and
*     each of the four track parameters are within limit:
*     hxt_track_criterion      for x_t
*     hyt_track_criterion      for y_t
*     htx_track_criterion      for t_x
*     hty_track_criterion      for t_y
*
*     d.f. geesaman           17 January 1994
* $Log$
* Revision 1.2  1994/06/06 16:37:57  cdaq
* Add switch to include single stub tracks
*
* Revision 1.1  1994/02/19  06:15:28  cdaq
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
*                  5) If hsingle_stub is set, make a track of all single
*                     stubs.
*
      implicit none
      include "gen_data_structures.cmn"
      include "hms_tracking.cmn"
      external h_chamnum
      integer*4 h_chamnum
      
*     local variables
*
      logical ABORT
      character*50 here
      parameter (here='H_LINK_STUBS')
      character*(*) err
      integer*4  isp1,isp2,isp       !  loop index on space points
      integer*4  ihit                !  loop index on hits
      integer*4  spindex,spoint,duppoint
      integer*4  sptracks            !  number of tracks with this seed
      integer*4  stub_tracks(HNTRACKS_MAX)
      integer*4  numhits
      integer*4  itrack              !  loop index on tracks
      integer*4  track
      integer*4  track_space_points(HNTRACKS_MAX,hmax_space_points+1)
      integer*4  tryflag             ! flag to loop over rest of points
      integer*4  newtrack            ! make a new track
*
      ABORT= .FALSE.
      err=':'
      HNTRACKS_FP=0
      if(hsingle_stub.eq.0 ) then
*     loop over all pairs of space points
       if(hnspace_points_tot.ge.2) then   ! return if less than 2 space points
        do isp1=1,hnspace_points_tot-1   ! loop over all points
*     is this point all ready associated with a track
         tryflag=1
         if(HNTRACKS_FP.gt.0) then
          do itrack=1,HNTRACKS_FP           
           if(track_space_points(itrack,1).gt.0) then
             do isp2=1,track_space_points(itrack,1)
              if(track_space_points(itrack,isp2+1).eq.isp1) then
               tryflag=0                   ! space point all ready in a track
              endif                        ! end test on found point
             enddo
           endif                           ! end test of >0  point
          enddo                            ! end loop over tracks
         endif
*     if space point not all ready part of a track then look for matches
         if( tryflag .eq.1) then
          newtrack=1
          do isp2=isp1+1,hnspace_points_tot
*     are these stubs in the same chamber. If so then skip
           if(h_chamnum(isp1).ne.h_chamnum(isp2)) then
*     does this stub match
            if(abs(hbeststub(isp1,1)-hbeststub(isp2,1)).lt.hxt_track_criterion
     &   .and. abs(hbeststub(isp1,2)-hbeststub(isp2,2)).lt.hyt_track_criterion
     &   .and. abs(hbeststub(isp1,3)-hbeststub(isp2,3)).lt.hxpt_track_criterion
     &   .and. abs(hbeststub(isp1,4)-hbeststub(isp2,4)).lt.hxpt_track_criterion
     &      ) then
             if(newtrack.eq.1) then         
*     make a new track
              if(HNTRACKS_FP.lt.HNTRACKS_MAX) then    ! are there too many 
                HNTRACKS_FP=HNTRACKS_FP+1      ! increment the number of tracks
                sptracks=1                     ! one track with this seed
                stub_tracks(1)=HNTRACKS_FP
                track_space_points(HNTRACKS_FP,1)=2
                track_space_points(HNTRACKS_FP,2)=isp1
                track_space_points(HNTRACKS_FP,3)=isp2
                newtrack=0                     ! make no more track in this loop
              endif                            ! end test on too many tracks
             else
*     check if there is another space point in same chamber
              itrack=0
              do while (itrack.lt.sptracks)
                itrack=itrack+1
                track=stub_tracks(itrack)
                spoint=0
                duppoint=0
                do isp=1,track_space_points(track,1)
                 if(h_chamnum(isp2).eq.
     &             h_chamnum(track_space_points(track,isp+1))) then
                    spoint=isp
                 endif
                 if(isp2.eq.track_space_points(track,isp+1)) then
                    duppoint=1
                 endif                   
                enddo                       ! end loop over sp in tracks with isp1
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
                   if(HNTRACKS_FP.lt.HNTRACKS_MAX) then    ! are there too many 
                    HNTRACKS_FP=HNTRACKS_FP+1   ! increment the number of tracks
                    sptracks= sptracks+1        ! one track with this seed
                    stub_tracks(sptracks) = HNTRACKS_FP
                   track_space_points(HNTRACKS_FP,1)=track_space_points(track,1)
                     do isp=1,track_space_points(track,1)
                      if(isp.ne.spoint) then
                       track_space_points(HNTRACKS_FP,isp+1)=
     &                  track_space_points(track,isp+1)
                      elseif(isp.eq.spoint) then
                        track_space_points(HNTRACKS_FP,isp+1)= isp2
                      endif                 ! end check for dup on copy
                     enddo                  ! end copy of track
                   endif                    ! end if on too many tracks
                  endif                     ! end if on same chamber
                 endif                      ! end if on  duplicate point
              enddo                         ! end do while over tracks with isp1
             endif
            endif
           endif                            ! end test on same chamber
          enddo                             ! end loop over new space points
         endif                              ! end test on tryflag
        enddo                               ! end outer loop over space points
       endif                                ! end if on <2 space points
      else                                  ! if hsingle_stub .ne. 0
*      when hsingle_stub is set, make each space point a track
*      This will have poor resolution but may be appropriate for debugging
*
        do isp1=1,hnspace_points_tot        ! loop over all points
           if(HNTRACKS_FP.lt.HNTRACKS_MAX) then    ! are there too many 
              HNTRACKS_FP=HNTRACKS_FP+1   ! increment the number of tracks
              track_space_points(HNTRACKS_FP,1)= 1
              track_space_points(HNTRACKS_FP,2)= isp1
           endif                    ! end if on too many tracks
        enddo                               ! end loop over all space points
      endif                                 ! end test on hsingle_stub 
*     now list all hits on a track
      if(HNTRACKS_FP.gt.0)   then
        do itrack=1,HNTRACKS_FP             ! loop over all tracks
          HNTRACK_HITS(itrack,1)=0
          do isp1=1,track_space_points(itrack,1)
             spindex=track_space_points(itrack,isp1+1)
             numhits=hspace_point_hits(spindex,1)
              do ihit=1,numhits
               if(HNTRACK_HITS(itrack,1).lt.HNTRACKHITS_MAX) then 
                 HNTRACK_HITS(itrack,1)=HNTRACK_HITS(itrack,1)+1
                 HNTRACK_HITS(itrack,HNTRACK_HITS(itrack,1)+1)=
     &             hspace_point_hits(spindex,ihit+2)                
               endif                       ! end test on too many hits
              enddo                        ! end loop over space point hits
          enddo                            ! end loop over space points
        enddo                              ! end loop over all tracks
      endif
      if(hdebuglinkstubs.ne.0) then
        call h_print_links
      endif
      return
      end
*********
*     mode: Fortran
*     fortran-if-indent: 1
*     fortran-do-indent: 1
*     End:
