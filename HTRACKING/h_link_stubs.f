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
* $Log: h_link_stubs.f,v $
* Revision 1.9  2003/04/01 15:21:33  jones
*  minor change
*
* Revision 1.8  2003/04/01 13:49:26  jones
* Modifications to tracking codes.
* Mainly fix problems at high rates. (M. E. Christy)
*
* Revision 1.7  1996/08/30 19:58:15  saw
* (DVW) Added some track tests
*
* Revision 1.6  1996/01/16 22:02:16  cdaq
* (JRA)
*
* Revision 1.5  1995/08/31 14:53:08  cdaq
* (JRA) Calculate dpos (pos. track - pos. hit) variables
*
* Revision 1.4  1995/05/22  19:39:15  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.3  1995/04/06  19:30:18  cdaq
* (JRA) Fix typo
*
* Revision 1.2  1994/06/06  16:37:57  cdaq
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
      include "hms_data_structures.cmn"
      include "hms_tracking.cmn"
      include "hms_id_histid.cmn"
      INCLUDE 'hms_track_histid.cmn'    !TEMP. JUNK
      include 'hms_bypass_switches.cmn'
      INCLUDE 'gen_event_info.cmn'
      external h_chamnum
      integer*4 h_chamnum
      
*     local variables
*
      logical ABORT
      character*12 here
      parameter (here='H_LINK_STUBS')
      character*(*) err
      integer*4  isp1,isp2,isp       !  loop index on space points
      integer*4  ihit                !  loop index on hits
      integer*4  spindex,spoint,duppoint
      integer*4  sptracks            !  number of tracks with this seed
      integer*4  stub_tracks(hntracks_max)
      integer*4  numhits
      integer*4  itrack              !  loop index on tracks
      integer*4  track
      integer*4  track_space_points(hntracks_max,hmax_space_points+1)
      integer*4  tryflag             ! flag to loop over rest of points
      integer*4  newtrack            ! make a new track
      real*4 dposx,dposy,dposxp,dposyp
      if (hbypass_track_eff_files.eq.0) then
       open(unit=13,file='scalers/htrackstubs.txt',status='unknown',
     $      access='append')
      endif
      hstubtest = 0
c
      if (HNTRACKS_MAX_FP .eq. 0) HNTRACKS_MAX_FP = 10 ! in case not set in param file.
      if (HNTRACKS_MAX_FP .gt. HNTRACKS_MAX) HNTRACKS_MAX_FP = HNTRACKS_MAX ! in case set too high  in param file.
c
*
      ABORT= .FALSE.
      err=' '
      hntracks_fp=0
       if(hdebugprintrawdc.ne.0 ) then
              write(hluno,'(a,i5,a,i5)' ) 'In h_link_stubs ev # =',gen_event_ID_number,' num of sp pts = ',hnspace_points_tot
        endif
      if(hsingle_stub.eq.0 ) then
*     loop over all pairs of space points
       if(hnspace_points_tot.ge.2) then ! return if less than 2 space points
        do isp1=1,hnspace_points_tot-1  ! loop over all points
*     is this point all ready associated with a track
       if(hdebugprintrawdc.ne.0 ) then
              write(hluno,'(a,2i5)' ) 'Looping thru sp pt =',isp1,hntracks_fp
        endif
         tryflag=1
         if(hntracks_fp.gt.0) then
          do itrack=1,hntracks_fp           
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
       if(hdebugprintrawdc.ne.0 ) then
              write(hluno,'(a,2i5)' ) 'Try flag =',tryflag
        endif
         if( tryflag .eq.1) then
          newtrack=1
          do isp2=isp1+1,hnspace_points_tot
       if(hdebugprintrawdc.ne.0 ) then
              write(hluno,'(a,2i5)' ) '2nd Loop thru sp pt = ',isp2
        endif
*     are these stubs in the same chamber. If so then skip
           if(h_chamnum(isp1).ne.h_chamnum(isp2)) then
*     does this stub match

            dposx = hbeststub(isp2,1)-hbeststub(isp1,1)
            dposy = hbeststub(isp2,2)-hbeststub(isp1,2)
            dposxp= hbeststub(isp2,3)-hbeststub(isp1,3)
            dposyp= hbeststub(isp2,4)-hbeststub(isp1,4)

******************************************************
            if (abs(dposx).LT.abs(hstubminx)) hstubminx = dposx
            if (abs(dposy).LT.abs(hstubminy)) hstubminy = dposy
            if (abs(dposxp).LT.abs(hstubminxp)) hstubminxp = dposxp
            if (abs(dposyp).LT.abs(hstubminyp)) hstubminyp = dposyp
      if (hbypass_track_eff_files.eq.0) then
       if (abs(hstubminx) .gt. hxt_track_criterion) then
        write(13,*) 'event # ',gen_event_ID_number,
     $       ' hstubminx = ',hstubminx
       endif
       if (abs(hstubminy) .gt. hyt_track_criterion) then
        write(13,*) 'event # ',
     $       ' hstubminy =            ',hstubminy
       endif
       if (abs(hstubminxp) .gt. hxpt_track_criterion) then
        write(13,*) 'event # ',gen_event_ID_number,
     $       ' hstubminxp =                      ',hstubminxp
       endif
       if (abs(hstubminyp) .gt. hypt_track_criterion) then
        write(13,*) 'event # ',gen_event_ID_number,
     $       ' hstubminyp =                                 ',hstubminyp
       endif
       close(13)
      endif
******************************************************
       if(hdebugprintrawdc.ne.0 ) then
              write(hluno,'(a,4(f10.5,1x))' ) 'criterion='
     >,hxt_track_criterion, hyt_track_criterion
     >,hxpt_track_criterion, hypt_track_criterion
              write(hluno,'(a,4(f10.5,1x))' ) 'dpos =',dposx,dposy,dposxp,dposyp
              write(hluno,'(a,4(f10.5,1x))' ) 'stub 1 ='
     >,hbeststub(isp1,1),hbeststub(isp1,2),hbeststub(isp1,3),hbeststub(isp1,4)
              write(hluno,'(a,4(f10.5,1x))' ) 'stub 2 ='
     >,hbeststub(isp2,1),hbeststub(isp2,2),hbeststub(isp2,3),hbeststub(isp2,4)
        endif
             if      (abs(dposx) .lt. hxt_track_criterion
     &         .and. abs(dposy) .lt. hyt_track_criterion
     &         .and. abs(dposxp).lt. hxpt_track_criterion
     &         .and. abs(dposyp).lt. hypt_track_criterion) then
             if(newtrack.eq.1) then         
             hstubtest=1
*     make a new track
              if(hntracks_fp.lt.hntracks_max_fp) then ! are there too many 
               hntracks_fp=hntracks_fp+1 ! increment the number of tracks
               sptracks=1               ! one track with this seed
               stub_tracks(1)=hntracks_fp
               track_space_points(hntracks_fp,1)=2
               track_space_points(hntracks_fp,2)=isp1
               track_space_points(hntracks_fp,3)=isp2
               hx_sp1(hntracks_fp)=hbeststub(isp1,1)
               hx_sp2(hntracks_fp)=hbeststub(isp2,1)
               hy_sp1(hntracks_fp)=hbeststub(isp1,2)
               hy_sp2(hntracks_fp)=hbeststub(isp2,2)
               hxp_sp1(hntracks_fp)=hbeststub(isp1,3)
               hxp_sp2(hntracks_fp)=hbeststub(isp2,3)
               newtrack=0               ! make no more tracks in this loop
        if(hdebugprintrawdc.ne.0 ) then
              write(hluno,'(a,3i5,6(f10.5,1x))' ) 'track =', hntracks_fp,isp1,isp2
     >,hbeststub(isp1,1),hbeststub(isp1,2),hbeststub(isp1,3)
     >,hbeststub(isp2,1),hbeststub(isp2,2),hbeststub(isp2,3)
        endif
              else                      !!  MEC - added the next 3 lines to 
        if(hdebugprintrawdc.ne.0 ) then
              write(hluno,'(a,i5)' ) 
     >'Too many tracks set hntracks_fp to zero  = ',hntracks_fp 
        endif
               hntracks_fp = 0          !!  fail events with more than the 
               return                   !!  Max # of allowed tracks.
              endif                     ! end test on too many tracks
             else
*     check if there is another space point in same chamber
              itrack=0
              do while (itrack.lt.sptracks)
               if(hdebugprintrawdc.ne.0 )  then
                  write(hluno,'(a,2i5)') " itrack , sptracks = ",itrack,sptracks
               endif
               itrack=itrack+1
               track=stub_tracks(itrack)
               spoint=0
               duppoint=0
               do isp=1,track_space_points(track,1)
                if(h_chamnum(isp2).eq.
     &               h_chamnum(track_space_points(track,isp+1))) then
                 spoint=isp
                endif
                if(isp2.eq.track_space_points(track,isp+1)) then
                 duppoint=1
                endif                   
        if(hdebugprintrawdc.ne.0 ) then
              write(hluno,'(a,3i5)') " sp pt spoint duppoint =",isp,spoint,duppoint 
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
                 if(hntracks_fp.lt.hntracks_max_fp) then ! are there too many 
                  hntracks_fp=hntracks_fp+1 ! increment the number of tracks
        if(hdebugprintrawdc.ne.0 ) then
              write(hluno,'(a,i5)' ) 'Foun antoher track = ',hntracks_fp 
        endif
                  sptracks= sptracks+1  ! one track with this seed
                  stub_tracks(sptracks) = hntracks_fp
                  track_space_points(hntracks_fp,1)
     $                 =track_space_points(track,1)
                  do isp=1,track_space_points(track,1)
                   if(isp.ne.spoint) then
                    track_space_points(hntracks_fp,isp+1)=
     &                   track_space_points(track,isp+1)
                   elseif(isp.eq.spoint) then
                    track_space_points(hntracks_fp,isp+1)= isp2
                   endif                ! end check for dup on copy
                  enddo                 ! end copy of track
                 else                      !!  MEC - added the next 3 lines to 
        if(hdebugprintrawdc.ne.0 ) then
              write(hluno,'(a,i5)' ) 
     >'Too many tracks set hntracks_fp to zero  = ',hntracks_fp 
        endif
                  hntracks_fp = 0          !!  fail events with more than the 
                  return                   !!  Max # of allowed tracks.
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
      else                              ! if hsingle_stub .ne. 0
*     when hsingle_stub is set, make each space point a track
*     This will have poor resolution but may be appropriate for debugging
*
       do isp1=1,hnspace_points_tot     ! loop over all points
        if(hntracks_fp.lt.hntracks_max_fp) then ! are there too many 
         hntracks_fp=hntracks_fp+1      ! increment the number of tracks
         track_space_points(hntracks_fp,1)= 1
         track_space_points(hntracks_fp,2)= isp1
        else                      !!  MEC - added the next 3 lines to 
         hntracks_fp = 0          !!  fail events with more than the 
         return                   !!  Max # of allowed tracks.
        endif                           ! end if on too many tracks
       enddo                            ! end loop over all space points
      endif                             ! end test on hsingle_stub 
*     now list all hits on a track
      if(hntracks_fp.gt.0)   then
       do itrack=1,hntracks_fp          ! loop over all tracks
        hntrack_hits(itrack,1)=0
        do isp1=1,track_space_points(itrack,1)
         spindex=track_space_points(itrack,isp1+1)
         numhits=hspace_point_hits(spindex,1)
         do ihit=1,numhits
          if(hntrack_hits(itrack,1).lt.hntrackhits_max) then 
           hntrack_hits(itrack,1)=hntrack_hits(itrack,1)+1
           hntrack_hits(itrack,hntrack_hits(itrack,1)+1)=
     &          hspace_point_hits(spindex,ihit+2)                
           htrack_leftright(itrack,hntrack_hits(itrack,1))=
     $          hspace_point_leftright(spindex,ihit)
          endif                         ! end test on too many hits
         enddo                          ! end loop over space point hits
        enddo                           ! end loop over space points
       enddo                            ! end loop over all tracks
      endif
      if(hdebuglinkstubs.ne.0) then
       call h_print_links
      endif
      return
      end
*********
*     Local Variables:
*     mode: Fortran
*     fortran-if-indent: 1
*     fortran-do-indent: 1
*     End:
