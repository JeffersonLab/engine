      subroutine find_space_points(ncham_hits,
     &  hit_number, wire_center, plane_number,space_point_criterion,
     &  xsp,ysp,nspace_point_len,
     &  nspace_points,space_points,space_point_hits)
*       Created D.F. Geesaman     Sept 1993       
* $Log$
* Revision 1.2  1994/02/23 13:34:09  cdaq
* (SAW) Change 2nd arg of space_points_hits declaration from 1 to *
*
* Revision 1.1  1994/02/19  06:12:06  cdaq
* Initial revision
*
*
*     This algorithm finds space points in a wire chamber by finding the
*     intersection of each pair of hits in non-parallel planes,
*     then combining all two-hit space points that are within a squared
*     distance space_point_criterion.
      implicit none
*     input
      integer*4 ncham_hits         ! total number of hits in chamber
      integer*4 hit_number(*)      ! array of hit numbers 
      integer*4 plane_number(*)    ! array of plane numbers for each hit
      real*4 wire_center(*)        ! array of wire coordinates for hits
      real*4 xsp(*),ysp(*)         ! arrays of geometrical factors
                                   ! see pattern recognition writeup
      real*4    space_point_criterion   ! squared distance limit for points
      integer*4 nspace_point_len   ! dimension of space point arrays
*
*     outputs
      integer*4 nspace_points      ! number of space points in chamber
      real*4 space_points(nspace_point_len,2)     ! xt, yt of each space point
      integer*4 space_point_hits(nspace_point_len,*)
*                                  ! hit numbers for each space point
                                   ! (,1) number of hits
                                   ! (,2) number of combanations attached
                                   !      to the point
                                   ! (,3).. hits associated with space point
*
*
*     internal variables
      integer*4 ipair1,ipair2,icombo,i   ! do loop variables
      integer*4 loopsp,loop,hit(4),hit_point
      integer*4 add_flag,iflag(4)
      integer*4 max_number_pairs            ! max number of pairs of points
      parameter (max_number_pairs=1000)
      integer*4 pair(max_number_pairs,2)    ! hit1 hit2
      integer*4 ntest_points             ! number of valid combinations
      integer*4 plane1,plane2
      real*4 test_points(max_number_pairs,2) ! x and y of each test point
      real*4 determinate,xt,yt
      integer*4 max_number_comb
      parameter (max_number_comb=10*max_number_pairs)
      integer*4 ncombo
      integer*4 combos(max_number_pairs,2)   ! pair1 and pair2 of each combo
      real*4 sqdistance_test
*                           
      nspace_points=0
      ntest_points=0
*
*     loop over all pairs of intersecting wires and calculate position
      do ipair1=1,ncham_hits-1
        do ipair2=ipair1+1,ncham_hits
         if(ntest_points.lt.max_number_pairs) then 
           plane1=plane_number(ipair1)
           plane2=plane_number(ipair2)
           determinate=xsp(plane1)*ysp(plane2)-ysp(plane1)*xsp(plane2)
            if(abs(determinate) .gt. 1e-4 ) then
             ntest_points=ntest_points+1
             pair(ntest_points,1)=hit_number(ipair1)
             pair(ntest_points,2)=hit_number(ipair2)
             test_points(ntest_points,1)= 
     &        (wire_center(ipair1)*ysp(plane2)-wire_center(ipair2)*ysp(plane1))
     &        / determinate
             test_points(ntest_points,2)= 
     &        (wire_center(ipair2)*xsp(plane1)-wire_center(ipair1)*xsp(plane2))
     &        / determinate
           endif                   ! end if for indeterminate planes
         endif                     ! end test on too many pairs 
        enddo                      ! end loop over pair2
      enddo                        ! end loop over pair1
*
*     loop over all test_points and calculate squared distance
*     for each combination
*
      ncombo=0
      do ipair1=1,ntest_points-1
        do ipair2=ipair1+1,ntest_points
          if(ncombo.lt.max_number_comb) then
            sqdistance_test=
     &               (test_points(ipair1,1)-test_points(ipair2,1))**2 +
     &               (test_points(ipair1,2)-test_points(ipair2,2))**2
            if(sqdistance_test.le.space_point_criterion) then 
                ncombo=ncombo+1
                combos(ncombo,1)=ipair1
                combos(ncombo,2)=ipair2                    
            endif
          endif                    ! end test on too many combos
        enddo                      ! end loop over pair2
      enddo                        ! end loop over pair1
*
*     loop over all valid combinations and build space points
      if(ncombo.gt.0) then
        do icombo=1,ncombo
*     get hits in combo
         hit(1)=pair(combos(icombo,1),1)
         hit(2)=pair(combos(icombo,1),2)
         hit(3)=pair(combos(icombo,2),1)
         hit(4)=pair(combos(icombo,2),2)
*     get average space point xt, yt
         xt=(test_points(combos(icombo,1),1)+test_points(combos(icombo,2),1))/2
         yt=(test_points(combos(icombo,1),2)+test_points(combos(icombo,2),2))/2
*
*     loop over space_points
         if(nspace_points.gt.0) then
          loopsp=1
          add_flag=1
          do while (loopsp .le. nspace_points)
            if(space_point_hits(loopsp,1).gt.0)  then
              sqdistance_test=(xt-space_points(loopsp,1))**2 +
     &                   (yt-space_points(loopsp,2))**2
*     I want to be careful if sqdistance is between 1 and 
*     3 space_point_criterion. Let me ignore not add a new point then
              if(sqdistance_test.lt. (3.*space_point_criterion)) then
                 add_flag=0        ! do not add new space point
              endif
              if(sqdistance_test.lt.space_point_criterion) then
*     This is a real match.
*     Add the new hits to existing space point
               iflag(1)=0
               iflag(2)=0
               iflag(3)=0
               iflag(4)=0
                 do loop=1,space_point_hits(loopsp,1)
                  do i=1,4 
                   if(space_point_hits(loopsp,loop+2).eq.hit(i)) then
                      iflag(i)=1
                   endif
                  enddo         ! end loop on i  
                 enddo           ! end loop over hits in space point
                 do i=1,4
                  if(iflag(i).eq.0) then
                     hit_point=space_point_hits(loopsp,1)+1
                     space_point_hits(loopsp,1)=hit_point
                     space_point_hits(loopsp,hit_point+2)=hit(i)
                  endif
                 enddo    ! end loop over 4 hits
*              increment number of combos contributing to this space point
               space_point_hits(loopsp,2)=space_point_hits(loopsp,2)+1
*              terminate loop since this combo can only belong to one
*              space point
               loopsp=nspace_points+1    
              endif
            endif                  ! end check on number of hits
            loopsp=loopsp+1        ! increment loop counter on return
          enddo                    ! end do while loop over space points
*     create a new space point if more than 2*space_point_criteria
         if(nspace_points.lt.nspace_point_len) then
              if(add_flag.gt.0) then
                 nspace_points=nspace_points+1
                 space_point_hits(nspace_points,1)=2
                 space_point_hits(nspace_points,2)=1
                 space_point_hits(nspace_points,3)=hit(1)
                 space_point_hits(nspace_points,4)=hit(2)
                 space_points(nspace_points,1)=xt
                 space_points(nspace_points,2)=yt
                 if(hit(1).ne.hit(3) .and. hit(2) .ne. hit(3))   then
                       hit_point=space_point_hits(nspace_points,1)+1
                       space_point_hits(nspace_points,1)=hit_point
                       space_point_hits(nspace_points,hit_point+2)=hit(3)
                 endif              !
                 if(hit(1).ne.hit(4) .and. hit(2) .ne. hit(4))   then
                       hit_point=space_point_hits(nspace_points,1)+1
                       space_point_hits(nspace_points,1)= hit_point
                       space_point_hits(nspace_points,hit_point+2)=hit(4)
                 endif         
              endif             ! endif on check if too many space points
            endif               ! endif to add point on add_flag
         else
*    create first space point
            nspace_points=1
            space_point_hits(nspace_points,1)=2
            space_point_hits(nspace_points,2)=1
            space_point_hits(nspace_points,3)=hit(1)
            space_point_hits(nspace_points,4)=hit(2)
            space_points(nspace_points,1)=xt
            space_points(nspace_points,2)=yt
            if(hit(1).ne.hit(3) .and. hit(2) .ne. hit(3))   then
               hit_point=space_point_hits(nspace_points,1)+1
               space_point_hits(nspace_points,1)=hit_point
               space_point_hits(nspace_points,hit_point+2)=hit(3)
            endif              !
            if(hit(1).ne.hit(4) .and. hit(2) .ne. hit(4))   then
               hit_point=space_point_hits(nspace_points,1)+1
               space_point_hits(nspace_points,1)= hit_point
               space_point_hits(nspace_points,hit_point+2)=hit(4)
            endif         
         endif                    ! end check on 0 space points
        enddo                     ! end loop over combos
      endif                       ! end check if no valid combos
      return
      end
     

*234567890123456789012345678901234567890123456789012345678901234567890123456789
