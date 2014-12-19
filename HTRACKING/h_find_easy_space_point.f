      subroutine h_find_easy_space_point(ncham_hits,
     &  hit_num, wire_center, ipln,space_point_criterion,
     &  nspace_point_len,
     &  y_hit,yp_hit,easy_space_point,
     &  nspace_points,space_points,space_point_hits)
*
* $Log: h_find_easy_space_point.f,v $
* Revision 1.1  1995/10/25 15:00:13  cdaq
* Initial revision
*
*
* Simplified HMS find_space_point routine.  It is given all y hits, and checks
* to see if all x-like hits are close enough together to make a space point.
* 
      implicit none
*
      INCLUDE 'hms_data_structures.cmn'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      INCLUDE 'hms_tracking.cmn'
      INCLUDE 'hms_geometry.cmn'

*     input
      integer*4 ncham_hits		! total number of hits in chamber
      integer*4 hit_num(*)		! array of hit numbers
      integer*4 ipln(*)			! array of plane numbers for each hit
      real*4 wire_center(*)		! array of wire coordinates for hits
      real*4 space_point_criterion	! squared distance limit for points
      integer*4 nspace_point_len	! dimension of space point arrays
      integer*4 y_hit,yp_hit		!hit # of y and y' planes
      logical easy_space_point
*
*     outputs
      integer*4 nspace_points      ! number of space points in chamber
      real*4 space_points(nspace_point_len,2)     ! xt, yt of each space point
      integer*4 space_point_hits(nspace_point_len,*)
*                                  ! hit numbers for each space point
* internal Variables,
      integer*4 k
      integer*4 num_xhits
      real*4 xt,yt
      real*4 x_pos(hmax_hits_per_point)
      real*4 max_dist
*
*
      yt=(wire_center(y_hit)+wire_center(yp_hit))/2
      xt=0.
      num_xhits=0
      nspace_points = 0
      max_dist = sqrt(space_point_criterion/2)
*

* loop over all hits, find x of space point.
      do k = 1, ncham_hits
        if (k.ne.y_hit .and. k.ne.yp_hit) then !x-like hits
          x_pos(k) = ( wire_center(k)-yt*hysp(ipln(k)) )/hxsp(ipln(k))
          xt = xt + x_pos(k)
          num_xhits = num_xhits + 1
        else
          x_pos(k) = 0.
        endif
      enddo
      xt = xt / float(max(1,num_xhits))
      if(hdebugprintrawdc.ne.0 ) then
       write(hluno,*) 'mean x = ',xt,' max_dist = ',max_dist
      endif    

      easy_space_point = .true.
      do k = 1, ncham_hits
        if (k.ne.y_hit .and. k.ne.yp_hit) then
      if(hdebugprintrawdc.ne.0 ) then
       write(hluno,*) 'Test x like points ',k, x_pos(k),abs(xt-x_pos(k))
      endif               
          if (abs(xt-x_pos(k)).ge.max_dist) easy_space_point=.false.
        endif
      enddo

* If easy_space_point is true, all hits were on the space points.
      if (easy_space_point) then
        nspace_points = 1
        space_point_hits(1,1) = ncham_hits
        space_point_hits(1,2) = 0          !no combos.
        do k = 1, ncham_hits
          space_point_hits(1,k+2) = hit_num(k)
        enddo
        space_points(1,1)=xt
        space_points(1,2)=yt
       if(hdebugprintrawdc.ne.0 ) then
         write(hluno,*) 'space point x y ',space_points(1,1),space_points(1,2)
        endif
      endif

      return
      end
