      subroutine s_find_easy_space_point(ncham_hits,
     &  hit_num, wire_center, ipln,space_point_criterion,
     &  nspace_point_len,
     &  x_hit,xp_hit,easy_space_point,
     &  nspace_points,space_points,space_point_hits)
*
* $Log: s_find_easy_space_point.f,v $
* Revision 1.1  1995/10/26 14:18:53  cdaq
* Initial revision
*
* Simplified SOS find_space_point routine.  It is given all x hits, and checks
* to see if all y-like hits are close enough together to make a space point.
* 
      implicit none
*
      INCLUDE 'sos_data_structures.cmn'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      INCLUDE 'sos_tracking.cmn'
      INCLUDE 'sos_geometry.cmn'

*     input
      integer*4 ncham_hits		! total number of hits in chamber
      integer*4 hit_num(*)		! array of hit numbers
      integer*4 ipln(*)			! array of plane numbers for each hit
      real*4 wire_center(*)		! array of wire coordinates for hits
      real*4 space_point_criterion	! squared distance limit for points
      integer*4 nspace_point_len	! dimension of space point arrays
      integer*4 x_hit,xp_hit		! hit # of x and x' planes
      logical easy_space_point
*
*     outputs
      integer*4 nspace_points      ! number of space points in chamber
      real*4 space_points(nspace_point_len,2)     ! xt, yt of each space point
      integer*4 space_point_hits(nspace_point_len,*)
*                                  ! hit numbers for each space point
* internal Variables,
      integer*4 k
      integer*4 num_yhits
      real*4 xt,yt
      real*4 y_pos(smax_hits_per_point)
      real*4 max_dist
*
*
      xt=(wire_center(x_hit)+wire_center(xp_hit))/2
      yt=0.
      num_yhits=0
      nspace_points = 0
      max_dist = sqrt(space_point_criterion/2)
*

* loop over all hits, find y of space point.
      do k = 1, ncham_hits
        if (k.ne.x_hit .and. k.ne.xp_hit) then !y-like hits
          y_pos(k) = ( wire_center(k)-xt*sxsp(ipln(k)) )/sysp(ipln(k))
          yt = yt + y_pos(k)
          num_yhits = num_yhits + 1
        else
          y_pos(k) = 0.
        endif
      enddo
      yt = yt / float(max(1,num_yhits))

      easy_space_point = .true.
      do k = 1, ncham_hits
        if (k.ne.x_hit .and. k.ne.xp_hit) then
          if (abs(yt-y_pos(k)).ge.max_dist) easy_space_point=.false.
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
      endif

      return
      end
