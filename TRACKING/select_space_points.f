      subroutine select_space_points(nspace_point_len,
     & nspace_points,space_points,space_point_hits,min_hits,min_combos)
*     This routine goes through the list of space_points and space_point_hits
*     found by find_space_points and only accepts those with 
*     number of hits > min_hits
*     number of combinations > min_combos
*     dfg              30 august 1993
* $Log$
* Revision 1.2  1994/02/23 13:52:40  cdaq
* (SAW) Change 2nd arg of space_points_hits declaration from 1 to *
*
* Revision 1.1  1994/02/21  16:44:23  cdaq
* Initial revision
*
      implicit none
*     inputs
      integer*4 nspace_point_len        ! dimension variable for two-d arrays
      integer*4 nspace_points           ! number of input points     
					! on return it is the number of valid
                                        ! space points
      integer*4 space_points(nspace_point_len,2)
      integer*4 space_point_hits(nspace_point_len,*)
      integer*4 min_hits                ! minimum number of hits in valid point
      integer*4 min_combos              ! minimum number of combos
*
*     outputs
*     note nspace_points, space_points, and space_point_hits are all 
*     modified by the action of this routine
*     local variables
      integer*4 space_point_count,ploop,hloop
      if (nspace_points .gt. 0) then
        space_point_count=0
        do ploop=1,nspace_points
         if(space_point_hits(ploop,1).ge. min_hits .and.
     &      space_point_hits(ploop,2).ge. min_combos) then
            space_point_count=space_point_count+1
            space_points(space_point_count,1)=space_points(ploop,1)
            space_points(space_point_count,2)=space_points(ploop,2)
            do hloop=1,space_point_hits(ploop,1)+2
               space_point_hits(space_point_count,hloop)=
     &                 space_point_hits(ploop,hloop)
            enddo
         endif
        enddo
        nspace_points=space_point_count
      endif
      return
      end
