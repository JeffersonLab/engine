      subroutine H_PATTERN_RECOGNITION(ABORT,err)
*--------------------------------------------------------
*-
*-   Purpose and Methods :  Finds HMS Space points 
*-
*-      Required Input BANKS     HMS_DECODED_DC
*-
*-      Output BANKS             HMS_FOCAL_PLANE
*-                               HMS_DECODED_DC hit coordinates
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
*-   Created 30-AUG-1993   D. F. Geesaman
*-   Modified 19-JAN-1994  DFG    Include standard error form
* $Log$
* Revision 1.2  1994/02/21 03:17:53  cdaq
* (SAW) Removed reference to 3rd chamber in hnspace_points
*
c Revision 1.1  1994/02/19  06:15:47  cdaq
c Initial revision
c
*-

*
*     This routine finds the space points in each chamber using wire center
*     locations.
*
*--------------------------------------------------------
       IMPLICIT NONE
       SAVE
*
       character*50 here
       parameter (here= 'H_PATTERN_RECOGNITION')
*
       logical ABORT
       character*(*) err
*
       INCLUDE 'gen_data_structures.cmn'
       INCLUDE 'gen_constants.par'
       INCLUDE 'gen_units.par'
*
*
      include "hms_tracking.cmn"
      include "hms_geometry.cmn"
*     local variables
      integer*4 hit_number(hmax_chamber_hits)
      real*4 space_points(hmax_space_points,2)
      integer*4 space_point_hits(hmax_space_points,hmax_hits_per_point+2)
      integer*4 i,j,k
*
*     temporary initialization
      ABORT= .FALSE.
      err=':success'
*
*   
*     Chamber 1
      hnspace_points(1)=0
      hncham_hits(1)=HDC_HITS_PER_PLANE(1)+HDC_HITS_PER_PLANE(2)
     &          +HDC_HITS_PER_PLANE(3)+HDC_HITS_PER_PLANE(4)
     &          +HDC_HITS_PER_PLANE(5)+HDC_HITS_PER_PLANE(6)
      if(hncham_hits(1).gt.2)  then
          do i=1,hncham_hits(1)
             hit_number(i)=i
          enddo
          call find_space_points(hncham_hits(1),hit_number(1),
     &        HDC_WIRE_CENTER(1),
     &        HDC_PLANE_NUM(1),hspace_point_criterion(1),
     &        hxsp(1),hysp(1),hmax_space_points,
     &        hnspace_points(1), space_points, space_point_hits)
*    
*     select on minimum number of combinations and hits
         call select_space_points(hmax_space_points,
     &        hnspace_points(1), space_points, space_point_hits,
     &        hmin_hit(1),hmin_combos(1))
          do i=1,hnspace_points(1)
             hspace_points(i,1)=space_points(i,1)
             hspace_points(i,2)=space_points(i,2)
             hspace_point_hits(i,1)=space_point_hits(i,1)
             hspace_point_hits(i,2)=space_point_hits(i,2)
             do j=1,space_point_hits(i,1)
               hspace_point_hits(i,j+2)=space_point_hits(i,j+2)
             enddo
          enddo
      endif      
*
*     chamber 2
      hnspace_points(2)=0
      hncham_hits(2)=HDC_HITS_PER_PLANE(7)+HDC_HITS_PER_PLANE(8)
     &          +HDC_HITS_PER_PLANE(9)+HDC_HITS_PER_PLANE(10)
     &          +HDC_HITS_PER_PLANE(11)+HDC_HITS_PER_PLANE(12)
      if(hncham_hits(2).gt.2)  then
          do i=hncham_hits(1)+1,hncham_hits(1)+hncham_hits(2)
             hit_number(i)=i
          enddo
         call find_space_points(hncham_hits(2),hit_number(hncham_hits(1)+1),
     &        HDC_WIRE_CENTER(hncham_hits(1)+1),
     &        HDC_PLANE_NUM(hncham_hits(1)+1),hspace_point_criterion(2),
     &        hxsp(1),hysp(1),hmax_space_points,
     &        hnspace_points(2), space_points, space_point_hits)
*    
*     select on minimum number of combinations and hits
         call select_space_points(hmax_space_points,
     &        hnspace_points(2), space_points, space_point_hits,
     &        hmin_hit(2),hmin_combos(2))
          do i=1,hnspace_points(2)
             k=hnspace_points(1)+i
             hspace_points(k,1)=space_points(i,1)
             hspace_points(k,2)=space_points(i,2)
             hspace_point_hits(k,1)=space_point_hits(i,1)
             hspace_point_hits(k,2)=space_point_hits(i,2)
             do j=1,space_point_hits(i,1)
              hspace_point_hits(k,j+2)=space_point_hits(i,j+2)
             enddo
          enddo
      endif      
*     calculate total numbe of space points
      hnspace_points_tot=hnspace_points(1)+hnspace_points(2)
*     write out results if debugflagpr is set
      if(hdebugflagpr.ne.0) then
        call h_print_pr
      endif
*
      return
      end
