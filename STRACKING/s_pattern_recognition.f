      subroutine S_PATTERN_RECOGNITION(ABORT,err)
*--------------------------------------------------------
*-
*-   Purpose and Methods :  Finds SOS Space points 
*-
*-      Required Input BANKS     SOS_DECODED_DC
*-
*-      Output BANKS             SOS_FOCAL_PLANE
*-                               SOS_DECODED_DC hit coordinates
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
*-   Created 30-AUG-1993   D. F. Geesaman
*-   Modified 19-JAN-1994  DFG    Include standard error form
* $Log$
* Revision 1.1  1994/02/21 16:15:19  cdaq
* Initial revision
*
*
*     This routine finds the space points in each chamber using wire center
*     locations.
*
*--------------------------------------------------------
       IMPLICIT NONE
       SAVE
*
       character*50 here
       parameter (here= 'S_PATTERN_RECOGNITION')
*
       logical ABORT
       character*(*) err
*
       INCLUDE 'gen_data_structures.cmn'
       INCLUDE 'gen_constants.par'
       INCLUDE 'gen_units.par'
*
*
      include "sos_tracking.cmn"
      include "sos_geometry.cmn"
*     local variables
      integer*4 hit_number(smax_chamber_hits)
      real*4 space_points(smax_space_points,2)
      integer*4 space_point_hits(smax_space_points,smax_hits_per_point+2)
      integer*4 i,j,k
*
*     temporary initialization
      ABORT= .FALSE.
      err=':success'
*
*   
*     Chamber 1
      snspace_points(1)=0
      sncham_hits(1)=SDC_HITS_PER_PLANE(1)+SDC_HITS_PER_PLANE(2)
     &          +SDC_HITS_PER_PLANE(3)+SDC_HITS_PER_PLANE(4)
     &          +SDC_HITS_PER_PLANE(5)+SDC_HITS_PER_PLANE(6)
      if(sncham_hits(1).gt.2)  then
          do i=1,sncham_hits(1)
             hit_number(i)=i
          enddo
          call find_space_points(sncham_hits(1),hit_number(1),
     &        SDC_WIRE_CENTER(1),
     &        SDC_PLANE_NUM(1),sspace_point_criterion(1),
     &        sxsp(1),sysp(1),smax_space_points,
     &        snspace_points(1), space_points, space_point_hits)
*    
*     select on minimum number of combinations and hits
         call select_space_points(smax_space_points,
     &        snspace_points(1), space_points, space_point_hits,
     &        smin_hit(1),smin_combos(1))
          do i=1,snspace_points(1)
             sspace_points(i,1)=space_points(i,1)
             sspace_points(i,2)=space_points(i,2)
             sspace_point_hits(i,1)=space_point_hits(i,1)
             sspace_point_hits(i,2)=space_point_hits(i,2)
             do j=1,space_point_hits(i,1)
               sspace_point_hits(i,j+2)=space_point_hits(i,j+2)
             enddo
          enddo
      endif      
*
*     chamber 2
      snspace_points(2)=0
      sncham_hits(2)=SDC_HITS_PER_PLANE(7)+SDC_HITS_PER_PLANE(8)
     &          +SDC_HITS_PER_PLANE(9)+SDC_HITS_PER_PLANE(10)
     &          +SDC_HITS_PER_PLANE(11)+SDC_HITS_PER_PLANE(12)
      if(sncham_hits(2).gt.2)  then
          do i=sncham_hits(1)+1,sncham_hits(1)+sncham_hits(2)
             hit_number(i)=i
          enddo
         call find_space_points(sncham_hits(2),hit_number(sncham_hits(1)+1),
     &        SDC_WIRE_CENTER(sncham_hits(1)+1),
     &        SDC_PLANE_NUM(sncham_hits(1)+1),sspace_point_criterion(2),
     &        sxsp(1),sysp(1),smax_space_points,
     &        snspace_points(2), space_points, space_point_hits)
*    
*     select on minimum number of combinations and hits
         call select_space_points(smax_space_points,
     &        snspace_points(2), space_points, space_point_hits,
     &        smin_hit(2),smin_combos(2))
          do i=1,snspace_points(2)
             k=snspace_points(1)+i
             sspace_points(k,1)=space_points(i,1)
             sspace_points(k,2)=space_points(i,2)
             sspace_point_hits(k,1)=space_point_hits(i,1)
             sspace_point_hits(k,2)=space_point_hits(i,2)
             do j=1,space_point_hits(i,1)
              sspace_point_hits(k,j+2)=space_point_hits(i,j+2)
             enddo
          enddo
      endif      


*     chamber 3
      snspace_points(3)=0
      sncham_hits(3)=SDC_HITS_PER_PLANE(13)+SDC_HITS_PER_PLANE(14)
     &          +SDC_HITS_PER_PLANE(15)+SDC_HITS_PER_PLANE(16)
     &          +SDC_HITS_PER_PLANE(17)+SDC_HITS_PER_PLANE(18)
      if(sncham_hits(3).gt.2)  then
          do i=sncham_hits(1)+sncham_hits(2)+1,
     &         sncham_hits(1)+sncham_hits(2)+sncham_hits(3)
             hit_number(i)=i
          enddo
         call find_space_points(sncham_hits(3),
     &        hit_number(sncham_hits(1)+sncham_hits(2)+1),
     &        SDC_WIRE_CENTER(sncham_hits(1)+sncham_hits(2)+1),
     &        SDC_PLANE_NUM(sncham_hits(1)+sncham_hits(2)+1),
     &        sspace_point_criterion(3),
     &        sxsp(1),sysp(1),smax_space_points,
     &        snspace_points(3), space_points, space_point_hits)
*    
*     select on minimum number of combinations and hits
         call select_space_points(smax_space_points,
     &        snspace_points(3), space_points, space_point_hits,
     &        smin_hit(3),smin_combos(3))
          do i=1,snspace_points(3)
             k=i+snspace_points(1)+snspace_points(2)

             sspace_points(k,1)=space_points(i,1)
             sspace_points(k,2)=space_points(i,2)
             sspace_point_hits(k,1)=space_point_hits(i,1)
             sspace_point_hits(k,2)=space_point_hits(i,2)
             do j=1,space_point_hits(i,1)
               sspace_point_hits(k,j+2)=space_point_hits(i,j+2)
             enddo
          enddo
      endif      
*     calculate total numbe of space points
      snspace_points_tot=snspace_points(1)+snspace_points(2)+snspace_points(3)
*     write out results if debugflagpr is set
      if(sdebugflagpr.ne.0) then
        call s_print_pr
      endif
*
      return
      end
