      subroutine h_choose_single_hit(ABORT,err,nspace_points,
     &       space_point_hits)
*--------------------------------------------------------
*-
*-   Purpose and Methods :  This routine looks at all hits in a space
*-                          point. If two hits are in the same plane it
*-                          rejects the one with the longer drift time
*-
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
*-   Created 28-JUN-1994   D. F. Geesaman
* $Log$
* Revision 1.1  1994/06/30 02:40:17  cdaq
* Initial revision
*
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*50 here
      parameter (here= 'h_choose_single_hit')
      integer*4 nspace_points

*
      logical ABORT
      character*(*) err
*
      INCLUDE 'gen_data_structures.cmn'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      include 'hms_tracking.cmn'
      include 'hms_geometry.cmn'

*
      integer*4 space_point_hits(hmax_space_points,hmax_hits_per_point+2)
*
*     local variables
      integer*4 point,startnum,finalnum,goodhit
      integer*4 plane1,plane2,hit1,hit2,drifttime1
      integer*4 hits(hmax_hits_per_point)
      integer*4 j,k
      
*
*     temporary initialization
      ABORT= .FALSE.
      err=' '
*
*
*     loop over all space points
      if(nspace_points .gt. 0) then
         do point =1,nspace_points
            startnum = space_point_hits(point,1)
            finalnum=0
            do j=3,startnum+2
               hit1 = space_point_hits(point,j)
               plane1 = HDC_PLANE_NUM(hit1)
               drifttime1 = HDC_DRIFT_TIME(hit1)
               goodhit = 1
               do k=3,startnum+2
                  if(j .ne. k) then
                     hit2 = space_point_hits(point,k)
                     plane2 = HDC_PLANE_NUM(hit2)
                     if(plane1 .eq. plane2 ) then
                        if(drifttime1.gt. HDC_DRIFT_TIME(hit2) ) then
                           goodhit = 0
                        endif
                     endif              ! end test on equal planes
                  endif                 ! end test on equal list
               enddo                    ! end loop on k
               if(goodhit .gt.0 ) then
                  finalnum = finalnum + 1
                  hits(finalnum)=hit1
               endif                    ! end check on good hit
            enddo                       ! end loop on j
*     copy good hits to space_point_hits
            space_point_hits(point,1) = finalnum
            do j = 1, finalnum
               space_point_hits(point,j+2) = hits(j)
            enddo                       ! end of copy 
         enddo                          ! end loop on space points
      endif                             ! end test on no space points      
*
      return
      end
