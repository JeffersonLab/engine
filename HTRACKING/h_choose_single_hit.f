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
* $Log: h_choose_single_hit.f,v $
* Revision 1.4  1996/01/16 21:45:35  cdaq
* (JRA) Misc changes
*
* Revision 1.3  1995/05/22 19:39:07  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.2  1994/10/11  20:20:52  cdaq
* (JRA) Fix bug that allowed two hits on a single plane
*
* Revision 1.1  1994/06/30  02:40:17  cdaq
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
      INCLUDE 'hms_data_structures.cmn'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      include 'hms_tracking.cmn'
      include 'hms_geometry.cmn'

*
      integer*4 space_point_hits(hmax_space_points,hmax_hits_per_point+2)
*
*     local variables
      integer*4 point,startnum,finalnum,goodhit(hmax_dc_hits)
      integer*4 plane1,plane2,hit1,hit2,drifttime1,drifttime2
      integer*4 hits(hmax_hits_per_point)
      integer*4 j,k
      
*
*     temporary initialization
      ABORT= .FALSE.
      err=' '
*
*
*     loop over all space points
      do point =1,nspace_points
        startnum = space_point_hits(point,1)
        finalnum=0
          
        do j=3,startnum+2
          goodhit(j) = 1
        enddo
          
        do j=3,startnum+1
          hit1 = space_point_hits(point,j)
          plane1 = hdc_plane_num(hit1)
          drifttime1 = hdc_drift_time(hit1)
          do k=j+1,startnum+2
            hit2 = space_point_hits(point,k)
            plane2 = hdc_plane_num(hit2)
            drifttime2 = hdc_drift_time(hit2)
            if(plane1 .eq. plane2 ) then
              if(drifttime1.gt.drifttime2) then
                goodhit(j) = 0
              else                      !if equal times, choose 1st hit(arbitrary)
                goodhit(k) = 0
              endif
            endif                       ! end test on equal planes
          enddo                         ! end loop on k
        enddo                           ! end loop on j
        do j=3,startnum+2
          if(goodhit(j).gt.0) then
            finalnum = finalnum + 1
            hits(finalnum)=space_point_hits(point,j)
          endif                         ! end check on good hit
        enddo
*     copy good hits to space_point_hits
        space_point_hits(point,1) = finalnum
        do j = 1, finalnum
          space_point_hits(point,j+2) = hits(j)
        enddo                           ! end of copy 
      enddo                             ! end loop on space points
*     
      return
      end
