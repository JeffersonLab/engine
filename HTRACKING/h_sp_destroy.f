      subroutine h_sp_destroy(ABORT,err,nspace_points,
     &       space_point_hits,space_points,ich)
*--------------------------------------------------------
*-
*-   Purpose and Methods :  This routine loops over space points and 
*-                          removes those with less than 4 planes hit
*-                          and missing hits in Y,Y' planes                             
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*50 here
      parameter (here= 'h_sp_destroy')
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
      integer*4 space_points(hmax_space_points,2)
*
*     local variables
      integer*4 point,oldpoint,plane,ich
      integer*4 j,hit,nspace_points_good
      integer*4 nhits_plane(hdc_planes_per_chamber)
c      integer*4 nhits_plane(100) !Phil
      integer*4 space_points_temp(hmax_space_points,2)      
      integer*4 space_point_hits_temp(hmax_space_points,hmax_hits_per_point+2)
      integer*4 nplanes_hit,space_points_good(nspace_points)
c      integer*4 nplanes_hit,space_points_good(100) !Phil
*
*     temporary initialization
      ABORT= .FALSE.
      err=' '
*
cc      write(6,*) "IN h_sp_destroy, ICH = ",ich  !! MEC

       nspace_points_good = 0

       do j=1,nspace_points
         space_points_good(j) = 0
       enddo


*-  Start loop over SPs

      do point = 1,nspace_points             

**--  Do some initialization

        nplanes_hit = 0                       !!  # of planes with hits !!
        do j=1,hdc_planes_per_chamber
          nhits_plane(j) = 0                  !!  # of hits in each plane  !!
        enddo

**--  End initialization

**--  Count the number of hits in each plane and fill array with the hit #'s associated with each plane

        do j = 3,space_point_hits(point,1)+2    !!  Loop over all hits in sp - count multiple hits in each plane  !!
          hit = space_point_hits(point,j)
          plane = hdc_plane_num(hit)
          if(plane.GT.6) plane = plane - hdc_planes_per_chamber
          nhits_plane(plane) = nhits_plane(plane) + 1
        enddo

**--  End counting

**--  Now count the # of planes hit in the SP
  
        do j = 1,hdc_planes_per_chamber   !!  count # of planes hit !!
          if(nhits_plane(j).GT.0) nplanes_hit = nplanes_hit + 1
        enddo

        if(nplanes_hit.GE.hmin_hit(ich).AND.nhits_plane(2).GE.1.
     &            AND.nhits_plane(5).GE.1) then    !!  Don't clone if not enough planes or missing Y !!
          nspace_points_good = nspace_points_good + 1
          space_points_good(nspace_points_good) = point
        else
cc          write(6,*) "Missing Y-hit!!"
        endif

cc        write(6,*) "SP #:  ", point,"  # of planes hit:  ",nplanes_hit

      enddo

*-  End loop over SPs

cc      write(6,*) "# of Good SPs = ",nspace_points_good
cc      write(6,*) "The Good SPs are:  ",space_points_good

*-  Loop over SPs again and remove the bad ones

      nspace_points = nspace_points_good
      do point = 1,nspace_points
        oldpoint = space_points_good(point)
        space_points_temp(point,1) = space_points(oldpoint,1)
        space_points_temp(point,2) = space_points(oldpoint,2)
        do j = 1,hmax_hits_per_point+2
          space_point_hits_temp(point,j) = space_point_hits(oldpoint,j) 
        enddo
      enddo

**--  Copy temporary SP hit array to new SP hit array  


      do point =1,hmax_space_points
        if(point.LE.nspace_points) then
          space_points(point,1) = space_points_temp(point,1)  ! Update X,Y positions !
          space_points(point,2) = space_points_temp(point,2)
          do j = 1,hmax_hits_per_point+2
            space_point_hits(point,j) = space_point_hits_temp(point,j) 
          enddo
        else
          space_points(point,1) = 0  ! Update X,Y positions !
          space_points(point,2) = 0
        endif
      enddo

**--  End SP hit array copy

*-  End remove bad SPs

*     
      return
      end


















