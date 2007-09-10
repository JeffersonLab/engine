      subroutine h_sp_multiwire(ABORT,err,nspace_points,
     &       space_point_hits,space_points)
*--------------------------------------------------------
*-
*-   Purpose and Methods :  This routine loops over space points and 
*-                          looks at all hits in the space
*-                          point. If more than 1 hit is in the same 
*-                          plane then the space point is cloned with
*-                          all combinations of 1 wire per plane.  The 
*-                          requirements for cloning are:  1) at least 
*-                          4 planes fire, and 2) no more than 6 planes 
*-                          have multiple hits.      
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*50 here
      parameter (here= 'h_sp_multiwire')
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
      real*4 space_points(hmax_space_points,2)     ! xt, yt of each space point
*
*     local variables
      integer*4 point,plane,n1,n2,n3,ntot,nspace_points_check
      integer*4 i,j,k,hit,hit1,hit2,temp,newsp_num,endhit
      integer*4 nspace_points_new, nhits_plane(hdc_planes_per_chamber)
c      integer*4 nspace_points_new, nhits_plane(100) !Phil
      integer*4 nspace_points_tot,nplanes_mult,nplanes_hit,nplanes_single
      integer*4 hits_plane(hdc_planes_per_chamber,hmax_hits_per_point+1)
c      integer*4 hits_plane(100,hmax_hits_per_point+1) !Phil
      integer*4 maxplane(hdc_planes_per_chamber)
c      integer*4 maxplane(100) !Phil
      integer*4 hit_order(hmax_hits_per_point)
*
*     temporary initialization
      ABORT= .FALSE.
      err=' '
*
cc      write(6,*) "IN h_sp_multiwire"  !! MEC       


      nspace_points_tot = nspace_points
      do point = 1,nspace_points              !!  Start loop over SPs  !!

cc       write(6,*) "orig SP  ",space_point_hits(point,1),space_point_hits(point,2),
cc     &  space_point_hits(point,3),space_point_hits(point,4),
cc     &         space_point_hits(point,5),space_point_hits(point,6),
cc     &  space_point_hits(point,7),space_point_hits(point,8),
cc     &  space_point_hits(point,9),space_point_hits(point,10)


*-  Do some initialization

        nplanes_hit = 0                       !!  # of planes with hits !!
        nplanes_mult = 0                      !!  # of planes with multiple hits  !!
        nspace_points_new = 1
        do j=1,hdc_planes_per_chamber
          nhits_plane(j) = 0                  !!  # of hits in each plane  !!
          do k=1,hmax_hits_per_point+1
            hits_plane(j,k) = 0               !!  hit array with hit #s for each plane  !!
          enddo
        enddo

*-  End initialization

*-  Count the number of hits in each plane and fill array with the hit #'s associated with each plane

        do j = 3,space_point_hits(point,1)+2   !! Loop over all hits in sp -         !!
          hit = space_point_hits(point,j)      !! count multiple hits in each plane  !! 
          hit_order(hit) = j-2
          plane = hdc_plane_num(hit)
          if(plane.GT.6) plane = plane - hdc_planes_per_chamber  
          nhits_plane(plane) = nhits_plane(plane) + 1
          hits_plane(plane,1) = nhits_plane(plane)
          hits_plane(plane,nhits_plane(plane)+1) = hit
        enddo

*-  End counting

*-  Now do some counting of planes with various #s of hits
  
        do j = 1,hdc_planes_per_chamber   !!  count # of planes hit !!
          if(nhits_plane(j).GT.0) then
            nplanes_hit = nplanes_hit + 1
            nspace_points_new = nspace_points_new*nhits_plane(j)
          endif
          if(nhits_plane(j).GT.1) nplanes_mult = nplanes_mult + 1
        enddo
        nspace_points_new = nspace_points_new - 1
        nspace_points_check = nspace_points_tot + nspace_points_new
        nplanes_single = nplanes_hit - nplanes_mult

*-  End counting 

cc        write(6,*) "NEW SP:  ",nspace_points_new
cc        write(6,*) "sp",point,":",nhits_plane    !! MEC
c        write(6,*) "sp",point,":  # planes with single hit:",nplanes_single,"  
c     &# with mult hits:",nplanes_mult  !!  MEC 
     
        ntot = 0

*-  Now do cloning if conditions are met

        if(nplanes_hit.GE.4.AND.nplanes_mult.LT.4.AND.nplanes_mult.GT.0.AND.
     &            nspace_points_check.LT.20) then

**--  Order Planes by decreasing # of hits

          do j = 1,hdc_planes_per_chamber     !!  First start out sequentially  !!
            maxplane(j) = j          !!  Contains plane #s ordered with decreasing # of hits  !!
          enddo
        
          do j = 1,hdc_planes_per_chamber
            do k = j+1,hdc_planes_per_chamber
              if(nhits_plane(maxplane(k)).GT.nhits_plane(maxplane(j))) then  !!  switch position of plane #s in maxplane !!
                temp = maxplane(j)
                maxplane(j) = maxplane(k)
                maxplane(k) = temp
              endif 
            enddo
          enddo

c          write(6,*) "Max hit order is:  ",maxplane         

**--  End Order Planes

**--  First fill the clones with 1 hit each from the 3 planes with the most hits

        ntot = 0   
        do n1 = 1,nhits_plane(maxplane(1))
          do n2 = 1,nhits_plane(maxplane(2))
            do n3 = 1,nhits_plane(maxplane(3)) 
              ntot = ntot + 1
              newsp_num = nspace_points_tot + ntot - 1                    
              if(n1.EQ.1.AND.n2.EQ.1.AND.n3.EQ.1) newsp_num = point  !!  Copy first clone over original SP  !!
              space_points(newsp_num,1) = space_points(point,1)
              space_points(newsp_num,2) = space_points(point,2) 
              space_point_hits(newsp_num,1) = nplanes_hit
              space_point_hits(newsp_num,2) = space_point_hits(point,2)
              space_point_hits(newsp_num,3) = hits_plane(maxplane(1),n1+1)
              space_point_hits(newsp_num,4) = hits_plane(maxplane(2),n2+1)
              space_point_hits(newsp_num,5) = hits_plane(maxplane(3),n3+1)
              space_point_hits(newsp_num,6) = hits_plane(maxplane(4),2)
              if(hits_plane(maxplane(5),1).EQ.1) 
     &                   space_point_hits(newsp_num,7) = hits_plane(maxplane(5),2)
              if(hits_plane(maxplane(6),1).EQ.1) 
     &                    space_point_hits(newsp_num,8) = hits_plane(maxplane(6),2)
            enddo
          enddo
        enddo

*-  Loop over clones and order hits in same way as parent SP
         
          do i = 1,ntot
            newsp_num = nspace_points_tot + i  - 1
            if(i.EQ.1) newsp_num = point 
            do j = 3,nplanes_hit+2
              do k = 3,nplanes_hit+2
                hit1 = space_point_hits(newsp_num,j)
                hit2 = space_point_hits(newsp_num,k)                
                if(hit_order(hit2).GT.hit_order(hit1)) then
                  temp = space_point_hits(newsp_num,k)    !!  switch position of hits  !!
                  space_point_hits(newsp_num,k) = space_point_hits(newsp_num,j)
                  space_point_hits(newsp_num,j) = temp
                endif
              enddo
            enddo
          enddo

*-  End order clone hits



*-  End cloning

c          do j = 1,ntot        !!  make clones of sp !!
c            space_point_hits(j + nspace_points,1) = hdc_planes_per_chamber  !! now only 1 hit per plane  !!
c            do k = 1,hmax_hits_per_point+2
c              space_point_hits(j + nspace_points,k) = space_point_hits(point,k)  !! copy old SP hits to clones !!
c            enddo
c          enddo
     
          nspace_points_tot = nspace_points_tot + ntot - 1  !! add new SPs to running total
        else
          ntot = 1
        endif
        
cc        write(6,*) "Ntot:  ",ntot
cc        write(6,*) "Running total of SPs = ",nspace_points_tot


      enddo                             ! end loop on space points
      if(nspace_points_tot.LE.20) nspace_points = nspace_points_tot  !!  Don't increment inside DO loop  !!

      do i = 1,nspace_points_tot        !!  Fill in zeros  !!
        endhit = 3 + space_point_hits(i,1)
        do j = endhit,hmax_hits_per_point
          space_point_hits(i,j) = 0
        enddo
      enddo  

c      do i = 1,nspace_points_tot
c        write(6,*) space_point_hits(i,1),space_point_hits(i,2),space_point_hits(i,3),
c     &             space_point_hits(i,4),space_point_hits(i,5),space_point_hits(i,6),
c     &              space_point_hits(i,7),space_point_hits(i,8)
c      enddo

*     
      return
      end













