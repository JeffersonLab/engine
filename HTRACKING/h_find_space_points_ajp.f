      subroutine h_find_space_points_ajp(ich,nspacepoints,space_points,space_point_hits)

      implicit none
      save
      
      include 'hms_data_structures.cmn'
      include 'hms_geometry.cmn'
      include 'hms_tracking.cmn'

c     ignore hits beyond 10 hits in any given plane
c     number of pairs to test for a six-plane combo is as follows:
c     compare 2 Y planes to 4 non-Y planes = 8 unique pairs +
c     U and V = 9 unique pairs. Calculate average xtest,ytest over all 9 pairs
c     check whether each individual pair is within space_point_criterion of the 
c     average. 
c     In case of five planes, the number of unique pairs depends on which plane is missing
c     If we have six planes firing but no six-plane space points are found, then drop the
c     number of planes required to five

      integer ich,nspacepoints,nhitsrequired
      integer hitslist(6,10)
      integer testcombo(6)
      real xtest(9),ytest(9) ! coordinates of test points
      real xtsum,ytsum,xtemp,ytemp,xmean,ymean
      integer npair
      integer planelist(6)
      integer nhitsperplane(6)
      integer nplaneshit,ncombos,nplanesrequired
      integer hit,ihit,plane,iplane,jhit,jplane,ipair
      integer i1,i2,i3,i4,i5,i6
      integer worsthit
      real determinant
      logical all_pairs_good
      real squared_distance
      real space_points(hmax_space_points,2) 
      integer space_point_hits(hmax_space_points,hmax_hits_per_point+2)
      
c      write(*,*) 'AJP HMS space points routine chamber=',ich

      nplaneshit = 0
      do iplane = 1,6
         nhitsperplane(iplane) = 0
      enddo

c      write(*,*) 'initialized nplanes, nhitsperplane'

c     fill my local hit array:
      do ihit=1,hdc_tot_hits 
         plane = hdc_plane_num(ihit)
         if( plane.le.6 .and. ich.eq.1) then
            iplane = plane
            if(nhitsperplane(iplane).lt.10) then
               nhitsperplane(iplane) = nhitsperplane(iplane) + 1
               hitslist(iplane,nhitsperplane(iplane)) = ihit
            endif
         else if( plane.gt.6.and.ich.eq.2) then
            iplane = plane - 6
            if(nhitsperplane(iplane).lt.10) then
               nhitsperplane(iplane) = nhitsperplane(iplane) + 1
               hitslist(iplane,nhitsperplane(iplane)) = ihit
            endif
         endif
      enddo
      
c      write(*,*) 'initialized local hit arrays'

c     now we know 
c     a) number of hits per plane
c     b) list of hits per plane
      nplaneshit=0
      ncombos = 1
      do iplane=1,6
         if(nhitsperplane(iplane).gt.0) then
            nplaneshit = nplaneshit + 1
            ncombos = ncombos * nhitsperplane(iplane)
            planelist(nplaneshit) = iplane
         endif
      enddo

c      write(*,*) 'found nplaneshit=',nplaneshit,'ncombos=',ncombos

      nspacepoints = 0

      if(nplaneshit .eq. 6) then

         nhitsrequired = 6
         do while( nspacepoints .le. 0 .and. nhitsrequired .ge. 5)
            do i1=1,nhitsperplane(1)
               do i2=1,nhitsperplane(2)
                  do i3=1,nhitsperplane(3)
                     do i4=1,nhitsperplane(4)
                        do i5=1,nhitsperplane(5)
                           do i6=1,nhitsperplane(6)
                              testcombo(1) = hitslist(1,i1)
                              testcombo(2) = hitslist(2,i2)
                              testcombo(3) = hitslist(3,i3)
                              testcombo(4) = hitslist(4,i4)
                              testcombo(5) = hitslist(5,i5)
                              testcombo(6) = hitslist(6,i6)
                              
                              npair = 0
                              xtsum = 0.
                              ytsum = 0.
                              
                              do ihit=1,5
                                 do jhit=ihit+1,6
                                    iplane = ihit + 6 * (ich-1)
                                    jplane = jhit + 6 * (ich-1)
                                    determinant = hxsp(iplane)*hysp(jplane) - hysp(iplane)*hxsp(jplane)
                                    if( abs(determinant).gt.0.3 ) then
                                       npair = npair + 1
                                       xtest(npair) = (hdc_wire_center(testcombo(ihit))*hysp(jplane) - 
     $                                      hdc_wire_center(testcombo(jhit))*hysp(iplane) ) / determinant
                                       ytest(npair) = (hdc_wire_center(testcombo(jhit))*hxsp(iplane) - 
     $                                      hdc_wire_center(testcombo(ihit))*hxsp(jplane) ) / determinant
                                       
                                       xtsum = xtsum + xtest(npair)
                                       ytsum = ytsum + ytest(npair)
                                    endif
                                 enddo
                              enddo
                              
                              xmean = xtsum / float(npair)
                              ymean = ytsum / float(npair)
                              
c     IFF all pairs in this test combo have squared distance from the mean less than space_point_criterion, 
c     add a new space point to the array:
                              
                              all_pairs_good = .true.
                              
                              do ipair = 1,npair
                                 squared_distance = (xtest(ipair) - xmean)**2 + 
     $                                (ytest(ipair) - ymean)**2
                                 if( squared_distance .gt. hspace_point_criterion(ich) )
     $                                then
                                    all_pairs_good = .false.
                                 endif
                              enddo
                              
                              if(all_pairs_good.and.nspacepoints.lt.
     $                             hmax_space_points/2) then ! add a space point:

c                                 write(*,*) 'found six-hit space point'
                                 
                                 nspacepoints = nspacepoints + 1

c                                 write(*,*) '6hit, npoint=',nspacepoints
                                 
                                 space_point_hits(nspacepoints,1) = 6
                                 space_point_hits(nspacepoints,2) = 0
                                 space_points(nspacepoints,1) = xmean
                                 space_points(nspacepoints,2) = ymean
                                 do ihit=1,6
                                    space_point_hits(nspacepoints,ihit+2) = testcombo(ihit)
                                 enddo
                              else if ( nhitsrequired .eq. 5 ) then ! Can we get a good space point by removing one of the hits? Try all possible combinations of five out of 6:
                                 do worsthit=1,6
                                    npair = 0
                                    xtsum = 0.
                                    ytsum = 0.
                                    do ihit=1,5
                                       do jhit=ihit+1,6
                                          iplane = ihit + 6 * (ich-1)
                                          jplane = jhit + 6 * (ich-1)
                                          determinant = hxsp(iplane)*hysp(jplane) - hysp(iplane)*hxsp(jplane)
                                          if( abs(determinant).gt.0.3 
     $                                         .and.ihit.ne.worsthit
     $                                         .and.jhit.ne.worsthit )
     $                                         then
                                             npair = npair + 1
                                             xtest(npair) = (hdc_wire_center(testcombo(ihit))*hysp(jplane) - 
     $                                            hdc_wire_center(testcombo(jhit))*hysp(iplane) ) / determinant
                                             ytest(npair) = (hdc_wire_center(testcombo(jhit))*hxsp(iplane) - 
     $                                            hdc_wire_center(testcombo(ihit))*hxsp(jplane) ) / determinant
                                             
                                             xtsum = xtsum + xtest(npair)
                                             ytsum = ytsum + ytest(npair)
                                          endif
                                       enddo
                                    enddo

                                    xmean = xtsum / float(npair)
                                    ymean = ytsum / float(npair)
                                    
                                    all_pairs_good = .true.
                                    
                                    do ipair = 1,npair
                                       squared_distance = (xtest(ipair) - xmean)**2 + 
     $                                      (ytest(ipair) - ymean)**2
                                       if( squared_distance .gt. hspace_point_criterion(ich) )
     $                                      then
                                          all_pairs_good = .false.
                                       endif
                                    enddo
                                    
                                    if(all_pairs_good.and.nspacepoints
     $                                   .lt.hmax_space_points/2) then ! add a space point:
c$$$                                       write(*,*) '5-hit point found'
c$$$                                       write(*,*) 'worsthit=',worsthit
                                       nspacepoints = nspacepoints + 1

c                                       write(*,*) '5hit, npoint=',nspacepoints
                                       space_point_hits(nspacepoints,1) = 5
                                       space_point_hits(nspacepoints,2) = 0
                                       space_points(nspacepoints,1) = xmean
                                       space_points(nspacepoints,2) = ymean

                                       hit = 1

                                       do ihit=1,6
                                          if(ihit.ne.worsthit) then
                                             space_point_hits(nspacepoints,hit+2) = testcombo(ihit)
                                             hit = hit + 1
                                          endif
                                       enddo
                                    endif
                                 enddo ! worst hit 
                              endif ! all pairs passed space_point_criterion?
                              
                           enddo ! i6
                        enddo   ! i5
                     enddo      ! i4
                  enddo         ! i3
               enddo            ! i2
            enddo               ! i1
            
            if( nspacepoints .le. 0 ) nhitsrequired = nhitsrequired - 1

         enddo                  ! end do while loop over six-plane combos

      else if(nplaneshit .eq. 5) then
         
         do i1=1,nhitsperplane(planelist(1))
            do i2=1,nhitsperplane(planelist(2))
               do i3=1,nhitsperplane(planelist(3))
                  do i4=1,nhitsperplane(planelist(4))
                     do i5=1,nhitsperplane(planelist(5))
                        testcombo(1) = hitslist( planelist(1),i1 )
                        testcombo(2) = hitslist( planelist(2),i2 )
                        testcombo(3) = hitslist( planelist(3),i3 )
                        testcombo(4) = hitslist( planelist(4),i4 )
                        testcombo(5) = hitslist( planelist(5),i5 )

                        npair = 0
                        xtsum = 0.
                        ytsum = 0.
                        
                        do ihit=1,4
                           do jhit=ihit+1,5
                              iplane = planelist(ihit) + 6*(ich-1)
                              jplane = planelist(jhit) + 6*(ich-1)
                              determinant = hxsp(iplane)*hysp(jplane) - hysp(iplane)*hxsp(jplane)
                              if( abs(determinant).gt.0.3 ) then
                                 npair = npair + 1
                                 xtest(npair) = (hdc_wire_center(testcombo(ihit))*hysp(jplane) - 
     $                                hdc_wire_center(testcombo(jhit))*hysp(iplane) ) / determinant
                                 ytest(npair) = (hdc_wire_center(testcombo(jhit))*hxsp(iplane) - 
     $                                hdc_wire_center(testcombo(ihit))*hxsp(jplane) ) / determinant
                                 
                                 xtsum = xtsum + xtest(npair)
                                 ytsum = ytsum + ytest(npair)
                              endif
                           enddo
                        enddo

                        xmean = xtsum / float(npair)
                        ymean = ytsum / float(npair)
                        
c     IFF all pairs in this test combo have squared distance from the mean less than space_point_criterion, 
c     add a new space point to the array:
                        all_pairs_good = .true.
                        
                        do ipair = 1,npair
                           squared_distance = (xtest(ipair) - xmean)**2 + 
     $                          (ytest(ipair) - ymean)**2
                           if( squared_distance .gt. hspace_point_criterion(ich) )
     $                          then
                              all_pairs_good = .false.
                           endif
                        enddo

                        if( all_pairs_good .and. nspacepoints.lt.
     $                       hmax_space_points/2) then ! add a new space point:
c                           write(*,*) '5-hit point found'
                           nspacepoints = nspacepoints + 1

c                           write(*,*) '5hit, npoint=',nspacepoints

                           space_point_hits(nspacepoints,1) = 5
                           space_point_hits(nspacepoints,2) = 0
                           space_points(nspacepoints,1) = xmean
                           space_points(nspacepoints,2) = ymean
                           do ihit=1,5
                              space_point_hits(nspacepoints,ihit+2) = testcombo(ihit)
                           enddo
                        endif
                     enddo      ! i5
                  enddo         ! i4
               enddo            ! i3
            enddo               ! i2
         enddo                  ! i1
      endif                     ! nplaneshit == 5

c      write(*,*) 'SUCCESS! ich,nspacepoints=',ich,nspacepoints

      return 
      end
         
