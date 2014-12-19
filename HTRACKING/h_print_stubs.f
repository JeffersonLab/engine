      subroutine h_print_stubs
*     subroutine to dump output of H_LEFT_RIGHT
*     All the results are contained in hms_tracking.inc
*     d.f. geesaman          17 January 1994
* $Log: h_print_stubs.f,v $
* Revision 1.2  1995/05/22 19:39:18  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.1  1994/02/19  06:16:52  cdaq
* Initial revision
*
      implicit none
      include "hms_data_structures.cmn"
      include "hms_tracking.cmn"
      include "hms_geometry.cmn"
*     local variables
      integer*4 i,j
      write(hluno,'('' HMS STUB FIT RESULTS'')')
      if(hnspace_points_tot.ge.1) then
        write(hluno,'(''point        x_t              y_t     '',
     &           ''          xp_t          yp_t'')')
        write(hluno,'(''             [cm]             [cm]    '',
     &           ''         [rad]          [rad]'')')
1001  format(3x,i3,4x,4e15.7)
        do i=1,hnspace_points_tot
          write(hluno,1001) i,(hbeststub(i,j),j=1,4)
        enddo
        do i=1,hnspace_points_tot
           write(hluno,'(a,i3,a)') ' space point = ',i
     > ,' hit-number  plane  '
           do j=1,hspace_point_hits(i,1)
              write(hluno,'(T20,i4,3x,i4,3x,f10.5,3x,f10.5,3x,f10.5)') 
     >hspace_point_hits(i,2+j),HDC_PLANE_NUM( hspace_point_hits(i,2+j))
     >,hspace_point_wirecoord(i,j)
     >,hspace_point_leftright(i,j),hspace_point_driftdis(i,j)
           enddo 
        enddo
      endif
      return
      end
