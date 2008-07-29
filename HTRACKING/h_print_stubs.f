      subroutine h_print_stubs
*     subroutine to dump output of H_LEFT_RIGHT
*     All the results are contained in hms_tracking.inc
*     d.f. geesaman          17 January 1994
* $Log$
* Revision 1.2.24.1  2008/07/29 16:37:42  puckett
* made output of h_print_stubs more informative
*
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
      integer*4 h_chamnum
      integer*4 plane,wire
      real*4 wirecenter,driftdist,wirecoord
      real*4 stubcoord,diff

      external h_chamnum
c$$$      write(hluno,'('' HMS STUB FIT RESULTS'')')
c$$$      if(hnspace_points_tot.ge.1) then
c$$$        write(hluno,'(''point        x_t              y_t     '',
c$$$     &           ''          xp_t          yp_t'')')
c$$$        write(hluno,'(''             [cm]             [cm]    '',
c$$$     &           ''         [rad]          [rad]'')')
c$$$1001  format(3x,i3,4x,4e15.7)
c$$$        do i=1,hnspace_points_tot
c$$$          write(hluno,1001) i,(hbeststub(i,j),j=1,4)
c$$$        enddo
c$$$        write(hluno,'('' hit         HDC_WIRE_CENTER  HDC_DRIFT_DIS     '',
c$$$     &         '' HDC_WIRE_COORD'')')
c$$$        do i=1,HDC_TOT_HITS
c$$$          write(hluno,1002) i,HDC_WIRE_CENTER(i),HDC_DRIFT_DIS(i),
c$$$     &       HDC_WIRE_COORD(i)
c$$$1002  format(3x,i3,4x,e16.8,2x,e16.8,2x,e16.8)
c$$$        enddo
c$$$      endif
 101  format(A25)
      write(hluno,101) 'HMS SPACE POINTS AND STUB FIT RESULTS'
      
 102  format(6A16)
 103  format(2I16,4F16.5)
 104  format(A16,I16)
 105  format(6A21)
 106  format(2I21,4F21.5)
      
      do i=1,hnspace_points_tot
         write(hluno,102) 'point, ','chamber, ','xstub, ','ystub, ',
     $     'xpstub, ','chi2 '
         write(hluno,103) i,h_chamnum(i),hbeststub(i,1),hbeststub(i,2),
     $        hbeststub(i,3),h_stub_chi2perdf(i)
         write(hluno,104) 'Number of hits=',hspace_point_hits(i,1)

         write(hluno,105) 'plane,','wire,','wire center,','drift dist,',
     $        'wire coord,','wcoord-stubcoord(um)'
         do j=1,hspace_point_hits(i,1)
            plane = hdc_plane_num(hspace_point_hits(i,j+2))
            wire = hdc_wire_num(hspace_point_hits(i,j+2))
            wirecenter = hdc_wire_center(hspace_point_hits(i,j+2))
            driftdist = hdc_drift_dis(hspace_point_hits(i,j+2))
            wirecoord = hdc_wire_coord(hspace_point_hits(i,j+2))
c     calculate stub coordinate at this plane
            stubcoord = hplane_coeff(5,plane)*hbeststub(i,1) +
     $           hplane_coeff(6,plane)*hbeststub(i,2) + 
     $           hplane_coeff(3,plane)*hbeststub(i,3) + 
     $           hplane_coeff(4,plane)*hbeststub(i,4)
            diff = (wirecoord - stubcoord)*1.e4
            write(hluno,106) plane,wire,wirecenter,driftdist,wirecoord,
     $           diff
         enddo
      enddo

      return
      end

