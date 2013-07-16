      subroutine s_print_stubs
*     subroutine to dump output of S_LEFT_RIGHT
*     All the results are contained in sos_tracking.inc
*     d.f. geesaman          5 September 1993
* $Log: s_print_stubs.f,v $
* Revision 1.3  1995/05/22 19:45:46  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.2  1995/05/11  21:06:54  cdaq
* (JRA) ???
*
* Revision 1.1  1994/02/21  16:38:06  cdaq
* Initial revision
*
      implicit none
      include "sos_data_structures.cmn"
      include "sos_tracking.cmn"
      include "sos_geometry.cmn"
*     local variables
      integer*4 i,j,k
      write(sluno,'('' SOS STUB FIT RESULTS'')')
      if(snspace_points_tot.ge.1) then
        write(sluno,'(''point        x_t              y_t     '',
     &           ''          xp_t          yp_t'')')
        write(sluno,'(''             [cm]             [cm]    '',
     &           ''         [rad]          [rad]'')')
1001  format(3x,i3,4x,4e15.7)
        do i=1,snspace_points_tot
          write(sluno,1001) i,(sbeststub(i,j),j=1,4)
        enddo
        write(sluno,'('' hit   plane    SDC_WIRE_CENTER  SDC_DRIFT_DIS     '',
     &         '' SDC_WIRE_COORD'')')
        do i=1,snspace_points_tot
          do j=1,sspace_point_hits(i,1)
            k=sspace_point_hits(i,2+j)
            write(sluno,1002) k,sdc_plane_num(k),SDC_WIRE_CENTER(k),
     &       SDC_DRIFT_DIS(k),SDC_WIRE_COORD(k)
 1002       format(2x,i3,i4,4x,e16.8,2x,e16.8,2x,e16.8)
          enddo
          write(sluno,*) ' '
        enddo
      endif
      return
      end
