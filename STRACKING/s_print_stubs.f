      subroutine s_print_stubs
*     subroutine to dump output of S_LEFT_RIGHT
*     All the results are contained in sos_tracking.inc
*     d.f. geesaman          5 September 1993
* $Log$
* Revision 1.1  1994/02/21 16:38:06  cdaq
* Initial revision
*
      implicit none
      include "gen_data_structures.cmn"
      include "sos_tracking.cmn"
      include "sos_geometry.cmn"
*     local variables
      integer*4 i,j
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
        write(sluno,'('' hit         SDC_WIRE_CENTER  SDC_DRIFT_DIS     '',
     &         '' SDC_WIRE_COORD'')')
        do i=1,SDC_TOT_HITS
          write(sluno,1002) i,SDC_WIRE_CENTER(i),SDC_DRIFT_DIS(i),
     &       SDC_WIRE_COORD(i)
1002  format(3x,i3,4x,e16.8,2x,e16.8,2x,e16.8)
        enddo
      endif
      return
      end
