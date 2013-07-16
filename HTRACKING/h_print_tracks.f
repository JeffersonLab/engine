      subroutine h_print_tracks
*     prints the output of hms track fittinh
*     d.f. geesaman           17 January 1994
* $Log: h_print_tracks.f,v $
* Revision 1.3  1995/05/22 19:39:19  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.2  1994/06/06  16:42:03  cdaq
* (DFG) print warning if hsingle_stub is set.
*
* Revision 1.1  1994/02/19  06:17:36  cdaq
* Initial revision
*
      implicit none
      include "hms_data_structures.cmn"
      include "hms_tracking.cmn"
*
      external H_DPSIFUN
      real*8 H_DPSIFUN
*     local variables
      integer*4 itrack,ihit
      integer*4 hitnum,planenum
      real*8 ray(hnum_fpray_param),calculated_position,residual
      if(HNTRACKS_FP.gt.0) then
       if(hsingle_stub.ne.0) then
         write(hluno,'(''  Warning - hsingle_stub is set'')')
       endif
        write(hluno,'('' point     x_t             y_t     '',
     &           ''        xp_t        yp_t   chi**2 degrees of'')')
        write(hluno,'(''           [cm]            [cm]    '',
     &           ''        [rad]       [rad]          freedom'')')
          do itrack=1,HNTRACKS_FP
1001  format(1x,i3,2x,4e14.6,e10.3,1x,i3)
            write(hluno,1001) itrack,HX_FP(itrack),HY_FP(itrack),
     &           HXP_FP(itrack),HYP_FP(itrack),HCHI2_FP(itrack),
     &           HNFREE_FP(itrack)
          enddo
          do itrack=1,HNTRACKS_FP
              htrack_fit_num=itrack
              ray(1)=dble(HX_FP(itrack))
              ray(2)=dble(HY_FP(itrack))
              ray(3)=dble(HXP_FP(itrack))
              ray(4)=dble(HYP_FP(itrack))
      write(hluno,'(a,i3)') ' Hits in HMS track number',itrack
      write(hluno,'(a)') 
     &        '   hit  plane  HDC_WIRE_COORD   FIT POSITION    RESIDUAL'
*
              do ihit=1,HNTRACK_HITS(itrack,1)
                hitnum=HNTRACK_HITS(itrack,ihit+1)
                planenum=HDC_PLANE_NUM(hitnum)
                calculated_position=H_DPSIFUN(ray,planenum)
                residual=dble(HDC_WIRE_COORD(hitnum))-calculated_position
                write(hluno,1011) hitnum,planenum,HDC_WIRE_COORD(hitnum),
     &                    calculated_position,residual
1011  format(3x,i3,3x,i3,3x,e15.7,2d15.7)
              enddo
          enddo
        endif
      return
      end

