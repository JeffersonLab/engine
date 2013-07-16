      subroutine s_print_tracks
*     prints the output of track matching
*     d.f. geesaman           7 Sept 1993
* $Log: s_print_tracks.f,v $
* Revision 1.3  1995/05/22 19:45:47  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.2  1994/06/07  04:43:40  cdaq
* (DFG) print warning if ssingle_stub is set
*
* Revision 1.1  1994/02/21  16:40:41  cdaq
* Initial revision
*
*
      implicit none
      include "sos_data_structures.cmn"
      include "sos_tracking.cmn"
*
      external S_DPSIFUN
      real*8 S_DPSIFUN
*     local variables
      integer*4 itrack,ihit
      integer*4 hitnum,planenum
      real*8 ray(snum_fpray_param),calculated_position,residual
      if(SNTRACKS_FP.gt.0) then
         if(ssingle_stub.ne.0) then
            write(sluno,'(''  Warning - ssingle_stub is set'')')
         endif
         write(sluno,'('' point     x_t             y_t     '',
     &        ''        xp_t        yp_t   chi**2 degrees of'')')
         write(sluno,'(''           [cm]            [cm]    '',
     &        ''        [rad]       [rad]          freedom'')')
         do itrack=1,SNTRACKS_FP
 1001       format(1x,i3,2x,4e14.6,e10.3,1x,i3)
            write(sluno,1001) itrack,SX_FP(itrack),SY_FP(itrack),
     &           SXP_FP(itrack),SYP_FP(itrack),SCHI2_FP(itrack),
     &           SNFREE_FP(itrack)
         enddo
         do itrack=1,SNTRACKS_FP
            strack_fit_num=itrack
            ray(1)=dble(SX_FP(itrack))
            ray(2)=dble(SY_FP(itrack))
            ray(3)=dble(SXP_FP(itrack))
            ray(4)=dble(SYP_FP(itrack))
            write(sluno,'(a,i3)') ' Hits in SOS track number',itrack
            write(sluno,'(a)') 
     &           '   hit  plane  SDC_WIRE_COORD   FIT POSITION    RESIDUAL'
*     
            do ihit=1,SNTRACK_HITS(itrack,1)
               hitnum=SNTRACK_HITS(itrack,ihit+1)
               planenum=SDC_PLANE_NUM(hitnum)
               calculated_position=S_DPSIFUN(ray,planenum)
               residual=dble(SDC_WIRE_COORD(hitnum))-calculated_position
               write(sluno,1011) hitnum,planenum,SDC_WIRE_COORD(hitnum),
     &              calculated_position,residual
 1011          format(3x,i3,3x,i3,3x,e15.7,2d15.7)
            enddo
         enddo
      endif
      return
      end

