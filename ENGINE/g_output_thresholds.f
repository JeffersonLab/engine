      subroutine g_output_thresholds(lunout,roc,slot,signalcount,
     &               elements_per_plane,signal0,signal1)
* $Log$
* Revision 1.1  1995/11/28 19:12:22  cdaq
* Initial revision
*
*
*
      implicit none
      save
*
      character*21 here
      parameter (here='g_output_thresholds')
*
      integer*4 lunout
      integer*4 roc,slot
      integer*4 signalcount,elements_per_plane
      real*4 signal0(*),signal1(*)
*
      integer*4 pln,cnt,element,sigtyp
      integer*4 ind,istart
*
      INCLUDE 'gen_decode_common.cmn'
      INCLUDE 'gen_detectorids.par'

      istart=g_decode_slotpointer(roc,slot)
      if (istart.eq.-1) then   !uninstrumented slot.
        write(lunout,*) 'roc#',roc,', slot#',slot,' is not in the map'
        return
      endif

      if (signalcount.eq.1) then           !cerenkov, calorimeter.
        do ind=istart,istart+g_decode_subaddcnt(roc,slot)-1
          pln=g_decode_planemap(ind)
          cnt=g_decode_countermap(ind)
c          if (pln.eq.0) then
          if (g_decode_didmap(ind).eq.UNINST_ID) then
            write(lunout,*) '4000'  ! set threshold very high if there is no signal
          else
            element=(pln-1)*elements_per_plane+cnt
            write(lunout,*) nint(signal0(element))
          endif
        enddo

      else if (signalcount.eq.2) then      !hodoscopes.
        do ind=istart,istart+g_decode_subaddcnt(roc,slot)-1
          pln=g_decode_planemap(ind)
          cnt=g_decode_countermap(ind)
          sigtyp=g_decode_sigtypmap(ind)
          element=pln+(cnt-1)*elements_per_plane  !convert 2d pln,cnt to 1d array
c          if (pln.eq.0) then
          if (g_decode_didmap(ind).eq.UNINST_ID) then
            write(lunout,*) '4000'  ! set threshold very high if there is no signal
          else
            if (sigtyp.eq.0) then
              write(lunout,*) nint(signal0(element))
            else if (sigtyp.eq.1) then
              write(lunout,*) nint(signal1(element))
            else
              write(6,*) 'sigtyp=',sigtyp,' in g_output_thresholds'
            endif
          endif
        enddo
      else
        write(6,*) 'signalcount=',signalcount,' in g_output_thresholds'
      endif

      return
      end
