      subroutine g_output_thresholds(lunout,roc,slot,signalcount,
     &               elements_per_plane,signal0,signal1)
* $Log$
* Revision 1.2  1996/01/16 18:13:50  cdaq
* (JRA) Warn if thresholds change by too much
*
* Revision 1.1  1995/11/28 19:12:22  cdaq
* Initial revision
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
      real*4 sigma0(*),sigma1(*)
      real*4 delta_ped
c      integer*4 thresholds(*)
*
      integer*4 pln,cnt,element,sigtyp
      integer*4 ich,ind,istart
      logical annoying_message
*
      INCLUDE 'gen_decode_common.cmn'
      INCLUDE 'gen_detectorids.par'

      annoying_message=.true.

      istart=g_decode_slotpointer(roc,slot)
      if (istart.eq.-1) then   !uninstrumented slot.
        write(lunout,*) 'roc#',roc,', slot#',slot,' is not in the map'
        return
      endif

      if (signalcount.eq.1) then           !cerenkov, calorimeter.
        do ich=1,g_decode_subaddcnt(roc,slot)
          ind=istart+ich-1
          pln=g_decode_planemap(ind)
          cnt=g_decode_countermap(ind)
          if (g_decode_didmap(ind).eq.UNINST_ID) then
            write(lunout,*) '4000'  ! set threshold very high if there is no signal
          else
            element=(pln-1)*elements_per_plane+cnt
            write(lunout,*) nint(signal0(element))
c      write(6,*) signal0(element),signal1(element),sigma0(element),sigma1(element)
c      write(6,*) 'g_threshold_readback(',ich,roc,slot,')=',g_threshold_readback(ich,roc,slot)
            delta_ped=signal0(element)-float(g_threshold_readback(ich,roc,slot))
            if ( (abs(delta_ped) .gt. min(20.,2.*sigma0(element)))  .and.
     &             g_threshold_readback(ich,roc,slot).ne.0) then
              if (annoying_message) then
                write(6,*) 'Warning! Danger Will Robinson!  Inconsistant Thresholds approaching!'
                write(6,*) '  roc slot channel threshold  calc.thresh. delta  #sigma(pos. is OK).'
                annoying_message=.false.
              endif
              write(6,'(2x,i3,i5,i6,2f11.1,f11.1,f9.1)') roc,slot,ich,
     &          g_threshold_readback(ich,roc,slot),signal0(element),delta_ped,delta_ped/(sigma0(element)+.001)
            endif
          endif
        enddo

      else if (signalcount.eq.2) then      !hodoscopes.
        do ich=1,g_decode_subaddcnt(roc,slot)
          ind=istart+ich-1
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
              delta_ped=signal0(element)-float(g_threshold_readback(ich,roc,slot))
              if ( (abs(delta_ped) .gt. min(20.,2.*sigma0(element)))  .and.
     &               g_threshold_readback(ich,roc,slot).ne.0) then
                if (annoying_message) then
                  write(6,*) 'Warning! Danger Will Robinson!  Inconsistant Thresholds approaching!'
                  write(6,*) '  roc slot channel threshold  calc.thresh. delta  #sigma(pos. is OK).'
                  annoying_message=.false.
                endif
              write(6,'(2x,i3,i5,i6,2f11.1,f11.1,f9.1)') roc,slot,ich,
     &          g_threshold_readback(ich,roc,slot),signal0(element),delta_ped,delta_ped/(sigma0(element)+.001)
              endif
            else if (sigtyp.eq.1) then
              write(lunout,*) nint(signal1(element))
              delta_ped=signal1(element)-float(g_threshold_readback(ich,roc,slot))
              if ( (abs(delta_ped) .gt. min(20.,2.*sigma1(element)))  .and.
     &               g_threshold_readback(ich,roc,slot).ne.0) then
                if (annoying_message) then
                  write(6,*) 'Warning! Danger Will Robinson!  Inconsistant Thresholds approaching!'
                  write(6,*) '  roc slot channel threshold  calc.thresh. delta  #sigma(pos. is OK).'
                  annoying_message=.false.
                endif
              write(6,'(2x,i3,i5,i6,2f11.1,f11.1,f9.1)') roc,slot,ich,
     &          g_threshold_readback(ich,roc,slot),signal1(element),delta_ped,delta_ped/(sigma1(element)+.001)
              endif
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
