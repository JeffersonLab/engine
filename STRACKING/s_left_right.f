      subroutine S_LEFT_RIGHT(ABORT,err)
*     This routine fits stubs to all possible left-right combinations of
*     drift distances and chooses the set with the minimum chi**2
*     It then fills the SDC_WIRE_COORD variable for each hit in a good
*     space point.
*     d. f. geesaman           31 August 1993
* $Log$
* Revision 1.1  1994/02/21 16:14:42  cdaq
* Initial revision
*
*
      implicit none
      save
       include 'gen_data_structures.cmn'
       include 'sos_tracking.cmn'
       include 'sos_geometry.cmn'
*
      externaljbit                            ! cernlib bit routine
      integer*4 jbit
*
*     local variables
*
       character*50 here
       parameter (here= 'S_LEFT_RIGHT')
*
       logical ABORT
       character*(*) err
      integer*4 isp, ihit,idummy,pmloop
      integer*4 nplusminus
      integer*4 numhits
      integer*4 hits(smax_hits_per_point)
      integer*4 ichamber,plane
      real*4 plusminus(smax_hits_per_point)
      real*4 plusminusbest(smax_hits_per_point)
      real*4 chi2
      real*4 minchi2
      real*4 stub(4)
*
      ABORT= .FALSE.
      err=':'

      do isp=1,snspace_points_tot             ! loop over all space points
          minchi2=1e10
          numhits=sspace_point_hits(isp,1)
          nplusminus=2**numhits
          do ihit=1,numhits
              hits(ihit)=sspace_point_hits(isp,2+ihit)
          enddo
*     use bit value of integer word to set + or -
          do pmloop=0,nplusminus-1
              do ihit=1,numhits
                if(jbit(pmloop,ihit).eq.1) then
                   plusminus(ihit)=1.0
                else
                   plusminus(ihit)=-1.0
                endif
              enddo
              call s_find_best_stub(numhits,hits,plusminus,stub,chi2)
              if(sdebugstubchisq.ne.0) then
                 write(sluno,'('' pmloop='',i4,''   chi2='',e14.6)')
     &                     pmloop,chi2
              endif
              if (chi2.lt.minchi2)  then
                  minchi2=chi2
                  do idummy=1,numhits
                    plusminusbest(idummy)=plusminus(idummy)
                  enddo
                  do idummy=1,4
                    sbeststub(isp,idummy)=stub(idummy)
                  enddo
              endif                         ! end if on lower chi2
          enddo                             ! end loop on possible left-right
*
*     calculate final coordinate based on plusminusbest
*           
          do ihit=1,numhits
            SDC_WIRE_COORD(sspace_point_hits(isp,ihit+2))=
     &      SDC_WIRE_CENTER(sspace_point_hits(isp,ihit+2)) +
     &      plusminusbest(ihit)*SDC_DRIFT_DIS(sspace_point_hits(isp,ihit+2))
          enddo
*
*     stubs are calculated in rotated coordinate system
*     use first hit to determine chamber
          plane=SDC_PLANE_NUM(hits(1))
          stub(3)=(sbeststub(isp,3) - stanbeta(plane))
     &                /(1.0 + sbeststub(isp,3)*stanbeta(plane))
          stub(4)=sbeststub(isp,4)
     &                /(sbeststub(isp,3)*ssinbeta(plane)+scosbeta(plane))

          stub(1)=sbeststub(isp,1)*scosbeta(plane) 
     &             - sbeststub(isp,1)*stub(3)*ssinbeta(plane)
          stub(2)=sbeststub(isp,2) 
     &             - sbeststub(isp,1)*stub(4)*ssinbeta(plane)
          sbeststub(isp,1)=stub(1)
          sbeststub(isp,2)=stub(2)
          sbeststub(isp,3)=stub(3)
          sbeststub(isp,4)=stub(4)

*
      enddo                                   ! end loop over space points
*
*     write out results if sdebugflagstubs is set
      if(sdebugflagstubs.ne.0) then
         call s_print_stubs
      endif
      return
      end
        
