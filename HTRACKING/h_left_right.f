      subroutine H_LEFT_RIGHT(ABORT,err)
*     This routine fits stubs to all possible left-right combinations of
*     drift distances and chooses the set with the minimum chi**2
*     It then fills the HDC_WIRE_COORD variable for each hit in a good
*     space point.
*     d. f. geesaman           17 January 1994
* $Log$
* Revision 1.1  1994/02/19 06:15:15  cdaq
* Initial revision
*
*
      implicit none
      save
       include 'gen_data_structures.cmn'
       include 'hms_tracking.cmn'
       include 'hms_geometry.cmn'
*
      externaljbit                            ! cernlib bit routine
      integer*4 jbit
*
*     local variables
*
       character*50 here
       parameter (here= 'H_LEFT_RIGHT')
*
       logical ABORT
       character*(*) err
      integer*4 isp, ihit,idummy,pmloop
      integer*4 nplusminus
      integer*4 numhits
      integer*4 hits(hmax_hits_per_point)
      integer*4 ichamber,plane
      real*4 plusminus(hmax_hits_per_point)
      real*4 plusminusbest(hmax_hits_per_point)
      real*4 chi2
      real*4 minchi2
      real*4 stub(4)
*
      ABORT= .FALSE.
      err=':'

      do isp=1,hnspace_points_tot             ! loop over all space points
          minchi2=1e10
          numhits=hspace_point_hits(isp,1)
          nplusminus=2**numhits
          do ihit=1,numhits
              hits(ihit)=hspace_point_hits(isp,2+ihit)
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
              call h_find_best_stub(numhits,hits,plusminus,stub,chi2)
              if(hdebugstubchisq.ne.0) then
                 write(hluno,'(''hms  pmloop='',i4,''   chi2='',e14.6)')
     &                     pmloop,chi2
              endif
              if (chi2.lt.minchi2)  then
                  minchi2=chi2
                  do idummy=1,numhits
                    plusminusbest(idummy)=plusminus(idummy)
                  enddo
                  do idummy=1,4
                    hbeststub(isp,idummy)=stub(idummy)
                  enddo
              endif                         ! end if on lower chi2
          enddo                             ! end loop on possible left-right
*
*     calculate final coordinate based on plusminusbest
*           
          do ihit=1,numhits
            HDC_WIRE_COORD(hspace_point_hits(isp,ihit+2))=
     &      HDC_WIRE_CENTER(hspace_point_hits(isp,ihit+2)) +
     &      plusminusbest(ihit)*HDC_DRIFT_DIS(hspace_point_hits(isp,ihit+2))
          enddo
*
*     stubs are calculated in rotated coordinate system
*     use first hit to determine chamber
          plane=HDC_PLANE_NUM(hits(1))
          stub(3)=(hbeststub(isp,3) - htanbeta(plane))
     &                /(1.0 + hbeststub(isp,3)*htanbeta(plane))
          stub(4)=hbeststub(isp,4)
     &                /(hbeststub(isp,3)*hsinbeta(plane)+hcosbeta(plane))

          stub(1)=hbeststub(isp,1)*hcosbeta(plane) 
     &             - hbeststub(isp,1)*stub(3)*hsinbeta(plane)
          stub(2)=hbeststub(isp,2) 
     &             - hbeststub(isp,1)*stub(4)*hsinbeta(plane)
          hbeststub(isp,1)=stub(1)
          hbeststub(isp,2)=stub(2)
          hbeststub(isp,3)=stub(3)
          hbeststub(isp,4)=stub(4)

*
      enddo                                   ! end loop over space points
*
*     write out results if debugflagstubs is set
      if(hdebugflagstubs.ne.0) then
         call h_print_stubs
      endif
      return
      end
        
