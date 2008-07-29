      subroutine h_stub_t0_calc(abort,err)

      implicit none
      save

      logical abort
      character*(*) err

      include 'hms_data_structures.cmn'
      include 'hms_tracking.cmn'
      include 'hms_geometry.cmn'

      real*4 t0best
      real*4 t0temp,tdrifttemp,ddrifttemp
      integer*4 planetemp,wiretemp
      real*4 chi2,minchi2

      real*4 leftright(hmax_hits_per_point)
      real*8 TT(3)
      real*8 dstub(3)
      real*4 stub(4),beststub(5)
      real*4 dpos(hmax_hits_per_point)

      real*4 h_drift_dist_calc
      external h_drift_dist_calc

      integer isp,ihit,numhits,currenthit,j
      integer pindex

      logical firststep

      abort = .false.
      err = ' '

      if(h_stub_t0_step.le.0.) h_stub_t0_step = 1.
      if(h_stub_t0_slop.lt.0.) h_stub_t0_slop = abs(h_stub_t0_slop)

      !write(*,*) 'NEW EVENT'

      do isp=1,hnspace_points_tot

         !write(*,*) 'space point #',isp,' DC #',
c     $        hdc_plane_num(hspace_point_hits(isp,3))/6+1
         !write(*,*) 'old x,y,dx/dz,dy/dz,t0=',(hbeststub(isp,j),j=1,5)

         numhits = hspace_point_hits(isp,1)
c     get the left-right value of each hit: this only works if h_left_right has already been called!
         do ihit=1,numhits
            if(hdc_wire_coord(hspace_point_hits(isp,ihit+2)).lt.
     $           hdc_wire_center(hspace_point_hits(isp,ihit+2))) then
               leftright(ihit) = -1.
            else
               leftright(ihit) =  1.
            endif
         enddo

c     get the pattern index for this space point:
         pindex = int(hbeststub(isp,6))
         
         t0temp = -h_stub_t0_slop / 2.

         firststep = .true.

c     loop over t0 and recalculate stub chi2 for each t0:

         do while (t0temp.le.h_stub_t0_slop/2.)
            
c     calculate trial hit positions for current value of t0 and least-squares matrix coeffs:
            TT(1) = 0.
            TT(2) = 0.
            TT(3) = 0.

            do ihit=1,numhits
               currenthit = hspace_point_hits(isp,ihit+2)
               planetemp = hdc_plane_num(currenthit)
               wiretemp = hdc_wire_num(currenthit)
               tdrifttemp = t0temp + hdc_drift_time(currenthit)
               ddrifttemp = h_drift_dist_calc(planetemp,wiretemp,tdrifttemp)

               dpos(ihit) = hdc_wire_center(currenthit) + 
     $              leftright(ihit)*ddrifttemp - hpsi0(planetemp)
               do j=1,3
                  TT(j) = TT(j) + ((dpos(ihit))*hstubcoef(planetemp,j))
     $                 / hdc_sigma(planetemp)
               enddo
            enddo

            dstub(1)=HAAINV3(1,1,pindex)*TT(1) + HAAINV3(1,2,pindex)*TT(2) +
     &           HAAINV3(1,3,pindex)*TT(3)
            dstub(2)=HAAINV3(1,2,pindex)*TT(1) + HAAINV3(2,2,pindex)*TT(2) +
     &           HAAINV3(2,3,pindex)*TT(3)
            dstub(3)=HAAINV3(1,3,pindex)*TT(1) + HAAINV3(2,3,pindex)*TT(2) +
     &           HAAINV3(3,3,pindex)*TT(3)
            
            chi2=0.
            stub(1)=dstub(1)
            stub(2)=dstub(2)
            stub(3)=dstub(3)
            stub(4)=0.

            do ihit=1,numhits
               planetemp = hdc_plane_num(hspace_point_hits(isp,ihit+2))
               chi2=chi2+((dpos(ihit))/hdc_sigma(planetemp)
     &              -hstubcoef(planetemp,1)*stub(1)
     &              -hstubcoef(planetemp,2)*stub(2)
     &              -hstubcoef(planetemp,3)*stub(3) )**2
            enddo

            if(firststep.or.chi2.lt.minchi2) then
               minchi2 = chi2
               firststep=.false.
               t0best = t0temp
c     store stub fit results
               do j=1,4
                  beststub(j) = stub(j)
               enddo
            endif

            t0temp = t0temp + h_stub_t0_step
         enddo ! end loop over t0
c     now recalculate final coordinate:
         do ihit=1,numhits
            currenthit = hspace_point_hits(isp,ihit+2)
            planetemp = hdc_plane_num(currenthit)
            wiretemp = hdc_wire_num(currenthit)
            tdrifttemp = t0best + hdc_drift_time(currenthit)
            ddrifttemp = h_drift_dist_calc(planetemp,wiretemp,tdrifttemp)
            
            hdc_wire_coord(currenthit) = hdc_wire_center(currenthit) + 
     $           leftright(ihit)*ddrifttemp
         enddo
c     recalculate rotated best stub fit parameters using formulas from h_left_right.f
c     use first hit to determine chamber:
         currenthit = hspace_point_hits(isp,3)
         
         planetemp = hdc_plane_num(currenthit)
         
         stub(3) = ( beststub(3) - htanbeta(planetemp) ) /
     $        ( 1. + beststub(3) * htanbeta(planetemp) )
         stub(4) = beststub(4) / 
     $        (beststub(3)*hsinbeta(planetemp) + hcosbeta(planetemp) )
         stub(1) = beststub(1)*hcosbeta(planetemp) - 
     $        beststub(1)*stub(3)*hsinbeta(planetemp)
         stub(2) = beststub(2) -
     $        beststub(1)*stub(4)*hsinbeta(planetemp)

         hbeststub(isp,1) = stub(1)
         hbeststub(isp,2) = stub(2)
         hbeststub(isp,3) = stub(3)
         hbeststub(isp,4) = stub(4)
         hbeststub(isp,5) = t0best
         
         
         !write(*,*) 'new x,y,dx/dz,dy/dz,t0=',(hbeststub(isp,j),j=1,5)
         !write(*,*) 'chi2=',minchi2

      enddo                     ! end loop over space points

      return 
      end
      
