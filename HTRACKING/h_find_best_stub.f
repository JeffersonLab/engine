      subroutine h_find_best_stub(numhits,hits,pl,pindex,plusminus,stub,chi2)
*     This subroutine does a linear least squares fit of a line to the
*     hits in an individual chamber. It assumes that the y slope is 0 
*     The wire coordinate is calculated
*     from the wire center + plusminus*(drift distance).
*     This is called in a loop over all combinations of plusminus
*     
*     d. f. geesaman
* $Log$
* Revision 1.6.24.1  2008/07/29 16:44:58  puckett
* added option to find best t0 offset for stub, etc
*
* Revision 1.6  1996/01/16 21:51:00  cdaq
* (JRA)
*
* Revision 1.5  1995/10/10 13:37:10  cdaq
* (JRA) Cleanup
*
* Revision 1.4  1995/05/22 19:39:12  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.3  1994/11/22  20:04:56  cdaq
* (SAW) Matrix solver routine now called h_solve_3by3
*
* Revision 1.2  1994/10/12  18:38:46  cdaq
* (DJM) Don't recalculate plane array, remove repetitive calc of AA matrix
*
* Revision 1.1  1994/02/19  06:14:29  cdaq
* Initial revision
*
*
*     the four parameters of a stub are x_t,y_t,xp_t,yp_t
*
*     Called by H_LEFT_RIGHT
*
      implicit none
      include "hms_data_structures.cmn"
      include "hms_tracking.cmn"
      include "hms_geometry.cmn"

*     input quantities
      integer*4 numhits
      integer*4 hits(*)
      real*4 plusminus(*)
*
*     output quantitites
      real*8 dstub(3)               !x,y,xp of local line fit
cajp051608      real*4 stub(4) added t0 to stub parameters
      real*4 stub(4)
      real*4 chi2,minchi2                  ! chi2 of fit   

c     ajp 05/16/08
      real*4 tdrift_temp,ddrift_temp,t0_temp,t0_best
      integer istep
c     end ajp 05/16/08
   
*
*     local variables
      real*4 dpos(hmax_hits_per_point)
      integer*4 pl(hmax_hits_per_point) !keep name same as in h_left_right.f
      integer*4 pindex                  ! passed from h_left_right to h_solve_3by3
      real*8 TT(3)
      integer*4 hit
      integer*4 i
cajp 051708
      real*4 h_drift_dist_calc
      external h_drift_dist_calc

c end ajp 051708 
*
      TT(1)=0.
      TT(2)=0.
      TT(3)=0.

cajp 5/16/08 
      istep = 0

c$$$ 101  continue ! iterate stub t0
c$$$
c$$$      if(h_find_stub_t0) then
c$$$         t0_temp = -h_stub_t0_slop / 2. + istep * h_stub_t0_step
c$$$         istep = istep + 1
c$$$      endif

cajpend051608

* calculate trial hit position and least squares matrix coefficients.
      do hit=1,numhits
        dpos(hit)=HDC_WIRE_CENTER(hits(hit)) +
     &       plusminus(hit)*HDC_DRIFT_DIS(hits(hit)) -
     &       hpsi0(pl(hit))

c     ajp 5/16/08 -- add t0 shift to drift time
c$$$        if(h_find_stub_t0) then
c$$$           tdrift_temp = hdc_drift_time(hits(hit)) + t0_temp
c$$$           ddrift_temp = h_drift_dist_calc(pl(hit),
c$$$     $          hdc_wire_num(hits(hit)),tdrift_temp)
c$$$           dpos(hit)=hdc_wire_center(hits(hit)) + 
c$$$     $          plusminus(hit)*ddrift_temp -
c$$$     $          hpsi0(pl(hit))
c$$$        endif

c     end ajp 5/16/08
        do i=1,3
           TT(i)=TT(i)+((dpos(hit))*hstubcoef(pl(hit),i))/hdc_sigma(pl(hit))
        enddo
      enddo
*
* djm 10/2/94 removed repetitive calculations of matrix AA3. This matrix and its
* inverse now calculated for the 14 most popular hit plane configurations and stored 
* at initialization. (See h_generate_geometry.f)
 
* solve three by three equations using stored inverse matrix 
ccc      call h_solve_3by3(TT,pindex,dstub)

      dstub(1)=HAAINV3(1,1,pindex)*TT(1) + HAAINV3(1,2,pindex)*TT(2) +
     &     HAAINV3(1,3,pindex)*TT(3)
      dstub(2)=HAAINV3(1,2,pindex)*TT(1) + HAAINV3(2,2,pindex)*TT(2) +
     &     HAAINV3(2,3,pindex)*TT(3)
      dstub(3)=HAAINV3(1,3,pindex)*TT(1) + HAAINV3(2,3,pindex)*TT(2) +
     &     HAAINV3(3,3,pindex)*TT(3)

* calculate chi2.  Remember one power of sigma is in hstubcoef
      chi2=0.
      stub(1)=dstub(1)
      stub(2)=dstub(2)
      stub(3)=dstub(3)
      stub(4)=0.
c      stub(5)=0.
      do hit=1,numhits
         chi2=chi2+((dpos(hit))/hdc_sigma(pl(hit))
     &        -hstubcoef(pl(hit),1)*stub(1)
     &        -hstubcoef(pl(hit),2)*stub(2)
     &        -hstubcoef(pl(hit),3)*stub(3) )**2
      enddo

c     ajp 5/16/08

c$$$      if(h_find_stub_t0) then
c$$$         if(istep.le.1.or.chi2.lt.minchi2) then
c$$$            minchi2 = chi2
c$$$            t0_best = t0_temp
c$$$         endif
c$$$         
c$$$         if(t0_temp.le.h_stub_t0_slop/2.) then
c$$$            goto 101
c$$$         else                   ! finished finding t0: set stub t0 and chi2 to t0_best and minchi2, resp.
c$$$            stub(5) = t0_best
c$$$            chi2 = minchi2
c$$$         endif
c$$$      endif

c     end ajp 5/16/08
      return
      end
