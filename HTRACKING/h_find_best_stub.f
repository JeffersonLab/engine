      subroutine h_find_best_stub(numhits,hits,pl,pindex,plusminus,stub,chi2)
*     This subroutine does a linear least squares fit of a line to the
*     hits in an individual chamber. It assumes that the y slope is 0 
*     The wire coordinate is calculated
*     from the wire center + plusminus*(drift distance).
*     This is called in a loop over all combinations of plusminus
*     
*     d. f. geesaman
* $Log$
* Revision 1.3  1994/11/22 20:04:56  cdaq
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
      include "gen_data_structures.cmn"
      include "hms_tracking.cmn"
      include "hms_geometry.cmn"

*     input quantities
      integer*4 numhits
      integer*4 hits(*)
      real*4 plusminus(*)
*
*     output quantitites
      real*8 dstub(4)               !, x, xp , y, yp of local line fit
      real*4 stub(4)
      real*4 chi2                  ! chi2 of fit      
*
*     local variables
      real*4 position(hmax_hits_per_point)
      integer*4 pl(hmax_hits_per_point) !keep name same as in h_left_right.f
      integer*4 pindex                  ! passed from h_left_right to h_solve_3by3
      real*8 TT(3)
      integer*4 ihit,ierr
      integer*4 i
*

      chi2=10000.
*     calculate trial hit position
      do ihit=1,numhits
         position(ihit)=HDC_WIRE_CENTER(hits(ihit)) +
     &          plusminus(ihit)*HDC_DRIFT_DIS(hits(ihit))
      enddo

*     calculate least squares matrix coefficients
      do i=1,3
        TT(i)=0.
          do ihit=1,numhits
           TT(i)=TT(i)+((position(ihit)-hpsi0(pl(ihit)))*
     &          hstubcoef(pl(ihit),i)) /hdc_sigma(pl(ihit))
         enddo
       enddo
*
* djm 10/2/94 removed repetitive calculations of matrix AA3. This matrix and its
* inverse now calculated for the 14 most popular hit plane configurations and stored 
* at initialization. (See h_generate_geometry.f)
 
* solve three by three equations using stored inverse matrix 
      call h_solve_3by3(TT,pindex,dstub,ierr)
*
      if(ierr.ne.0) then
         stub(1)=10000.
         stub(2)=10000.
         stub(3)=2.
         stub(4)=2.
      else
*      calculate chi2
*     remember one power of sigma is in sstubcoef
        chi2=0.
        stub(1)=dstub(1)
        stub(2)=dstub(2)
        stub(3)=dstub(3)
        stub(4)=0.
        do ihit=1,numhits
         chi2=chi2+((position(ihit)-hpsi0(pl(ihit)))/hdc_sigma(pl(ihit))
     &         -hstubcoef(pl(ihit),1)*stub(1)
     &         -hstubcoef(pl(ihit),2)*stub(2)
     &         -hstubcoef(pl(ihit),3)*stub(3) )**2
        enddo
      endif
      return
      end
