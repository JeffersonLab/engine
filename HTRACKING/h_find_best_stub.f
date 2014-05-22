      subroutine h_find_best_stub(numhits,hits,pl,pindex,plusminus,stub,chi2)
*     This subroutine does a linear least squares fit of a line to the
*     hits in an individual chamber. It assumes that the y slope is 0 
*     The wire coordinate is calculated
*     from the wire center + plusminus*(drift distance).
*     This is called in a loop over all combinations of plusminus
*     
*     d. f. geesaman
* $Log: h_find_best_stub.f,v $
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
      real*4 stub(4)
      real*4 chi2                  ! chi2 of fit      
*
*     local variables
      real*4 dpos(hmax_hits_per_point)
      integer*4 pl(hmax_hits_per_point) !keep name same as in h_left_right.f
      integer*4 pindex                  ! passed from h_left_right to h_solve_3by3
      real*8 TT(3)
      integer*4 hit
      integer*4 i
*
      TT(1)=0.
      TT(2)=0.
      TT(3)=0.

* calculate trial hit position and least squares matrix coefficients.
      do hit=1,numhits
        dpos(hit)=HDC_WIRE_CENTER(hits(hit)) +
     &       plusminus(hit)*HDC_DRIFT_DIS(hits(hit)) -
     &       hpsi0(pl(hit))
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
      do hit=1,numhits
        chi2=chi2+((dpos(hit))/hdc_sigma(pl(hit))
     &       -hstubcoef(pl(hit),1)*stub(1)
     &       -hstubcoef(pl(hit),2)*stub(2)
     &       -hstubcoef(pl(hit),3)*stub(3) )**2
      enddo
      return
      end
