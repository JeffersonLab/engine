      subroutine s_find_best_stub(numhits,hits,pl,pindex,plusminus,stub,chi2)
*     This subroutine does a linear least squares fit of a line to the
*     hits in an individual chamber. It assumes that the y slope is 0 
*     The wire coordinate is calculated
*     from the wire center + plusminus*(drift distance).
*     This is called in a loop over all combinations of plusminus
*     
*     d. f. geesaman
* $Log: s_find_best_stub.f,v $
* Revision 1.5  1996/01/17 19:05:23  cdaq
* (JRA)
*
* Revision 1.4  1995/10/10 13:39:56  cdaq
* (JRA) Cleanup
*
* Revision 1.3  1995/05/22 19:45:39  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.2  1994/11/22  21:11:50  cdaq
* (SPB) Recopied from hms file and modified names for SOS
*
* Revision 1.1  1994/02/21  16:13:42  cdaq
* Initial revision
*
*
*     the four parameters of a stub are x_t,y_t,xp_t,yp_t
*
*     Called by S_LEFT_RIGHT
*
      implicit none
      include 'sos_data_structures.cmn'
      include 'sos_tracking.cmn'
      include 'sos_geometry.cmn'
*     input quantities
      integer*4 numhits
      integer*4 hits(*)
      real*4 plusminus(*)
*
*     output quantitites
      real*8 dstub(3)               ! x, xp , y of local line fit
      real*4 stub(4)
      real*4 chi2                  ! chi2 of fit      
*
*     local variables
      real*4 dpos(smax_hits_per_point)
      integer*4 pl(smax_hits_per_point)
      integer*4 pindex
      real*8 TT(3)
      integer*4 hit
      integer*4 i

      TT(1)=0.
      TT(2)=0.
      TT(3)=0.

* calculate trail hit position and least squares matrix coefficients.
      do hit=1,numhits
        dpos(hit)=SDC_WIRE_CENTER(hits(hit)) +
     &       plusminus(hit)*SDC_DRIFT_DIS(hits(hit)) -
     &       spsi0(pl(hit))
        do i=1,3
          TT(i)=TT(i)+(dpos(hit)*sstubcoef(pl(hit),i))/sdc_sigma(pl(hit))
        enddo
      enddo
*
*     solve three by three equations
ccc      call s_solve_3by3(TT,pindex,dstub,ierr)

      dstub(1)=SAAINV3(1,1,pindex)*TT(1) + SAAINV3(1,2,pindex)*TT(2) +
     &     SAAINV3(1,3,pindex)*TT(3)
      dstub(2)=SAAINV3(1,2,pindex)*TT(1) + SAAINV3(2,2,pindex)*TT(2) +
     &     SAAINV3(2,3,pindex)*TT(3)
      dstub(3)=SAAINV3(1,3,pindex)*TT(1) + SAAINV3(2,3,pindex)*TT(2) +
     &     SAAINV3(3,3,pindex)*TT(3)

*
* calculate chi2.  Remember one power of sigma is in sstubcoef
      chi2=0.
      stub(1)=dstub(1)
      stub(2)=dstub(2)
      stub(3)=dstub(3)
      stub(4)=0.
      do hit=1,numhits
        chi2=chi2+(dpos(hit)/sdc_sigma(pl(hit))
     &       -sstubcoef(pl(hit),1)*stub(1)
     &       -sstubcoef(pl(hit),2)*stub(2)
     &       -sstubcoef(pl(hit),3)*stub(3) )**2
      enddo
      return
      end
