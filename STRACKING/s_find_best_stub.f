      subroutine s_find_best_stub(numhits,hits,pl,pindex,plusminus,stub,chi2)
*     This subroutine does a linear least squares fit of a line to the
*     hits in an individual chamber. It assumes that the y slope is 0 
*     The wire coordinate is calculated
*     from the wire center + plusminus*(drift distance).
*     This is called in a loop over all combinations of plusminus
*     
*     d. f. geesaman
* $Log$
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
      real*8 dstub(4)               !, x, xp , y, yp of local line fit
      real*4 stub(4)
      real*4 chi2                  ! chi2 of fit      
*
*     local variables
      real*4 position(smax_hits_per_point)
      integer*4 pl(smax_hits_per_point)
      integer*4 pindex
      real*8 TT(3)
      integer*4 ihit,ierr
      integer*4 i
*
      chi2=10000.
*     calculate trail hit position
      do ihit=1,numhits
         position(ihit)=SDC_WIRE_CENTER(hits(ihit)) +
     &          plusminus(ihit)*SDC_DRIFT_DIS(hits(ihit))
      enddo
*     calculate least squares matrix coefficients
      do i=1,3
        TT(i)=0.
          do ihit=1,numhits
           TT(i)=TT(i)+((position(ihit)-spsi0(pl(ihit)))*
     &          sstubcoef(pl(ihit),i)) /sdc_sigma(pl(ihit))

         enddo
      enddo
*
*     solve four by four equations
      call s_solve_3by3(TT,pindex,dstub,ierr)
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
          chi2=chi2+((position(ihit)-spsi0(pl(ihit)))/sdc_sigma(pl(ihit))
     &         -sstubcoef(pl(ihit),1)*stub(1)
     &         -sstubcoef(pl(ihit),2)*stub(2)
     &         -sstubcoef(pl(ihit),3)*stub(3) )**2
        enddo
      endif
      return
      end
