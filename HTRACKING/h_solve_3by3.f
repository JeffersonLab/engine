      subroutine h_solve_3by3(TT,pindex,stub,ierr)
*     Explicit solution of a symmetric three by three equation TT = AA * STUB
*     Remember AA must be a symmetrix matrix
*     Used in find_best_stub.f

* $Log: h_solve_3by3.f,v $
* Revision 1.4  1995/10/10 17:36:37  cdaq
* (JRA) Cleanup
*
* Revision 1.3  1995/05/22 19:39:27  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.2  1994/11/22  20:07:20  cdaq
* (SAW) Change name, add h to aainv3, move to HTRACKING directory.
*
* Revision 1.1  1994/10/12  18:42:59  cdaq
* Initial revision
*
*
* djm 10/2/94
* The present version replaces solve_three_by_three(TT,AA,stub,ierr) in 
* find_best_stub. New version is entirely based on dfg's version, but matrix 
* inversion is now done only at initialization for faster event sorting.

*
      implicit none
      include 'hms_data_structures.cmn'
      include 'hms_tracking.cmn'


*     input quantities
      real*8 TT(3) 
      integer*4 pindex
*
*     output quantities
      real*8 stub(3)
      integer*4 ierr                       ! ierr = 0 means valid solution
*
*
      if(pindex.le.14)then     !accept 5/6 or 6/6 good planes
        ierr=0
        stub(1)=HAAINV3(1,1,pindex)*TT(1) + HAAINV3(1,2,pindex)*TT(2) + 
     & HAAINV3(1,3,pindex)*TT(3)
        stub(2)=HAAINV3(1,2,pindex)*TT(1) + HAAINV3(2,2,pindex)*TT(2) + 
     & HAAINV3(2,3,pindex)*TT(3)
        stub(3)=HAAINV3(1,3,pindex)*TT(1) + HAAINV3(2,3,pindex)*TT(2) + 
     & HAAINV3(3,3,pindex)*TT(3)
      else
        ierr=1
      endif          !end test on plane index
 
*      write(6,*)'TT i=1,2,3',TT(1),TT(2),TT(3)
*
*      write(6,*)'aainv(1,1,) (1,2,) (1,3,)',aainv(1,1,pindex),
*     &    aainv(1,2,pindex),aainv(1,3,pindex)
*
*      write(6,*)'aainv(2,2) (2,3) (3,3)',aainv(2,2,pindex),
*     &    aainv(2,3,pindex),aainv(3,3,pindex)
*
*     write(6,*)
     
      return

      end
