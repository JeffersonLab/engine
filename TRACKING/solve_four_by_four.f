      subroutine  solve_four_by_four(TT,AA,stub,ierr)
*     Explicit solution of a symmetric four by four equation TT = AA * STUB
*     Remember AA must be a symmetrix matrix
*     Used in find_best_stub.f and h_track_fit
*     d.f. geesaman       1 september 1993
* $Log: solve_four_by_four.f,v $
* Revision 1.1  1994/02/21 16:44:35  cdaq
* Initial revision
*
*
      implicit none
*     input quantities
      real*8 TT(4),AA(4,4)
*
*     output quantities
      real*8 stub(4)
      integer*4 ierr                       ! ierr = 0 means valid solution
*
*     local quantities
      real*8 T1,T2,T3,T4,T5,T6,T7,T8,T10,T11,T12,T13,T14,T15,T16,T17,T18
      real*8 B11,B12,B13,B14,B22,B23,B24,B33,B34,B44,DET
      ierr=1
*
      T1=AA(3,3)*AA(4,4)-AA(3,4)*AA(3,4)
      T2=AA(2,3)*AA(4,4)-AA(2,4)*AA(3,4)
      T3=AA(2,3)*AA(3,4)-AA(2,4)*AA(3,3)
      T4=AA(1,3)*AA(2,4)-AA(1,4)*AA(2,3)
      T5=AA(1,2)*AA(2,4)-AA(1,4)*AA(2,2)
      T6=AA(1,2)*AA(2,3)-AA(1,3)*AA(2,2)
      T7=AA(1,3)*AA(4,4)-AA(1,4)*AA(3,4)
      T8=AA(1,3)*AA(3,4)-AA(1,4)*AA(3,3)
      T10=AA(1,2)*AA(4,4)-AA(1,4)*AA(2,4)
      T11=AA(1,2)*AA(3,4)-AA(1,4)*AA(2,3)
      T13=AA(1,2)*AA(3,4)-AA(1,3)*AA(2,4)
      T14=AA(1,2)*AA(3,3)-AA(1,3)*AA(2,3)
      T15=AA(2,2)*AA(4,4)-AA(2,4)*AA(2,4)
      T16=AA(2,2)*AA(3,4)-AA(2,4)*AA(2,3)
      T17=AA(1,2)*AA(2,3)-AA(1,3)*AA(2,2)
      T18=AA(2,2)*AA(3,3)-AA(2,3)*AA(2,3)
      B11=AA(2,2)*T1-AA(2,3)*T2+AA(2,4)*T3
      B12= -(AA(1,2)*T1-AA(1,3)*T2+AA(1,4)*T3)
      B13= AA(2,4)*T4-AA(3,4)*T5+AA(4,4)*T6
      B14= -(AA(2,3)*T4-AA(3,3)*T5+AA(3,4)*T6)
      DET= AA(1,1)*B11+AA(1,2)*B12+AA(1,3)*B13+AA(1,4)*B14
 
*     if determinant is finite then continue, otherwise quit
      if(abs(DET).gt. 1e-20) then
        ierr=0
        B11=B11/DET
        B12=B12/DET
        B13=B13/DET
        B14=B14/DET
        B22=(AA(1,1)*T1-AA(1,3)*T7+AA(1,4)*T8)/DET
        B23= -(AA(1,1)*T2-AA(1,3)*T10+AA(1,4)*T11)/DET
        B24=(AA(1,1)*T3-AA(1,3)*T13+AA(1,4)*T14)/DET
        B33=(AA(1,1)*T15-AA(1,2)*T10+AA(1,4)*T5)/DET
        B34= -(AA(1,1)*T16-AA(1,2)*T13+AA(1,4)*T17)/DET
        B44= (AA(1,1)*T18-AA(1,2)*T14+AA(1,3)*T17)/DET
*     Calculate results
        stub(3)=B13*TT(1)+B23*TT(2)+B33*TT(3)+B34*TT(4)
        stub(1)=B11*TT(1)+B12*TT(2)+B13*TT(3)+B14*TT(4)
        stub(4)=B14*TT(1)+B24*TT(2)+B34*TT(3)+B44*TT(4)
        stub(2)=B12*TT(1)+B22*TT(2)+B23*TT(3)+B24*TT(4)
      endif                                   !   end check on determinant
*
      return
      end
