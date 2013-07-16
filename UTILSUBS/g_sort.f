
      SUBROUTINE G_sort(N,size,threshold,ID) 
*----------------------------------------------------------------------
*- 
*-   Purpose and Methods : Sort numerically ordered input by decreasing size 
*- 
*-   Inputs  : N	- number of input quantities
*-           : size(*)	- input sizes in numerical order (not reorderd) 
*-           : threshold- minimum size to consider 
*-   Outputs : N	- number of input quantities (above threshold)
*-           : ID(*)	- ordered id numbers (above threshold)
*- 
*-   Created  8-Apr-1992   Kevin B. Beard
*-   Modified for hall C 9/1/93: KBB
*     $Log: g_sort.f,v $
*     Revision 1.1  1994/02/09 14:18:04  cdaq
*     Initial revision
*
*- 
*----------------------------------------------------------------------
      IMPLICIT NONE
      integer N,ID(*)
      real size(*),threshold 
      integer Nin,m,m1,temp_ID 
      logical significant,ordered_OK,swap
*----------------------------------------------------------------------
      Nin= N	!orig. number of inputs 
*
      IF(N.LE.0) THEN	!ignore if no elements
	N= 0
	RETURN
      ELSEIF(N.EQ.1) THEN	!simple
	significant= size(1).GE.threshold 
	If(significant) Then
	  id(1)= 1
	Else
	  N= 0
	EndIf
	RETURN
      ENDIF
*
*- pick out those above threshold
      N= 0 
      DO m=1,Nin 
	significant= size(m).GE.threshold 
	If(significant) Then		!above threshold
	  N= N+1			!# 
	  ID(N)= m			!ID's above thresh. in sequence
	EndIf 
      ENDDO
*
*- use a bubble sort to order by decreasing size 
      ordered_OK= .FALSE.
      DO WHILE (.not.ordered_OK) 
	ordered_OK= .TRUE.
	Do m=1,N-1			!1st element 
	  m1= m+1			!next element 
	  swap= size(ID(m)).LT.size(ID(m1)) 
	  if(swap) then			!swap pair
	    temp_ID= ID(m)
	    ID(m)= ID(m1) 
	    ID(m1)= temp_ID 
	    ordered_OK= .FALSE.		!swapped at least one pair 
	  endif	
	EndDo 
      ENDDO
*
      RETURN 
      END
