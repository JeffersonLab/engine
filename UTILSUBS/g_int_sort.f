
      SUBROUTINE G_int_sort(N,list,idx) 
*----------------------------------------------------------------------
*- 
*-   Purpose and Methods : Sort list by increasing value
*- 
*-   Inputs  : N	- number of input quantities
*-           : list(*)	- input lists in numerical order (not reorderd) 
*-           : threshold- minimum list to consider 
*-   Outputs : idx(*)	- ordered index pointers
*- 
*-   Created  17-Jul-1993 K.B.Beard
*-   Modified for hall C 9/1/93: KBB
*     $Log: g_int_sort.f,v $
*     Revision 1.1  1994/02/09 14:16:05  cdaq
*     Initial revision
*
*- 
*----------------------------------------------------------------------
      IMPLICIT NONE
      integer N,list(*),idx(*)
      integer m,m1,temp_ID
      logical ordered_OK,swap
*----------------------------------------------------------------------
*
      IF(N.LE.0) THEN	!ignore if no elements
	RETURN
      ELSEIF(N.EQ.1) THEN	!simple
	idx(1)= 1
	RETURN
      ENDIF
*
*- pick out those above threshold
      DO m= 1,N
	idx(m)= m
      ENDDO
*
*- use a bubble sort to order by increasing list 
      ordered_OK= .FALSE.
      DO WHILE (.not.ordered_OK) 
	ordered_OK= .TRUE.
	Do m=1,N-1			!1st element 
	  m1= m+1			!next element 
	  swap= list(idx(m)).GT.list(idx(m1)) 
	  if(swap) then			!swap pair
	    temp_ID= idx(m)
	    idx(m)= idx(m1) 
	    idx(m1)= temp_ID 
	    ordered_OK= .FALSE.		!swapped at least one pair 
	  endif	
	EndDo 
      ENDDO
*
      RETURN 
      END
