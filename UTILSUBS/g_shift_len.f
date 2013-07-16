      subroutine G_shift_len(in,out,length)
*----------------------------------------------------------------------
*- 
*-   Purpose and Methods : copy "in" to "out", upshift "out" and
*-                         return important length of "out"
*- 
*-   Inputs  : in	-  character string of any length
*-   Outputs : out	-  character string of any length
*-           : length  -  important length of out
*-
*-   Created  8-Jul-1993 Kevin B. Beard 
*-   Modified 9/1/93 for hall C: KBB
*     $Log: g_shift_len.f,v $
*     Revision 1.2  1996/09/05 21:06:17  saw
*     (SAW) Change from function to subroutine
*
*     Revision 1.1  1994/02/09 14:17:47  cdaq
*     Initial revision
*
*- 
*----------------------------------------------------------------------
      IMPLICIT NONE
      character*(*) in,out
      integer length 
*
      integer G_important_length	!FUNCTION
*
*----------------------------------------------------------------------
*
      call ShiftAll(in,out)	!copy, upshift, remove leading blanks&tabs
      length= G_important_length(out)
      RETURN 
      END
