      INTEGER FUNCTION G_important_length(string)
*----------------------------------------------------------------------
*- 
*-   Purpose and Methods : return last nonblank,nonnull character position 
*-			   always return at least 1 
*- 
*-   Inputs  : string -  character string of any length
*- 
*-   Created  24-MAR-1992   Kevin B. Beard 
*-   Modified 9/1/93 for hall C: KBB
*     $Log: g_important_length.f,v $
*     Revision 1.1  1994/02/09 14:16:13  cdaq
*     Initial revision
*
*- 
*----------------------------------------------------------------------
      IMPLICIT NONE
      character*(*) string 
      integer m,length 
      integer LEN		!FUNCTION 
      logical one_character,more,nonblank_last,all_blank 
*----------------------------------------------------------------------
*
      length= LEN(string)
      one_character= length.EQ.1 
      nonblank_last= string(length:length).NE.' '
      all_blank= string.EQ.' ' 
*
      IF(all_blank) THEN 
	G_important_length= 1 
      ELSEIF(one_character .or. nonblank_last) THEN
	G_important_length= length
      ELSE		!longer than 1 character and last is blank 
	Do G_important_length= 1,length-1
	  IF(string(G_important_length+1:).EQ.' ') RETURN 
        EndDo
	G_important_length= 1 
      ENDIF
      RETURN 
      END
