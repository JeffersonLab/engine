      SUBROUTINE NO_Comments(string) 
*
* $Log$
* Revision 1.2  1994/06/06 04:39:27  cdaq
* (KBB) Speedup
*
* Revision 1.1  1994/02/22  20:01:25  cdaq
* Initial revision
*
*
      character*(*) string
      character*23 flag
      data flag/'!@#$%^&*<>[]{}*;?:"()~/'/ 
      integer string_length	!FUNCTION
c
c     strips out comments [including "quotes"]
c
      do i=1,string_length(string)
         if(INDEX(flag,string(i:i)).ne.0) then
            string(i:)=' '
            return
         elseif(string(i:).EQ.' ') then
            return
         endif 
      enddo
      return 
      end
