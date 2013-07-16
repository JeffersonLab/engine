      SUBROUTINE NO_Comments(string) 
*
* $Log: no_comments.f,v $
* Revision 1.3  1996/05/24 16:01:47  saw
* (SAW) Relocate data statements for f2c compatibility
*
* Revision 1.2  1994/06/06 04:39:27  cdaq
* (KBB) Speedup
*
* Revision 1.1  1994/02/22  20:01:25  cdaq
* Initial revision
*
*
      character*(*) string
      integer string_length	!FUNCTION
      character*23 flag
      data flag/'!@#$%^&*<>[]{}*;?:"()~/'/ 
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
