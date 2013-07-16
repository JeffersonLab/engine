      integer function regparmstringarray(name, strings, size)
*
* $Log: regparmstringarray.f,v $
* Revision 1.1  1994/08/18 03:50:34  cdaq
* Initial revision
*
      character*(*) name, strings(*)
      integer*4 size
      
      regparmstringarray = 0
      return
      end
