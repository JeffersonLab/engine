*
*     Wrapperse for G77 intrinsic functions that have different names from
*     other f77's
*
* $Log: other_wrappers.f,v $
* Revision 1.1  2000/11/30 14:24:44  saw
* JIDNNT function
*
*
* JIDNNT Return  nearest INT for a REAL*16 number
*
      integer*4 function jidnnt(f)
      real*8 f
*
      jidnnt = nint(f)
      return
      end

