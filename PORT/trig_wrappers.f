*
*     Wrappers for F2C intrinsic functions that have different names
*     from standard f77s.
*
* $Log: trig_wrappers.f,v $
* Revision 1.1  1999/02/24 15:29:17  saw
* Add to CVS tree
*
* Revision 1.1  1996/09/09 13:34:26  saw
* Initial revision
*
*

      real*4 function sind(x)
      real*4 x
      sind = sin(x*3.1415926535/180)
      return
      end

      real*4 function cosd(x)
      real*4 x
      cosd = cos(x*3.1415926535/180)
      return
      end

      real*4 function tand(x)
      real*4 x
      tand = tan(x*3.1415926535/180)
      return
      end
