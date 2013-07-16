*
*     Wrappers for the ran function for f2c fortran
*
* $Log: ran_wrappers.f,v $
* Revision 1.1  1999/02/24 15:29:17  saw
* Add to CVS tree
*
*
      real*4 function ran(seed)
      integer*4 seed
      logical started
      save started
      real*4 RAND_MAX
      parameter(RAND_MAX=2147483647)
      integer*4 random
      real*4 fran

      data started /.false./
      
c
c     Ignore any seeds
c
      fran = (random()+1.0)/(RAND_MAX+1.0)
      ran = fran
      return
      end
