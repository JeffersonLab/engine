#include <stdlib.h>

long int random_(void){
  return(random());
}

void qsort_(void *base, int *nmemb, int *size,
	    int (*compar)(const void *,const void *))
{
  qsort(base,*nmemb,*size,compar);
}

// lshift and rshift are intrinsics provided by g77 and gfortran 4.2,
// but not by gfortran 4.1.1

int lshift_(int *Num2Shift, int *NumBits ) {
    return(*Num2Shift << *NumBits);
}

int rshift_(int *Num2Shift, int *NumBits ) {
    return(*Num2Shift >> *NumBits);
}
