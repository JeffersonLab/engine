#include <stdlib.h>

long int random_(void){
  return(random());
}

void qsort_(void *base, int *nmemb, int *size,
	    int (*compar)(const void *,const void *))
{
  qsort(base,*nmemb,*size,compar);
}
