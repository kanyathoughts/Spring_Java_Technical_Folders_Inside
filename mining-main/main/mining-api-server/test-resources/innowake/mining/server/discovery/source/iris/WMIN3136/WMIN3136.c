#include<stdio.h>
#if defined(__VMS) || defined(VMS)
#  include "inc:WMIN3136.h"
#else
#  include "WMIN3136.h"
#endif
 
 int main () {
 	printf("Hello World");
 }