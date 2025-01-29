#include <stdio.h>
#include <stdlib.h>
 
int main(void)
{
   FILE *stream;
 
   if ((stream = fopen("mylib/myfile", "r")) == NULL)
   {
      perror("Could not open data file");
      abort();
   }
   printf("Reach Sometime");
}