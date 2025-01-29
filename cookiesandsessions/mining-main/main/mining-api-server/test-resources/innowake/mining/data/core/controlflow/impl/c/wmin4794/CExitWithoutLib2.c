#include <stdio.h>
 
FILE *stream;
 
int main(void)
{
   if ((stream = fopen("mylib/myfile", "r")) == NULL)
   {
      perror("Could not open data file");
      exit(EXIT_FAILURE);
   }
   printf("Reach Sometime");
}