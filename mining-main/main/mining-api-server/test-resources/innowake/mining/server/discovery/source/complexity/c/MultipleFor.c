#include <stdio.h>
int main () {
   int a,b;
   /* for loop execution */
   for( a = 10; a < 20; a = a + 1 ){
      printf("value of a: %d\n", a);
   }
   
   for( b = 15; b < 20; b = b + 1 ){
      printf("value of b: %d\n", b);
   }
   return 0;
}