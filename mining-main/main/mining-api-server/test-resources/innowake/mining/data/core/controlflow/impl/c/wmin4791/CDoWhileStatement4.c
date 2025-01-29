#include <stdio.h>
int main() {
  do{
    printf("inside the first while");
    ++i;
  } while (i <= 5);
  printf("outside the while");
  do{
    printf("inside the second while");
    ++i;
  } while (i <= 7);
}