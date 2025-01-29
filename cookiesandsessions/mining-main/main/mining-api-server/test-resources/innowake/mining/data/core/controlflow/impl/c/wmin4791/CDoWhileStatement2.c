#include <stdio.h>
int main() {
  do{
    printf("inside the while");
    ++i;
  } while (i <= 5);
  printf("outside the while");
}