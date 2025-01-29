#include <stdio.h>
int main() {
  while (i <= 5) {
    printf("inside the while1");
    ++i;
    if(a> b) {
      continue;
    }
    printf("Should be un reachable1");
  }
  printf("outside the while1");
  while (i <= 3) {
    printf("inside the while2");
    ++i;
    if(a >= b) {
      break;
    }
    printf("Should be un reachable2");
  }
  printf("outside the while2");
}