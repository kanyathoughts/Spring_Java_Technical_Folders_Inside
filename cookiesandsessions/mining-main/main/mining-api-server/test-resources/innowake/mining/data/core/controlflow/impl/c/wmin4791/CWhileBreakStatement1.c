#include <stdio.h>
int main() {
  while (i <= 5) {
    printf("inside the while");
    ++i;
    if(a> b) {
      break;
    }
    printf("Should be un reachable");
  }
  printf("outside the while");
}