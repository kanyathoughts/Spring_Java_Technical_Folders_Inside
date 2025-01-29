#include <stdio.h>
int main() {
  while (i <= 5) {
    printf("inside the first while");
    ++i;
  }
  printf("outside the while");
  while (i <= 5) {
    printf("inside the second while");
    ++i;
  }
}