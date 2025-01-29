#include <stdio.h>
int main() {
  while (i <= 5) {
    printf("inside the first while");
    ++i;
  }
  printf("outside the first while");
  while (i <= 5) {
    printf("inside the second while");
    ++i;
  }
  printf("outside the second while");
}