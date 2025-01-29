#include <stdio.h>
int main() {
  do{
    printf("inside the while");
    ++i;
    if(i == 1){
      break;
    }
    printf("should be unreachable");
  } while (i <= 5);
  printf("outside the while");
  do{
    printf("inside the while2");
    ++i;
    break;
    printf("should be unreachable");
  } while (i <= 5);
}