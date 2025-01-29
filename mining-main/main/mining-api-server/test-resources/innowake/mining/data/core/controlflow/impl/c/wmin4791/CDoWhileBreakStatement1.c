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
}