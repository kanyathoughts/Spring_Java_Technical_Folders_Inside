#include<stdio.h>
int main()  {
  for (int i=1; i<6; i++) {
    printf("Inside the for loop");
    if(a > b){
      break;
    }
    printf("Should be unreachable");
  }
  printf("outside the for loop");
  
  for (int i=1; i<6; i++) {
    if(a > b) {
      continue;
    }
    printf("Unreachable Inside the for loop");
  }
}