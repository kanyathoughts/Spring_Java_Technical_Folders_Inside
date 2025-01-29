#include<stdio.h>
int main()  {
  for (int i=1; i<6; i++)
  {
    printf("Inside the for loop");
    break;
  }
  printf("outside the for loop");
}