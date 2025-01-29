#include <stdio.h>
int main()
{
  // Initialization the variable
  int n = 1,n2 =3;
 
  // Check the condition
  while (n <= 3)
  {
    // Print the message based on the value of n
    if( n == 1 && n2 <4)
      printf( "Good Morning\n");
    else if( n == 2)
      printf( "Good Afternoon\n");
    else
      printf( "Good Evening\n");

    //Increment the variable
    n++;
  }

  return 0;
}