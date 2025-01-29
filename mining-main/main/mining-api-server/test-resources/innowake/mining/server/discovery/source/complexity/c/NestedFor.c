#include <stdio.h>
int main()
{
  // Iterate the loop for 5 times
  for (int i=1; i<6; i++)
  {
    // Iterate the loop for 8 times
    for (int j=1; j<9; j++)
    {
      // Print the value of i and j when both are equal
      if (i != j && i < j)
         printf("%d, %d\n",i ,j);
    }
  }
  return 0;
}