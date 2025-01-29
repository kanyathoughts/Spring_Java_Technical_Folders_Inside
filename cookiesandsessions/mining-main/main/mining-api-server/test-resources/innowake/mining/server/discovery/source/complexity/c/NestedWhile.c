#include <stdio.h>
int main()
{
    int i=1,j;
    while (i <= 5)
    {
        j=1;
        while (j <= i  && i != 10)
        {
            printf("%d ",j);
            j++;
        }
        printf("n");
        i++;
    }
    return 0;
}