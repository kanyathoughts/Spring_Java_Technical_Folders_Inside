#include <stdio.h>
void swap(int *a, int *b);
int main()
{
    int n1,n2,sum;
	swap(&m, &n); 
    return 0;
}
void swap(int *a, int *b)
{
    int tmp;
    tmp = *a;
    *a = *b;
    *b = tmp;
}
