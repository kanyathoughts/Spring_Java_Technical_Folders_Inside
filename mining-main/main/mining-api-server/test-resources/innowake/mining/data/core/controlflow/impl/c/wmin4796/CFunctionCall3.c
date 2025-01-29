#include <stdio.h>
void printNumber(int nbr);
void myFunction(void (*f)(int));
int main(void)  
{
    myFunction(printNumber(1));
    return (0);
}
void printNumber(int nbr)
{
    printf("%d\n", nbr);
}
void myFunction(void (*f)(int))
{
    printf("%d\n", nbr);
}
