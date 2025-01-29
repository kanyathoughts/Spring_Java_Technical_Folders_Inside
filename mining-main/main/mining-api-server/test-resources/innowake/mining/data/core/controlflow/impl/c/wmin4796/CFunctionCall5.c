#include <stdio.h>
void print();
void display(void (*p)());  
void helloworld(void (*f)());  
int main(void)  
{
    helloworld(display(print()));
    return (0);
}
void print()  
{
    printf("Hello World!");
}
void display(void (*p)())  
{
    p();
}
void helloworld(void (*f)())  
{
    f();
}
