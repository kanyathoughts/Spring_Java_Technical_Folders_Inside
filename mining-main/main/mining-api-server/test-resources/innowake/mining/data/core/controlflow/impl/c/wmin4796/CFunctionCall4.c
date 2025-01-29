#include <stdio.h>
void print();
void display();
void helloworld(void (*f)(), void (*p)());
int main(void)  
{
    helloworld(display(), print());
    return (0);
}
void print()  
{
    printf("Hello World!");
}
void display()  
{
    printf("Hello World!");
}
void helloworld(void (*f)(), void (*p)())  
{
    f();
    p();
}
