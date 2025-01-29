#include <stdio.h>
void print(void (*f1)()); 
void print2(void (*f2)());  
void summ();  
void display();  
void helloworld(void (*f)(), void (*p)());
int main(void)  
{
    helloworld(display(), print(print2(summ())));
    return (0);
}
void print(void (*f1)())  
{
	f1();
}
void print2(void (*f2)())  
{
    f2();
}
void summ()  
{
    printf("sum");
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
