#include <stdio.h>
void foo();
void bar();
int main()
{
    foo();
    bar();
    return 0;
}
void foo()
{
	printf("foo");
}
void bar()
{
	printf("bar");
}
