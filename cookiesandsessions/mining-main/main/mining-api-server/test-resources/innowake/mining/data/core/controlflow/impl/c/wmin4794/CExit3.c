#include <stdio.h>
#include <stdlib.h>

int main(void)
{
	int exit_code = 10;
	printf("Termination using _Exit");
	_Exit(exit_code);
}
