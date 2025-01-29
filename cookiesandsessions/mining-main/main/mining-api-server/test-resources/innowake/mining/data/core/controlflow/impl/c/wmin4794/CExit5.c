#include <stdio.h>
#include <stdlib.h>

int main(void)
{
	int exit_code = 10;
	printf("Termination using _Exit");
	if(a > b) {
	  if(c > d) {
	    _Exit(exit_code);
	    }
	  else {
	    exit(exit_code);
	  }
	} else {
	  printf("Did not exit");
	}
	if(b > d) {
	  abort();
	}
	printf("Did not abort");
}
