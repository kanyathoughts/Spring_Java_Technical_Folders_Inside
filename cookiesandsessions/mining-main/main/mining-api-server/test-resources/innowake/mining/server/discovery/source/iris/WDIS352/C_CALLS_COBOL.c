/*  Sample C file to show 
	C -> COBOL Calls
*/
#include <stdio.h>
#include <libcob.h>

// Linkage to COBOL PROGRAM
#pragma linkage (CBL_PRG_FOR_C, COBOL) 

int CBL_PRG_FOR_C(char *hello, char *world);

int main() {
	COB_RTD = cob_get_rtd();
	int ret;
	int return_status;
	char hello[7] = "Hello ";
	char world[7] = "World!";
	cob_init(rtd, 0, NULL);
	ret = CBL_PRG_FOR_C(hello, world);
	cob_stop_run (rtd, return_status);
	/* Single line comment */
	/*
	* Multi line comment 
	*/
	/* Comment followed by code */ int a = 10;
	int b = 10; /* Comment after code */
	
	/*
	* Multi line comment followed by code
	*/ int c = 20; /*
	* Multi line comment after by code
	*/
	
	/* Comment followed by code */ int d = 10; /* Second
      comment */  int f = 10; /* Comment after code */
	
	return ret;
}
