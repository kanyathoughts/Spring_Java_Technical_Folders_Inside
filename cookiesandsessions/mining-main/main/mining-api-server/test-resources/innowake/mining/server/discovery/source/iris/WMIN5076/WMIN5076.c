#include "WMIN5076.h"    
#include <stdio.h>
void foo(int id, char *name)
{
    fprintf(stderr, "foo(%d, \"%s\");\n", id, name);
}