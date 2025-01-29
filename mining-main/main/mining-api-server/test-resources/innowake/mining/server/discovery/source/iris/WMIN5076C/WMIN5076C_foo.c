#include "WMIN5076C_foo.h"    
#include <stdio.h>
void foo(int id, char *name)
{
    fprintf(stderr, "foo(%d, \"%s\");\n", id, name);
}
