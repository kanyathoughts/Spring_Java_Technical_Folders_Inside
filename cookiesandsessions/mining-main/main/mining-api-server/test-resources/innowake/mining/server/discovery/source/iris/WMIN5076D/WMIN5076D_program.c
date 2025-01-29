#include "WMIN5076D_foo.h"
#include "WMIN5076D_abc.h"
#include "WMIN5076D_xyz.h"
    
#include <stdio.h>
void foo(int id, char *name)
{
    fprintf(stderr, "foo(%d, \"%s\");\n", id, name);
}
void abc(int id, char *name)
{
    fprintf(stderr, "abc(%d, \"%s\");\n", id, name);
}
void xyz(int id, char *name)
{
    fprintf(stderr, "xyz(%d, \"%s\");\n", id, name);
}
