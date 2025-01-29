#include "WMIN5076A.h" 
#include <stdio.h>
void foo(int id, char *name)
{
    fprintf(stderr, "foo(%d, \"%s\");\n", id, name);
}
void abc(int id, char *name)
{
    fprintf(stderr, "foo(%d, \"%s\");\n", id, name);
    
}
void xyz(int id, char *name)
{
    fprintf(stderr, "foo(%d, \"%s\");\n", id, name);
}