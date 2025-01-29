int main()
{
FILE *stream,*fptr4,*fptr5;
if ( (stream = fopen("myfile2.dat", "ab+, lrecl=80, blksize=240, recfm=fb, type=record")) == NULL )                           
     printf("Could not open data file for read update\n");
fptr4 = fopen("var/splool/file.c","r+");
if((fptr5=fopen("home\\lib\\C\\example.c" ,"w+t,lrecl=70, blksize=200"))!=NULL)
    printf("file opened for editing ");
return 0;
}