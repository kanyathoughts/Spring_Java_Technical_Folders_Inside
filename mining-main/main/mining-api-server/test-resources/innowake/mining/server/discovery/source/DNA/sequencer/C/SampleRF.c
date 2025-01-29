#include <stdio.h>

main() {

   FILE *fp;
   char buff[255];

   fp = fopen("test.txt", "w+");
   fscanf(fp, "%s", buff);

   fgets(buff, 255, (FILE*)fp);
   printf("2: %s\n", buff );
   
   fputs("This is testing for fputs...\n", fp);
   
   fclose(fp);
}