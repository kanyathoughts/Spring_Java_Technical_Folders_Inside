       IDENTIFICATION DIVISION.
       PROGRAM-ID.         IBA600P.
       AUTHOR.             SMI!!!
       DATE-WRITTEN.       JULY, 2007.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  FILLER.

           05  FILLER                    PIC X(01) VALUE 'Y'.
               88  FIRST-TIME-THRU       VALUE 'Y'.
               88  NOT-FIRST-TIME-THRU   VALUE 'N'.

       LINKAGE SECTION.
       PROCEDURE DIVISION.

      ***************** END OF IBA600P *********************************