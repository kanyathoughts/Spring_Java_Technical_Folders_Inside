       IDENTIFICATION DIVISION.
       PROGRAM-ID.    WMIN8012A.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  MY-PROGRAM-NAME PIC X(10) VALUE 'WMIN8012'.
           COPY MIN8012A.
           05 MY-COPY-NAME PIC X(10) VALUE 'MMRS710A: '.
       LINKAGE SECTION.
       PROCEDURE DIVISION.
      DO-DISPLAY SECTION.
           DISPLAY MY-PROGRAM-NAME  ' Start '.

