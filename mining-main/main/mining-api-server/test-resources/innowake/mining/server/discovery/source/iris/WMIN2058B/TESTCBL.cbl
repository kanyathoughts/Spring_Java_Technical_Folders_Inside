       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TESTCBL.
      ******************************************************************
      *              ENVIRONMENT DIVISION.                             *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       INPUT-OUTPUT  SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       LINKAGE SECTION.
       PROCEDURE DIVISION.

           CALL 'ABEND'.

           STOP RUN.
