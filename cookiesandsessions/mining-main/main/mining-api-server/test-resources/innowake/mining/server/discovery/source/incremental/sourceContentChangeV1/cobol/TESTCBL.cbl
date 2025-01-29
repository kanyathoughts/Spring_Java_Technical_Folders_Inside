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
****************************************************************************
******** VSAM AREAS
****************************************************************************
       01  FILLER       PIC X(16) VALUE 'xxxxxxx-VSAMxxxx'.
       01  MYVSAMK-RESP           PIC 9(08) VALUE ZERO.
       01  MYVSAMK-FILENAME       PIC X(08) VALUE 'VSAMK'.
       01  MYVSAMK-RECORD.
           05 MYVSAMK-ALL.
            10 MYVSAMK-LENX       PIC X(04).
            10 MYVSAMK-KEY        PIC X(10).
            10 MYVSAMK-DATA       PIC X(66).
       LINKAGE SECTION.
       PROCEDURE DIVISION.

           DISPLAY 'Test 1'
           DISPLAY 'Modified Source Content'.

           STOP RUN.
