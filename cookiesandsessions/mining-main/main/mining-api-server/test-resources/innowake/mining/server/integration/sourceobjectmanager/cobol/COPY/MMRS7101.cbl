       IDENTIFICATION DIVISION.
       PROGRAM-ID.    MMRS7101.
      *    ************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
      *    ************************************************************
       WORKING-STORAGE SECTION.
       01  MY-PROGRAM-NAME PIC X(10) VALUE 'MMRS7101:'.
      *    ************************************************************
       01  TRUNC-TEST-DISP    PIC -99 VALUE ZERO.
       01  MY-BIN-FIELDS BINARY.
           05  TRUNC-TEST              PIC S9        VALUE ZERO.
      *    ************************************************************
           COPY MMRS710A.
      *    ************************************************************
       LINKAGE SECTION.
       PROCEDURE DIVISION.
           .
      *    ************************************************************

