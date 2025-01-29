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
       DISPLAY-DBD-LIST.                                                        
           MOVE MMRS-M01-GLOBAL-NAME TO MY-PROGRAM-NAME.
           MOVE MMRS-M02-GLOBAL-NAME TO MY-PROGRAM-NAME.
      *    ************************************************************

