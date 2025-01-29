****************************************************************************
******** MMRS-M01  MAINFRAME MODERNIZATION REFERENCE SYSTEM
****************************************************************************
******** MMRS7101    USE COPYCODES WITHIN COBOL SOURCE
******** MMRS7101    WRITE INFORMATION WITH DISPLAY TO SPOOLFILE IN JES
******** MMRS7101    TRANSLATE CHARACTER INTO HEX VALUES (EBCDIC)
******** MMRS7101    SET RETURNCODE FOR USAGE IN JCL
******** MMRS7101    SHOW DIFFERENT BEHAVIOR OF TRUNC (BIN) TO (STD)
******** MMRS7101    TEST MEE INSERT TO ADD JAVA INTO COBOL SOURCE
****************************************************************************
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
      *    test truncation of binary fields. Default is TRUNC(STD)
       01  TRUNC-TEST-DISP    PIC -99 VALUE ZERO.
       01  MY-BIN-FIELDS BINARY.
           05  TRUNC-TEST              PIC S9        VALUE ZERO.
      *    ************************************************************
      *    USE ONE OF THE COPYBOOKS MMRS710A OR MMRS710B OR MMRS710C
           COPY MMRS710A.
           05 MY-COPY-NAME PIC X(10) VALUE 'MMRS710A: '.
      *    COPY MMRS710B.
      *    COPY MMRS710C.
      *    ************************************************************
       01  MY-EBCDIC-CHECK-1     PIC X(8) VALUE '1'.
       01  MY-EBCDIC-CHECK-A     PIC X(8) VALUE 'A'.
       01  MY-HEX-TRANS          PIC X(8) VALUE 'MMRS71Z1'.
       01  MY-HEX-ORIGIN-LEN     PIC 9(5).
       01  MY-HEX-ORIGIN         PIC X(100).
       01  MY-HEX-CONV-RESULT    PIC X(200).
       01  MY-ADD-SOURCE     PIC 9(5)V9(2) VALUE 12345.11.
       01  MY-ADD-TARGET     PIC 9(5)V9(2) VALUE ZERO.
      *    ************************************************************
       LINKAGE SECTION.
       PROCEDURE DIVISION