****************************************************************************
******** MMRS-M01  mainframe modernization reference system
****************************************************************************
******* MMRS71Z3  add value in working storage section and pass it to caller
**********************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    MMRS71Z3.
      *    ************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.
      *    ************************************************************
       WORKING-STORAGE SECTION.
       01  MY-PROGRAM-NAME PIC X(10) VALUE 'MMRS71Z3:'.
           COPY MMRS710A.
      *    ************************************************************
       01  MY-COUNTER             PIC 9(04) VALUE ZERO.
      *    ************************************************************
       LINKAGE SECTION.
           COPY MMRS710D.
      *    ************************************************************
       PROCEDURE DIVISION USING MMRS-COMMAREA.
           ADD  1          TO MY-COUNTER
           MOVE MY-COUNTER TO MMRS-COUNTER-Z3
           GOBACK
           .
      *    --------------------------------------------------
