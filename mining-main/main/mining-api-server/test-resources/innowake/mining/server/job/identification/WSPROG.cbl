****************************************************************************
******** MMRS-M01  MAINFRAME MODERNIZATION REFERENCE SYSTEM
****************************************************************************
******* MMRS7111  READ WORKFILE AND WRITE SEQ. AND VSAMM FILES
******* MMRS7111  RECORDFORMAT OF FILES ARE
******* MMRS7111  F, FB, V, VB, VSAM-ESDS, VSAM-KSDS, VSAM-RRDS
****************************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    WSPROG.
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
      *    INFO COMING FROM JCL (SYSIN)
      *    ************************************************************
       WORKING-STORAGE SECTION.
       01 WS-NUM1 PIC 9(9) VALUE 10 .
       01 WS-NUM2 PIC 9(9) VALUE 10.
       01 WS-NUM3 PIC 9(9) VALUE 100.
       01 WS-NUM4 PIC 9(9) VALUE 100.
       01 WS-NUMA PIC 9(9) VALUE 10.
       01 WS-NUMB PIC 9(9) VALUE 10.
       01 WS-NUMC PIC 9(9) VALUE 10.
       01 WS-NUMD PIC 9(9) VALUE 100.
       01 WS-NUME PIC 9(9) VALUE 10.
       01 ABCD PIC 9(9) VALUE 10.
       01 AB PIC 9(9) VALUE 10.
       01 ABC PIC 9(9) VALUE 10.
      *    ************************************************************
      *    ************************************************************
       LINKAGE SECTION.
      *    ************************************************************
       PROCEDURE DIVISION.
           COPY WSCOPY.


      *    --------------------------------------------------

