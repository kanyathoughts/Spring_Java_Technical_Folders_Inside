****************************************************************************
******** MMRS-M01  MAINFRAME MODERNIZATION REFERENCE SYSTEM
****************************************************************************
******* MMRS7111  READ WORKFILE AND WRITE SEQ. AND VSAMM FILES
******* MMRS7111  RECORDFORMAT OF FILES ARE
******* MMRS7111  F, FB, V, VB, VSAM-ESDS, VSAM-KSDS, VSAM-RRDS
****************************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    MMRS7100.
      *    ************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MYSYSIN      ASSIGN MYSYSIN
                               ORGANIZATION IS SEQUENTIAL
                               FILE STATUS  IS MYSYSIN-STATUS.

       DATA DIVISION.
       FILE SECTION.
      *    ************************************************************
      *    INFO COMING FROM JCL (SYSIN)
       FD  MYSYSIN                RECORDING MODE IS F
                                  LABEL RECORDS ARE OMITTED
                                  RECORD   80 CHARACTERS.
       01  MYSYSIN-RECORD.
           05 MYSYSIN-COMMAND     PIC X(10).
              88 MYSYSIN-COMMAND-F         VALUE 'F         '.
              88 MYSYSIN-COMMAND-FB        VALUE 'FB        '.
              88 MYSYSIN-COMMAND-V         VALUE 'V         '.
           05 MYSYSIN-REST        PIC X(70).
           COPY MMRS710G.
      *    ************************************************************
       WORKING-STORAGE SECTION.
      *    ************************************************************
       LINKAGE SECTION.
      *    ************************************************************
       PROCEDURE DIVISION.
           GOBACK.
