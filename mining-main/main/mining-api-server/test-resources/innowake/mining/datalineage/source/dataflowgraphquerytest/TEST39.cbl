****************************************************************************
******** MMRS-M01  MAINFRAME MODERNIZATION REFERENCE SYSTEM
****************************************************************************
******* MMRS7111  READ WORKFILE AND WRITE SEQ. AND VSAMM FILES
******* MMRS7111  RECORDFORMAT OF FILES ARE
******* MMRS7111  F, FB, V, VB, VSAM-ESDS, VSAM-KSDS, VSAM-RRDS
****************************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    MMRS7111.
      *    ************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MYVXOUT      ASSIGN MYVXOUT
                               FILE STATUS  IS MYVXOUT-STATUS.
           SELECT MYVSAMR      ASSIGN MYVSAMR
							   FILE STATUS  IS MYVSAMR-STATUS.
       DATA DIVISION.
       FILE SECTION.
      *    ************************************************************
      *    DATAFILE FORMAT=V      (VARIABLE UNBLOCKED)
       FD  MYVXOUT                RECORDING MODE V
                                  LABEL RECORDS STANDARD
                                  RECORD VARYING FROM 1 TO 80
                                  DEPENDING ON MYVXOUT-LEN.
       01  MYVXOUT-RECORD PIC X(80).
       WORKING-STORAGE SECTION.
      *    ************************************************************
       01 MYVXOUT-STATUS      PIC X(2).
       01 MYVXOUT-LEN         PIC 9(4) COMP.
      *    ************************************************************
       LINKAGE SECTION.
      *    ************************************************************
       PROCEDURE DIVISION.
      *    --------------------------------------------------
      *    WRITE OUTPUT FILES
       DO-ALL-WRITE-OUTPUT SECTION.
           DISPLAY 'MYVXOUT WRITE=' MYVXOUT-STATUS
                   ' LEN=' MYVXOUT-LEN
           .