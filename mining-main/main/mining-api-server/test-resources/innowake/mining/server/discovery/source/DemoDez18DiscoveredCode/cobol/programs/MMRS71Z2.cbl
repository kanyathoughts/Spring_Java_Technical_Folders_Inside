****************************************************************************
******** MMRS-M01  mainframe modernization reference system
****************************************************************************
******* MMRS71Z2  get date time and cpu-time
******* MMRS71Z2  call assembler
**********************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    MMRS71Z2.
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
       01  MY-PROGRAM-NAME PIC X(10) VALUE 'MMRS71Z2:'.
           COPY MMRS710A.
      *    ************************************************************
       01  ASSNAME                PIC X(08) VALUE 'MMRS7ZA1'.
       01  ASSPAR1.
           10 PAREYET             PIC X(4) VALUE 'TIM:'.
           10 RC1                 PIC S9(9) BINARY VALUE ZERO.
           10 TIMEDATE1           PIC X(20) VALUE SPACES.
           10 PAREYEC             PIC X(4) VALUE 'CPU:'.
           10 RC2                 PIC S9(9) BINARY VALUE ZERO.
           10 CPUTIME             PIC S9(18) BINARY VALUE ZERO.
      *    ************************************************************
       LINKAGE SECTION.
       01  ASSTIMEDATE.
           10 ASSTIME-HH          PIC X(2).
           10 ASSTIME-MM          PIC X(2).
           10 ASSTIME-SS          PIC X(2).
           10 ASSTIME-REST        PIC X(6).
           10 ASSDATE-YYYY        PIC X(4).
           10 ASSDATE-MM          PIC X(2).
           10 ASSDATE-DD          PIC X(2).
           10 ASSCPU              PIC 9(18).
      *    ************************************************************
       PROCEDURE DIVISION USING ASSTIMEDATE.
           CALL ASSNAME USING ASSPAR1
           MOVE TIMEDATE1 TO ASSTIMEDATE
           MOVE CPUTIME   TO ASSCPU
           .
      *    --------------------------------------------------
