       IDENTIFICATION DIVISION.
       PROGRAM-ID.    multiStep.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MYFXOUT      ASSIGN MYFXOUT
                               FILE STATUS  IS MYFXOUT-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  MYFXOUT                RECORDING MODE IS F
                                  LABEL RECORDS ARE OMITTED
                                  RECORD   80 CHARACTERS.
       WORKING-STORAGE SECTION.
       01  MY-PROGRAM-NAME PIC X(10) VALUE 'UNREF'.
       LINKAGE SECTION.
      *    ************************************************************
       PROCEDURE DIVISION.
       DO-ALL-OPEN-OUTPUT SECTION.
           IF MYSYSIN-COMMAND-ALL OR MYSYSIN-COMMAND-F
             OPEN OUTPUT MYFXOUT
           END-IF
           .
           EXEC CICS WRITE
                FILE('fileWrite')
                INTO(WS-VSAM-TABLE)
                RIDFLD(WS-VSAM-TABLE-KEY)
           END-EXEC
