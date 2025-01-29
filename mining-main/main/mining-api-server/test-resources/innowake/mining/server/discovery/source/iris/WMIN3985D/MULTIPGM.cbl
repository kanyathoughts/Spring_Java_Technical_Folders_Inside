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
           SELECT MYFBIN      ASSIGN MYFBIN
                               FILE STATUS  IS MYFXOUT-STATUS.
           SELECT MYIO     ASSIGN MYIO
                               FILE STATUS  IS MYFXOUT-STATUS.
                               

       DATA DIVISION.
       FILE SECTION.
       FD  MYFXOUT                RECORDING MODE IS F
                                  LABEL RECORDS ARE OMITTED
                                  RECORD   80 CHARACTERS.
       FD  MYFBIN                 RECORDING MODE IS F
                                  LABEL RECORDS ARE OMITTED
                                  RECORD   80 CHARACTERS
                                  BLOCK CONTAINS 3 RECORDS.
       FD  MYIO                   RECORDING MODE IS F
                                  LABEL RECORDS ARE OMITTED
                                  RECORD   80 CHARACTERS
                                  BLOCK CONTAINS 3 RECORDS.
       WORKING-STORAGE SECTION.
       01  MY-PROGRAM-NAME PIC X(10) VALUE 'MULTIPGM'.
       LINKAGE SECTION.
      *    ************************************************************
       PROCEDURE DIVISION.
       DO-ALL-OPEN-OUTPUT SECTION.
           IF MYSYSIN-COMMAND-ALL OR MYSYSIN-COMMAND-F
             OPEN OUTPUT MYFXOUT
           END-IF
           IF MYSYSIN-COMMAND-ALL OR MYSYSIN-COMMAND-F
             OPEN INPUT MYFBIN
           END-IF
           IF MYSYSIN-COMMAND-ALL OR MYSYSIN-COMMAND-F
             OPEN I-O MYIO
           END-IF
           .
