       IDENTIFICATION DIVISION.
       PROGRAM-ID.    MAINPGM.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MYFXOUT      ASSIGN MYFXOUT
                               FILE STATUS  IS MYFXOUT-STATUS.
           SELECT MYFBIN       ASSIGN MYFBIN
                               FILE STATUS  IS MYFBOUT-STATUS.
           SELECT MYFBIO       ASSIGN MYFBIO
                               FILE STATUS  IS MYFBOUT-STATUS.
           SELECT MYEXTEND     ASSIGN MYEXTEND
                               FILE STATUS  IS MYFBOUT-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  MYFXOUT                RECORDING MODE IS F
                                  LABEL RECORDS ARE OMITTED
                                  RECORD   80 CHARACTERS.
       01  MYFXOUT-RECORD.
           05 MYFXOUT-ALL         PIC X(80).
       FD  MYFBIN                 RECORDING MODE IS F
                                  LABEL RECORDS ARE OMITTED
                                  RECORD   80 CHARACTERS
                                  BLOCK CONTAINS 3 RECORDS.
       01  MYFBIN-RECORD.
           05 MYFBIN-COMMAND     PIC X(10).
              88 MYFBIN-COMMAND-F         VALUE 'F         '.
              88 MYFBIN-COMMAND-FB        VALUE 'FB        '.
              88 MYFBIN-COMMAND-V         VALUE 'V         '.
              88 MYFBIN-COMMAND-VB        VALUE 'VB        '.
              88 MYFBIN-COMMAND-VSAMK     VALUE 'VSAMK     '.
              88 MYFBIN-COMMAND-VSAME     VALUE 'VSAME     '.
              88 MYFBIN-COMMAND-VSAMR     VALUE 'VSAMR     '.
              88 MYFBIN-COMMAND-ALL       VALUE 'ALL       '.
           05 MYFBIN-REST        PIC X(70).
       FD  MYFBIO                 RECORDING MODE IS F
                                  LABEL RECORDS ARE OMITTED
                                  RECORD   80 CHARACTERS
                                  BLOCK CONTAINS 3 RECORDS.
       01  MYFBIO-RECORD.
           05 MYFBIO-ALL         PIC X(80).
       FD  MYEXTEND               RECORDING MODE IS F
                                  LABEL RECORDS ARE OMITTED
                                  RECORD   80 CHARACTERS
                                  BLOCK CONTAINS 3 RECORDS.
       01  MYEXTEND-RECORD.
           05 MYEXTEND-ALL         PIC X(80).
       WORKING-STORAGE SECTION.
       01  MY-PROGRAM-NAME PIC X(10) VALUE 'MAINPGM'.
       LINKAGE SECTION.
       PROCEDURE DIVISION.
       DO-ALL-OPEN-OUTPUT SECTION.
           IF MYSYSIN-COMMAND-ALL OR MYSYSIN-COMMAND-F
             OPEN OUTPUT MYFXOUT
           END-IF
           IF MYSYSIN-COMMAND-ALL OR MYSYSIN-COMMAND-FB
             OPEN INPUT MYFBIN
           END-IF
           IF MYSYSIN-COMMAND-ALL OR MYSYSIN-COMMAND-FB
             OPEN I-O MYFBIO
           END-IF
           IF MYSYSIN-COMMAND-ALL OR MYSYSIN-COMMAND-FB
             OPEN EXTEND MYEXTEND
           END-IF
           .
