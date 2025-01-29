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
       FD  MYFBIN                 RECORDING MODE IS F
                                  LABEL RECORDS ARE OMITTED
                                  RECORD   80 CHARACTERS
                                  BLOCK CONTAINS 3 RECORDS.
       FD  MYFBIO                 RECORDING MODE IS F
                                  LABEL RECORDS ARE OMITTED
                                  RECORD   80 CHARACTERS
                                  BLOCK CONTAINS 3 RECORDS.
       FD  MYEXTEND               RECORDING MODE IS F
                                  LABEL RECORDS ARE OMITTED
                                  RECORD   80 CHARACTERS
                                  BLOCK CONTAINS 3 RECORDS.
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
