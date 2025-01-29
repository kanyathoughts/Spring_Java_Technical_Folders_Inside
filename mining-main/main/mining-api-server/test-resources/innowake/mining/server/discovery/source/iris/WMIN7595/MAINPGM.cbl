       IDENTIFICATION DIVISION.
       PROGRAM-ID.    MAINPGM.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MYFXOUT_FD   ASSIGN MYFXOUT
           					   ORGANIZATION IS INDEXED
           					   ACCESS MODE  IS DYNAMIC
                               FILE STATUS  IS MYFXOUT_FD-STATUS.
           SELECT MYFBIN_FD    ASSIGN MYFBIN
           					   ORGANIZATION IS SEQUENTIAL
           					   ACCESS MODE  IS RANDOM
                               FILE STATUS  IS MYFBOUT-STATUS.
           SELECT MYFBIO_FD    ASSIGN LABEL-MYFBIO
           					   ORGANIZATION IS RELATIVE
           					   ACCESS MODE  IS DYNAMIC
                               FILE STATUS  IS MYFBOUT-STATUS.
           SELECT MYEXTEND_FD  ASSIGN SEQUENTIAL-AS-MYEXTEND
                               FILE STATUS  IS MYFBOUT-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  MYFXOUT_FD                RECORDING MODE IS F
                                  LABEL RECORDS ARE OMITTED
                                  RECORD   80 CHARACTERS.
       FD  MYFBIN_FD                 RECORDING MODE IS F
                                  LABEL RECORDS ARE OMITTED
                                  RECORD   80 CHARACTERS
                                  BLOCK CONTAINS 3 RECORDS.
       FD  MYFBIO_FD                 RECORDING MODE IS F
                                  LABEL RECORDS ARE OMITTED
                                  RECORD   80 CHARACTERS
                                  BLOCK CONTAINS 3 RECORDS.
       FD  MYEXTEND_FD               RECORDING MODE IS F
                                  LABEL RECORDS ARE OMITTED
                                  RECORD   80 CHARACTERS
                                  BLOCK CONTAINS 3 RECORDS.
       WORKING-STORAGE SECTION.
       01  MY-PROGRAM-NAME PIC X(10) VALUE 'MAINPGM'.
       LINKAGE SECTION.
       PROCEDURE DIVISION.
       DO-ALL-OPEN-OUTPUT SECTION.
           IF MYSYSIN-COMMAND-ALL OR MYSYSIN-COMMAND-F
             OPEN OUTPUT MYFXOUT_FD
           END-IF
           IF MYSYSIN-COMMAND-ALL OR MYSYSIN-COMMAND-FB
             OPEN INPUT MYFBIN_FD
           END-IF
           IF MYSYSIN-COMMAND-ALL OR MYSYSIN-COMMAND-FB
             OPEN I-O MYFBIO_FD
           END-IF
           IF MYSYSIN-COMMAND-ALL OR MYSYSIN-COMMAND-FB
             OPEN EXTEND MYEXTEND_FD
           END-IF
           .
