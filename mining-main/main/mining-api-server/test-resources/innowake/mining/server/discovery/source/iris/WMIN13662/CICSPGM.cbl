       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CICSPGM.cbl.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MYEXTEND     ASSIGN '     '
                               FILE STATUS  IS MYFBOUT-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  MYEXTEND               RECORDING MODE IS F
                                  LABEL RECORDS ARE OMITTED
                                  RECORD   80 CHARACTERS
                                  BLOCK CONTAINS 3 RECORDS.
       WORKING-STORAGE SECTION.
       LINKAGE SECTION.
       PROCEDURE DIVISION.
