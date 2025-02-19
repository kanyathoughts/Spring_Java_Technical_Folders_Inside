       IDENTIFICATION DIVISION.
       PROGRAM-ID. MGOPRGM2.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MGO-TEST-FILE-3    ASSIGN     MGOFILE3.
           SELECT MGO-TEST-FILE-4    ASSIGN     MGOFILE4.
       FILE SECTION.
       FD  MGO-TEST-FILE-3
             LABEL RECORDS ARE STANDARD
             RECORDING MODE IS F
             BLOCK CONTAINS 0 RECORDS.
       01  EXTRACT-REC-3       PIC X(232).

       FD  MGO-TEST-FILE-4
             LABEL RECORDS ARE STANDARD
             RECORDING MODE IS F
             BLOCK CONTAINS 0 RECORDS.
       01  EXTRACT-REC-4       PIC X(232).

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  REC.

       PROCEDURE DIVISION.
            OPEN INPUT MGO-TEST-FILE-3
                       MGO-TEST-FILE-4
            STOP RUN.
