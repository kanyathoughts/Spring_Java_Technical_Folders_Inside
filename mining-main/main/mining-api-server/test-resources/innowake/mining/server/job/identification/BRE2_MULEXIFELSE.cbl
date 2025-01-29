       IDENTIFICATION DIVISION.
       PROGRAM-ID.  EXTINPUT.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TEST-FILE
             ASSIGN TO OUTFILE
             ORGANIZATION IS SEQUENTIAL
             FILE STATUS IS FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  TEST-FILE.
       01  MY-RECORD.
           05 CMF-REST                    PIC X(133).

       WORKING-STORAGE SECTION.
       01 TESTFIELD PIC X(80).
       01 A PIC 9(9) VALUES(5).
       01 B PIC 9(9) VALUES(42).
       01 C PIC 9(9) VALUES(42).
       01  FILE-STATUS                PIC X(02).
       01  WS-EOF                     PIC A(1).

       PROCEDURE DIVISION.
           COMPUTE TESTFIELD = A + B

      * Candidate: Multiple expression If else condition
           IF TESTFIELD = '1' AND A = '2'
              READ TEST-FILE
                 NOT AT END DISPLAY TESTFIELD
              END-READ
           END-IF.

      * Candidate: Multiple expression If else condition
           IF TESTFIELD = '3' OR TESTFIELD = '4'
              EXEC SQL
                   DECLARE C1 CURSOR FOR
                     SELECT TESTFIELD FROM IW_SQL_TEST
              END-EXEC
           END-IF.

           DISPLAY '5' '6' PARENT1.FLAG-ENABLED PARENT3.FLAG-ENABLED.

       END PROGRAM TEST.
