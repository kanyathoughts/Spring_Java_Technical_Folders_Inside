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

      * Candidate: body (if) contains a FILE_ACCESS_STATEMENT
           IF TESTFIELD = '2'
              READ TEST-FILE
                 AT END MOVE TESTFIELD TO WS-EOF
                 NOT AT END DISPLAY TESTFIELD
              END-READ
           END-IF.

      * Candidate: body (else) contains a DATABASE_ACCESS_STATEMENT
           IF TESTFIELD = '4'
              DISPLAY '4'
           ELSE
              EXEC SQL
                   DECLARE C1 CURSOR FOR
                     SELECT TESTFIELD FROM IW_SQL_TEST
              END-EXEC
           END-IF.

      * Candidate: body (if) contains a DATABASE_ACCESS_STATEMENT
           IF TESTFIELD = '6'
              EXEC SQL
                   FETCH C1 INTO :TESTFIELD
               END-EXEC
           END-IF.

       END PROGRAM TEST.
