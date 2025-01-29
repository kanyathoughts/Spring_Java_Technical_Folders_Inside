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

      * No candidate: open file is no FILE_ACCESS_STATEMENT
           IF TESTFIELD = '1'
              OPEN INPUT TEST-FILE
           END-IF.

      * Candidate: body (if) contains a FILE_ACCESS_STATEMENT
           IF TESTFIELD = '2'
              READ TEST-FILE
                 AT END MOVE 'Y' TO WS-EOF
                 NOT AT END DISPLAY MY-RECORD
              END-READ
           END-IF.
      * Candidate: body contains (else) a DATABASE_ACCESS_STATEMENT
           IF TESTFIELD = '3'
              DISPLAY '3'
           ELSE
              EXEC SQL SELECT
                 TESTFIELD INTO :TESTFIELD FROM IW_SQL_TEST
              END-EXEC.

      * Candidate: body (else) contains a DATABASE_ACCESS_STATEMENT
           IF TESTFIELD = '4'
              DISPLAY '4'
           ELSE
              EXEC SQL
                   DECLARE C1 CURSOR FOR
                     SELECT TESTFIELD FROM IW_SQL_TEST
              END-EXEC
           END-IF.

      * No candidate: Open cursor is no DATABASE_ACCESS_STATEMENT
           IF TESTFIELD = '5'
              EXEC SQL
               OPEN C1
              END-EXEC
           END-IF.

      * Candidate: body (if) contains a DATABASE_ACCESS_STATEMENT
           IF TESTFIELD = '6'
              EXEC SQL
                   FETCH C1 INTO :TESTFIELD
               END-EXEC
           END-IF.

       END PROGRAM TEST.
