       IDENTIFICATION DIVISION.
       PROGRAM-ID.  TEST.
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
       01 TESTFIELD X(80).
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

      * Candidate: body (else) contains a FILE_ACCESS_STATEMENT
           IF TESTFIELD = '3'
              DISPLAY '3'
           ELSE
              WRITE MY-RECORD FROM RECORD-BUFA.

      * Candidate: body (else) contains a FILE_ACCESS_STATEMENT
           IF TESTFIELD = '4'
              DISPLAY '4'
           ELSE
              DELETE TEST-FILE
           END-IF.

      * No candidate: close file is no FILE_ACCESS_STATEMENT
           IF TESTFIELD = '5'
              CLOSE TEST-FILE
           END-IF.

           OPEN INPUT TEST-FILE

           READ TEST-FILE
              AT END MOVE 'Y' TO WS-EOF
              NOT AT END DISPLAY MY-RECORD
           END-READ
           WRITE MY-RECORD FROM RECORD-BUFA.

           DELETE TEST-FILE

           CLOSE TEST-FILE

       END PROGRAM TEST.
