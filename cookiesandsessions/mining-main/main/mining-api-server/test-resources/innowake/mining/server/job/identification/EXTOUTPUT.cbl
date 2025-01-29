       IDENTIFICATION DIVISION.
       PROGRAM-ID.  EXTOUTPUT.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 TESTFIELD PIC X(80).

       PROCEDURE DIVISION.

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

      * Candidate: body contains a DATABASE_ACCESS_STATEMENT
           IF TESTFIELD = '2'
              EXEC SQL
                 INSERT INTO IW_SQL_TEST (ALPHA_SHORT, I_4)
                 VALUES ('TEST1', 2)
              END-EXEC
           END-IF.

      * Candidate: body contains a DATABASE_ACCESS_STATEMENT
           IF TESTFIELD = '1'
              EXEC SQL
                 DELETE FROM IW_SQL_TEST
              END-EXEC
           END-IF.


       END PROGRAM TEST.