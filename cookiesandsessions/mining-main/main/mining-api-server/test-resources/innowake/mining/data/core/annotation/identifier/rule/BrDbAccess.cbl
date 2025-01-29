       IDENTIFICATION DIVISION.
       PROGRAM-ID.  TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 TESTFIELD X(80).

       PROCEDURE DIVISION.

      * Candidate: body contains a DATABASE_ACCESS_STATEMENT
           IF TESTFIELD = '1'
              EXEC SQL
                 DELETE FROM IW_SQL_TEST
              END-EXEC
           END-IF.

      * Candidate: body contains a DATABASE_ACCESS_STATEMENT
           IF TESTFIELD = '2'
              EXEC SQL
                 INSERT INTO IW_SQL_TEST (ALPHA_SHORT, I_4)
                 VALUES ('TEST1', 2)
              END-EXEC
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

      * Candidate: body contains a DATABASE_ACCESS_STATEMENT
           IF TESTFIELD = '7'
              EXEC SQL
                CLOSE C1
              END-EXEC
           END-IF.

           EXEC SQL
              DELETE FROM IW_SQL_TEST
           END-EXEC.

           EXEC SQL
              INSERT INTO IW_SQL_TEST (ALPHA_SHORT, I_4)
              VALUES ('TEST1', 2)
           END-EXEC.

           EXEC SQL SELECT
              TESTFIELD INTO :TESTFIELD FROM IW_SQL_TEST
           END-EXEC.

           EXEC SQL
                DECLARE C1 CURSOR FOR
                  SELECT TESTFIELD FROM IW_SQL_TEST
           END-EXEC.

           EXEC SQL
            OPEN C1
           END-EXEC.

           EXEC SQL
                FETCH C1 INTO :TESTFIELD
            END-EXEC.

           EXEC SQL
             CLOSE C1
           END-EXEC.

       END PROGRAM TEST.
