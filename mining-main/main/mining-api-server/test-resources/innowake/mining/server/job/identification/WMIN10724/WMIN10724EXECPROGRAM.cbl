       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXSQL01.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 HOST-VARS.
          03 ALPHA PIC X(8).
          03 PACKED72 PIC 9(7)V9(2) COMP-3.
       01 DATEF PIC X(10).
       01 BEFOR PIC X(31).

       PROCEDURE DIVISION.
       BEGIN.

           MOVE 'TEST1   ' TO ALPHA.
           MOVE '2017-02-10' TO DATEF.
           MOVE HOST-VARS TO BEFOR.

           EXEC SQL
                    INSERT INTO IW_SQL_TEST
                    (ALPHA_SHORT
                    ,DATUM
                    )
                    VALUES
                    (
                    :ALPHA,
                    TO_DATE(:DATEF, 'YYYY-MM-DD')
                    )
           END-EXEC

       GOBACK.
