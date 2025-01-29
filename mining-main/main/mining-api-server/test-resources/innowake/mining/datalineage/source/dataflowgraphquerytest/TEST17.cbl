       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXSQL01.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 HOST-VARS.
          03 ALPHA PIC X(8).
          03 NUMB50 PIC 9(5).
          03 NUMB03 PIC V9(3).
          03 PACKED72 PIC 9(7)V9(2) COMP-3.
          03 DATEF PIC X(10).
       01 BEFOR PIC X(31).
       01 AFTE PIC X(31).
       
       PROCEDURE DIVISION.
       BEGIN.
           
           MOVE 'TEST1   ' TO ALPHA.
           MOVE 12345 TO NUMB50.
           MOVE 0.321 TO NUMB03.
           MOVE 123.456 TO PACKED72.
           MOVE '2017-02-10' TO DATEF.
           MOVE HOST-VARS TO BEFOR.
           DISPLAY 'BEFORE INSERT ' BEFOR ' ' PACKED72.

           EXEC SQL
                    INSERT INTO IW_SQL_TEST
                    (ALPHA_SHORT
                    ,N_5_0
                    ,N_0_3
                    ,P_7_2
                    ,DATUM
                    )
                    VALUES
                    (
                    :ALPHA,
                    :NUMB50,
                    :NUMB03,
                    :PACKED72,
                    TO_DATE(:DATEF, 'YYYY-MM-DD')
                    )
           END-EXEC
           
       GOBACK.
