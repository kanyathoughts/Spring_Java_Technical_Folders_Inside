       IDENTIFICATION DIVISION.
       PROGRAM-ID. SQLALIASES.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 PROC-NAME PIC X(10).
            EXEC SQL
              DECLARE ASD CURSOR FOR WITH ALIASTABLE1 AS
              (SELECT A, B, C FROM T1),
              ALIASTABLE2 AS
              (SELECT A, B, C FROM T1),
              ALIASTABLE3 AS
              (SELECT A, B, C FROM T1
              LEFT OUTER JOIN ALIASTABLE2)
              SELECT A, B, C FROM ALIASTABLE1 ,
              ALIASTABLE3
            END-EXEC
            EXEC SQL
            SELECT A, B, C FROM T2;
            END-EXEC
       PROCEDURE DIVISION.
       BEGIN.
           MOVE 'ALIASTABLE3' TO PROC-NAME

           EXEC SQL
           CALL :PROC-NAME (:ALPHA-SHORT, 'HELLO OO' -1, 5, 'HELLO OO')
           END-EXEC
           GOBACK.
       END PROGRAM SQLALIASES.

