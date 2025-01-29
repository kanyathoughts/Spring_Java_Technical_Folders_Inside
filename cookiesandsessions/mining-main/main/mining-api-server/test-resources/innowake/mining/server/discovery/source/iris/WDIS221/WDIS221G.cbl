       IDENTIFICATION DIVISION.
       PROGRAM-ID. WDIS221G.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 PROC-NAME PIC X(10).
       PROCEDURE DIVISION.
       BEGIN.
       
           IF TRUE THEN
             MOVE 'HELLO' TO PROC-NAME
           ELSE
             MOVE 'WORLD' TO PROC-NAME
           END-IF

           EXEC SQL
           CALL :PROC-NAME (:ALPHA-SHORT, 'HELLO OO' -1, 5, 'HELLO OO')
           END-EXEC

           GOBACK.
       END PROGRAM WDIS221G.
