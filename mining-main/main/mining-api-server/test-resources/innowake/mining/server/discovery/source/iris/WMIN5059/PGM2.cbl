       IDENTIFICATION DIVISION.
       PROGRAM-ID. MGOCOMP3.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  FIELD-1                   PIC 9(9).
       EXEC SQL
         INCLUDE SQLCA
       END-EXEC.

       EXEC SQL                                                         KESVH000
           SELECT
        ALPHA_SHORT
        INTO 
        $ALPHA-SHORT 
        FROM DSN8C10.EMP           
021340 END-EXEC.

       EXEC SQL                                                         KESVH000
           SELECT * FROM VLMQPA           
021340 END-EXEC
       EXEC SQL                                                         KESVH000
           CALL CONSTANTPROC.TEST (:VAR1, :VAR2, :VAR3)           
021340 END-EXEC

       PROCEDURE DIVISION.
         FIRST-PARA.
           IF FIELD-1 = 'P'
               DISPLAY 'P'.
           IF FIELD-1 = 'R'
               DISPLAY 'R'.
           DISPLAY 'Q'.
           EXIT.
