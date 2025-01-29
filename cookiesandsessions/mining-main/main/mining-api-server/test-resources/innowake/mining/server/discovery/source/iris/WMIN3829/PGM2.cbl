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
        FROM TPL.IW_SQL_TEST           
021340 END-EXEC.

       EXEC SQL                                                         KESVH000
           SELECT * FROM TABLE1           
021340 END-EXEC

       PROCEDURE DIVISION.
         FIRST-PARA.
           IF FIELD-1 = 'P'
               DISPLAY 'P'.
           IF FIELD-1 = 'R'
               DISPLAY 'R'.
           DISPLAY 'Q'.
           EXIT.
