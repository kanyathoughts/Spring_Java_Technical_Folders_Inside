       IDENTIFICATION DIVISION.
       PROGRAM-ID. WMIN2777_Perform3.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  SWITCHES.
           03  GROUP-ONE                      PIC X(01) VALUE 'N'.
               88  VAR1                       VALUE 'N'.
               88  VAR2                       VALUE 'N'.

       PROCEDURE DIVISION.
      *    CHECK PARENT FOR CobolIfStmt
         PERFORM PROCEDURE 
           UNTIL VAR1
               DISPLAY 'NO'

      *    CHECK PARENT FOR CobolAndExpression
         PERFORM PROCEDURE 
           UNTIL VAR1 AND VAR2
               DISPLAY 'NO'

      *    CHECK PARENT FOR CobolOrExpression
         PERFORM PROCEDURE 
           UNTIL VAR1 OR VAR2
               DISPLAY 'NO'

      *    NOTHING SPECIAL NEEDED SINCE IT IS A COMPARISON EXPRESSION
         PERFORM PROCEDURE 
           UNTIL NOT VAR1
               DISPLAY 'NO'
       GOBACK.