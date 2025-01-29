       IDENTIFICATION DIVISION.
       PROGRAM-ID. WMIN2777L88_2.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  SWITCHES.
           03  GROUP-ONE                      PIC X(01) VALUE 'N'.
               88  VAR1                       VALUE 'N'.
               88  VAR2                       VALUE 'N'.

       PROCEDURE DIVISION.
      *    CHECK PARENT FOR CobolIfStmt
           IF VAR1
               DISPLAY 'NO'
           END-IF

      *    CHECK PARENT FOR CobolAndExpression
           IF VAR1 AND VAR2
               DISPLAY 'NO'
           END-IF

      *    CHECK PARENT FOR CobolOrExpression
           IF VAR1 OR VAR2
               DISPLAY 'NO'
           END-IF

      *    NOTHING SPECIAL NEEDED SINCE IT IS A COMPARISON EXPRESSION
           IF NOT VAR1
               DISPLAY 'NO'
           END-IF

           GOBACK.