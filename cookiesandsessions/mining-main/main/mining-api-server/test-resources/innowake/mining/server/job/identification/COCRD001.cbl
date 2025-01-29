       IDENTIFICATION DIVISION.
       PROGRAM-ID. COCRD001.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.

       01 WS-DEFAULT-VALUE                     PIC X(01).

       PROCEDURE DIVISION.
      *
      ******************************************************************
      *   MAIN PARA
      ******************************************************************
       1000-MAINLINE-ROUTINE.

       PERFORM 1100-OPEN-ORD-CURSOR     THRU 1100-EXIT.

       1000-EXIT.
           STOP RUN.

       1100-OPEN-ORD-CURSOR.

           EXEC SQL
               OPEN ORDER_CURSOR
           END-EXEC

           EVALUATE SQLCODE
              WHEN 0
                CONTINUE
              WHEN OTHER
                PERFORM 1200-SET-DEFAULT-VALUE
           END-EVALUATE

       1100-EXIT.
           EXIT.

       1200-SET-DEFAULT-VALUE.

           MOVE 'A'       TO  WS-DEFAULT-VALUE.

       1200-EXIT.
           EXIT.

