       IDENTIFICATION DIVISION.
       PROGRAM-ID. WMIN2777IF1.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 FIELD1             PIC XX.
         88    VAR1  VALUE('NO').
         88    VAR2  VALUE('NO').
       PROCEDURE DIVISION.
           IF VAR1
               DISPLAY 'NO'
           END-IF

           IF VAR1 AND VAR2
               DISPLAY 'NO'
           END-IF

           IF VAR1 OR VAR2
               DISPLAY 'NO'
           END-IF

           IF NOT VAR1
               DISPLAY 'NO'
           END-IF
