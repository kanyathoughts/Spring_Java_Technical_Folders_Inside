       IDENTIFICATION DIVISION.
       PROGRAM-ID. WMIN2777_MISC3.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  SWITCHES.
           03  GROUP-ONE                      PIC X(01) VALUE 'N'.
               88  VAR1                       VALUE 'N'.
               88  VAR2                       VALUE 'Y'.
               88  VAR3                       VALUE 'N'.

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
           
           IF VAR1 AND VAR2 AND NOT VAR3
               DISPLAY 'NO'
           END-IF
           
           IF VAR1 AND VAR2 AND NOT VAR3 AND FOO EQ 'BAR'
               DISPLAY 'NO'
           END-IF.

           SEC1 SECTION
           
           GOBACK.