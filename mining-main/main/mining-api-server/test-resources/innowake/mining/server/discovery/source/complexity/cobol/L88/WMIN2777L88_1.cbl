       IDENTIFICATION DIVISION.
       PROGRAM-ID. WMIN2777L88_1.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  SWITCHES.
           03  GROUP-ONE                      PIC X(01) VALUE 'N'.
               88  NO-SUCCESS                  VALUE 'N'.

       PROCEDURE DIVISION.
           IF NO-SUCCESS DISPLAY 'NO-SUCCESS'.
           GOBACK.