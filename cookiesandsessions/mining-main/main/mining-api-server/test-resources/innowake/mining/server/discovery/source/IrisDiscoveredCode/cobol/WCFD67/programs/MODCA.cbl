       IDENTIFICATION DIVISION.
       PROGRAM-ID.
           MODCA.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  WS-ERGEBNIS                          PIC  9(0008).
       01  WS-WOCHENTAG                         PIC  X(0010).
   
       PROCEDURE DIVISION.

           DISPLAY "Before Copy"

           COPY MODCB.
           
           DISPLAY "After Copy"

           EXIT.
