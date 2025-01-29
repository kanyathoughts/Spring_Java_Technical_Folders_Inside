       IDENTIFICATION DIVISION.
       PROGRAM-ID. MGOCOMP3.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  FIELD-1                   PIC 9(9).

       PROCEDURE DIVISION.
         FIRST-PARA.
           IF FIELD-1 = 'P'
               DISPLAY 'P'.
           IF FIELD-1 = 'R'
               DISPLAY 'R'.
           DISPLAY 'Q'.
           EXIT.
