       IDENTIFICATION DIVISION.
       PROGRAM-ID. COBORASM.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  FIELD-1                   PIC 9(9).

       PROCEDURE DIVISION.
         FIRST-PARA.
           IF FIELD-1 = 'P'
               DISPLAY 'P'
           ELSE
               IF FIELD-1 = 'Q'
                   DISPLAY 'Q'
               ELSE
                   DISPLAY 'NP'.
           EXIT.







