       IDENTIFICATION DIVISION.
       PROGRAM-ID. MGOCOMP4.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  FIELD-1                   PIC 9(9).

       PROCEDURE DIVISION.
         FIRST-PARA.
           IF FIELD-1 NOT = 'a'
               DISPLAY 'b'
           ELSE
              IF FIELD-1 NOT = 'e' AD 'd' AND 'q'
                 DISPLAY 'z'
           .
           EXIT.
