       IDENTIFICATION DIVISION.
       PROGRAM-ID. MIN520A.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 TESTFIELD  PIC 9.

       PROCEDURE DIVISION.
                 MOVE 1 TO TESTFIELD.
               DISPLAY ' ----- F:'.
       LABEL7.
              DISPLAY ' ----- F:'.
       END PROGRAM MIN520A.
