       IDENTIFICATION DIVISION.
       PROGRAM-ID. MIN514A.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 TESTFIELD  PIC 9.

       PROCEDURE DIVISION.
           MOVE 2 TO TESTFIELD
           EVALUATE TESTFIELD
           WHEN 1
           WHEN 2
           WHEN 3
           WHEN 4
               DISPLAY ' --- eval 1 --- '
           WHEN 5
           WHEN 6
           WHEN 7
           WHEN 8
               DISPLAY ' --- eval 2 --- '
           END-EVALUATE.
           GOBACK.
