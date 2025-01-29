       IDENTIFICATION DIVISION.
       PROGRAM-ID.     Evaluate1.
       DATE-COMPILED.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       WORKING-STORAGE SECTION.
       01 A PIC 9(9) VALUE 5.
       01 B PIC 9(9) VALUE 42.
       
       PROCEDURE DIVISION.
       MAIN-PROGRAM SECTION.

      * Candidate: two CONSTANT_REFERENCE
           EVALUATE A
             WHEN 5 OR 6
             WHEN 7
               DISPLAY '5'
           END-EVALUATE.
           GOBACK.