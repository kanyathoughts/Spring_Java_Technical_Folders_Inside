       IDENTIFICATION DIVISION.
       PROGRAM-ID.     IF1.
       DATE-COMPILED.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       WORKING-STORAGE SECTION.
       01 A PIC 9(9) VALUES(5).
       01 B PIC 9(9) VALUES(42).
       
       PROCEDURE DIVISION.
       MAIN-PROGRAM SECTION.

           IF A = B THEN
             DISPLAY 'if then'
           ELSE IF A != B
             DISPLAY 'else if'
           END-IF.
       
           GOBACK.