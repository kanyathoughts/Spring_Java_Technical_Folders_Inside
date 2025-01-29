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

      * Comment 1
           IF A = B THEN
      * Comment 2
             DISPLAY 'if then'
      * Comment 3
           ELSE IF A != B
             DISPLAY 'else if'
           END-IF.
       
           GOBACK.