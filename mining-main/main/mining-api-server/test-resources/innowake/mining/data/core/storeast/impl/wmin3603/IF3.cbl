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
             IF A > 2 THEN
               DISPLAY 'nested 1 if then'
               IF B = 3 THEN
                 DISPLAY 'nested 2 if then'
               ELSE IF B = 4 THEN
                 DISPLAY 'nested 3 else if then'
               END-IF.
             END-IF.
           ELSE IF A != B
             DISPLAY 'else if'
           END-IF.
       
           GOBACK.