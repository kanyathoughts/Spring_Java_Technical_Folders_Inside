       IDENTIFICATION DIVISION.
       PROGRAM-ID. ERROR1.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 TESTFIELD  PIC 9.

       PROCEDURE DIVISION.
                 DISPLAY ' ----- A:'
                 MOVE 1 TO TESTFIELD
                 IF TESTFIELD EQUALS 1
                 ELSE
                   GO TO LABEL404
                 END-IF.
                 DISPLAY ' --- UNREACHABLE --- '
       END PROGRAM ERROR1.