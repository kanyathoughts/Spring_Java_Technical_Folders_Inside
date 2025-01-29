       IDENTIFICATION DIVISION.
       PROGRAM-ID. MIN510G.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 TESTFIELD  PIC 9.

       PROCEDURE DIVISION.
                 MOVE 1 TO TESTFIELD.
                 PERFORM LABEL2 THRU LABEL4
                 DISPLAY ' --- END --- '
           GOBACK.
       LABEL2.
                 DISPLAY ' ----- C:'
                 PERFORM LABEL3 THRU LABEL4.
       LABEL3.
               DISPLAY ' ----- D:'.
       LABEL4.
               IF 1 EQUAL TESTFIELD
                    DISPLAY ' -- IF -- '
               ELSE
                    DISPLAY ' -- ELSE -- '
               END-IF.
       LABEL5.
               DISPLAY ' ----- F:'.
       LABEL7.
           GOBACK.
       END PROGRAM MIN510G.
