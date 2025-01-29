       IDENTIFICATION DIVISION.
       PROGRAM-ID. MIN510E.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 TESTFIELD  PIC 9.

       PROCEDURE DIVISION.
                 MOVE 1 TO TESTFIELD
                 DISPLAY ' ----- A:'.
                 PERFORM LABEL3 THRU LABEL5.
                 DISPLAY ' --- END --- '.
                 GOBACK.
       LABEL1.
                 DISPLAY ' ----- B:'.
       LABEL2.
                 DISPLAY ' ----- C:'.
       LABEL3.
               DISPLAY ' ----- D:'.
               PERFORM LABEL4 THRU LABEL5 3 TIMES.
       LABEL4.
               DISPLAY ' ----- E:'.
       LABEL5.
               IF 1 EQUAL TESTFIELD
                   DISPLAY 'IF 2'
                ELSE
                   DISPLAY 'ELSE 2'
                END-IF.
       LABEL6.
               DISPLAY ' ----- G:'.
       LABEL7.
           GOBACK.
       END PROGRAM MIN510E.
