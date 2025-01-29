       IDENTIFICATION DIVISION.
       PROGRAM-ID. MIN510C.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 TESTFIELD  PIC 9.

       PROCEDURE DIVISION.
                 MOVE 1 TO TESTFIELD
                 DISPLAY ' ----- A:'.
                 PERFORM LABEL3 THRU LABEL5.
                 GOBACK.
       LABEL1.
                 DISPLAY ' ----- B:'.
       LABEL2.
                 DISPLAY ' ----- C:'
                 DISPLAY ' --- END --- '.
       LABEL3.
               DISPLAY ' ----- D:'.
               IF 1 EQUAL TESTFIELD
                   PERFORM LABEL5
                ELSE
                   DISPLAY 'ELSE 1'
                END-IF.
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
       END PROGRAM MIN510C.
