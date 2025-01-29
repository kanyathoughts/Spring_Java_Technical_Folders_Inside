       IDENTIFICATION DIVISION.
       PROGRAM-ID. MIN510B.
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
               PERFORM LABEL4 THRU LABEL5.
       LABEL4.
               DISPLAY ' ----- E:'.
       LABEL5.
               DISPLAY ' ----- F:'.
       LABEL6.
               DISPLAY ' ----- G:'.
       LABEL7.
           GOBACK.
       END PROGRAM MIN510B.
