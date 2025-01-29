       IDENTIFICATION DIVISION.
       PROGRAM-ID. MIN510H.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 TESTFIELD  PIC 9.

       PROCEDURE DIVISION.
                 DISPLAY ' ----- A:'.

                 PERFORM LABEL3 THRU LABEL4.
                 GOBACK.
       LABEL1.
                 DISPLAY ' ----- B:'.
       LABEL2.
                 DISPLAY ' ----- C:'
                 PERFORM LABEL4 THRU LABEL5.
                 DISPLAY ' --- END --- '.
       LABEL3.
               DISPLAY ' ----- D:'.
              PERFORM LABEL3 THRU LABEL4.
       LABEL4.
               DISPLAY 'LABEL 4'
       LABEL5.
               DISPLAY ' ----- F:'.
       LABEL6.
               DISPLAY ' ----- G:'.
       LABEL7.
           GOBACK.
       END PROGRAM MIN510H.
