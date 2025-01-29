       IDENTIFICATION DIVISION.
       PROGRAM-ID. MIN510A.
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
                 PERFORM LABEL5 THRU LABEL6.
                 DISPLAY ' --- END --- '.
       LABEL3.
               DISPLAY ' ----- D:'.
              PERFORM LABEL2 THRU LABEL2.
       LABEL4.
               DISPLAY 'LABEL 4'
       LABEL5.
               GO TO LABEL6
               DISPLAY ' ----- F:'.
       LABEL6.
               DISPLAY ' ----- G:'.
       LABEL7.
           GOBACK.
       END PROGRAM MIN510A.
