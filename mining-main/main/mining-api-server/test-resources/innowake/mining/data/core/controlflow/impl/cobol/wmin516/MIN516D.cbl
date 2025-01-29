       IDENTIFICATION DIVISION.
       PROGRAM-ID. MIN516D.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 TESTFIELD  PIC 9.

       PROCEDURE DIVISION.
                 MOVE 4 TO TESTFIELD
                 DISPLAY ' ----- A:'.
       LABEL1.
                 DISPLAY ' ----- B:'.
       LABEL2.
                 DISPLAY ' ----- C:'
                 GO TO LABEL3 LABEL4 LABEL5 DEPENDING ON TESTFIELD
                 DISPLAY ' --- END --- '
           GOBACK.
       LABEL3.
               DISPLAY ' ----- D:'.
       LABEL4.
               DISPLAY ' ----- E:'.
       LABEL5.
               DISPLAY ' ----- F:'.
           GOBACK.
