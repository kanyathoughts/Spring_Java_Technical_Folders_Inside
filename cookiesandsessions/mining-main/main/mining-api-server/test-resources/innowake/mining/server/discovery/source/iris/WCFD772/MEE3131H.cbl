       IDENTIFICATION DIVISION.
       PROGRAM-ID. MEE3131H.
       PROCEDURE DIVISION.
           DISPLAY 'START'.
           GO TO LABEL_1.
           DISPLAY "PERFORM RETURNED!".
       LABEL_1.
           DISPLAY "LABEL1".
           GO TO LABEL_2.
           DISPLAY 'LABEL1 AFTER PERFORM'.
       LABEL_2.
           DISPLAY "LABEL2".
           GO TO LABEL_3.
           DISPLAY 'LABEL2 AFTER GOTO'.
       LABEL_3.
           DISPLAY "LABEL3".
           GOBACK.
