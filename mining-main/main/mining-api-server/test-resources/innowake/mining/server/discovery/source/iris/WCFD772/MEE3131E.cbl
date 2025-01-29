       IDENTIFICATION DIVISION.
       PROGRAM-ID. MEE3131E.
       PROCEDURE DIVISION.
           GO TO LABEL_1.
           DISPLAY "PERFORM RETURNED!".
       LABEL_1.
           DISPLAY "LABEL1".
           PERFORM LABEL_3.
       LABEL_2.
           DISPLAY "LABEL2".
       LABEL_3.
           DISPLAY "LABEL3".
           GOBACK.
