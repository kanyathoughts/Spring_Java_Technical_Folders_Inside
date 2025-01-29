       IDENTIFICATION DIVISION.
       PROGRAM-ID. MEE3131A.
       PROCEDURE DIVISION.
           PERFORM LABEL_1.
           DISPLAY "PERFORM RETURNED!".
       LABEL_1.
           DISPLAY "LABEL1".
           GO TO LABEL_2.
       LABEL_2.
           DISPLAY "LABEL2".
           GOBACK.
