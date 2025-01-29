       IDENTIFICATION DIVISION.
       PROGRAM-ID. MEE3131M.
       PROCEDURE DIVISION.
      * Taken from MEE3131E; it was ambiguous from the original program
      * whether the intent was to have a GOBACK statement at the bottom
      * or after the PERFORM statement.
           GO TO LABEL_1.
           DISPLAY "PERFORM RETURNED!".
       LABEL_1.
           DISPLAY "LABEL1".
           PERFORM LABEL_3.
           GOBACK.
       LABEL_2.
           DISPLAY "LABEL2".
       LABEL_3.
           DISPLAY "LABEL3".
