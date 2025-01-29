       IDENTIFICATION DIVISION.
       PROGRAM-ID. MEE3131N.
      * Taken from MEE3131F; it was ambiguous from the original program
      * whether the intent was to have a GOBACK statement at the bottom
      * or after the first PERFORM statement.
       PROCEDURE DIVISION.
           PERFORM LABEL_1.
           DISPLAY "PERFORM RETURNED!".
           GOBACK.
       LABEL_1.
           DISPLAY "LABEL1".
           PERFORM LABEL_3.
       LABEL_2.
           DISPLAY "LABEL2".
       LABEL_3.
           DISPLAY "LABEL3".
