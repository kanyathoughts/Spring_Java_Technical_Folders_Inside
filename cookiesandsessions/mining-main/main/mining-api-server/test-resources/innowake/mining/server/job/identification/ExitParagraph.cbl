       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     EE.
       DATA            DIVISION.
       WORKING-STORAGE SECTION.
       01 A PIC 9(9) VALUES 5.
       01 B PIC 9(9) VALUES 42.

       PROCEDURE DIVISION.

           DISPLAY '1'.
           GOBACK.

       DISPLAY-MEMO.
           IF A = B THEN
             DISPLAY 'if 1'
           END-IF.
       DISPLAY-MEMO-EXIT.
           IF A = B THEN
             DISPLAY 'if 2'
           END-IF.
           EXIT.

       END PROGRAM EE.
