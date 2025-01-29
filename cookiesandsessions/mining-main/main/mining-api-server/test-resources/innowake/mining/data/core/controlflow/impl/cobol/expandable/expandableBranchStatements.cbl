       IDENTIFICATION DIVISION. 
       PROGRAM-ID. A. 
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  MMRS-COMMAREA.
         10 MMRS-LOGIN                 PIC X(2).
           88 MMRS-LOGIN-OK           VALUE 'OK'.
           88 MMRS-LOGIN-NOK          VALUE '--'.
            
       PROCEDURE DIVISION.
           IF MMRS-LOGIN-OK
               DISPLAY 'if'
           ELSE
               DISPLAY 'else'
           END-IF.
           MOVE 2 TO TESTFIELD
           EVALUATE TESTFIELD
               WHEN 1
                   DISPLAY '1'
               WHEN 2
                   DISPLAY '2'
               WHEN 3
               WHEN 4
                   DISPLAY '3 or 4'
               WHEN OTHER
                   DISPLAY 'other'
           .
           DISPLAY 'after'
           
           GOBACK.
       END PROGRAM A.
