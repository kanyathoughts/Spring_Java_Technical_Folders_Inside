       IDENTIFICATION DIVISION.
       PROGRAM-ID. A.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  MMRS-COMMAREA.
         10 MMRS-LOGIN                 PIC X(2).
           88 MMRS-LOGIN-OK           VALUE 'OK'.
           88 MMRS-LOGIN-NOK          VALUE '--'.
            
       PROCEDURE DIVISION.
           EVALUATE FALSE
               WHEN TRUE
                   DISPLAY 'true'
           .
           DISPLAY 'after'
           GOBACK.
       END PROGRAM A.
