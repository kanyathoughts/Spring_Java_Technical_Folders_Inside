       IDENTIFICATION DIVISION.
       PROGRAM-ID. A.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  MMRS-COMMAREA.
         10 MMRS-LOGIN                 PIC X(2).
           88 MMRS-LOGIN-OK           VALUE 'OK'.
           88 MMRS-LOGIN-NOK          VALUE '--'.
         10 MMRS-LOGIN2                 PIC X(2).
         10 MMRS-LOGIN3                 PIC X(2).
            
       PROCEDURE DIVISION.
           EVALUATE FALSE
               WHEN TRUE
                   DISPLAY 'true'
           END-EVALUATE.

           EVALUATE MMRS-LOGIN
               WHEN 'A'
                   DISPLAY ' --- eval 1 --- '
           END-EVALUATE.

           EVALUATE MMRS-LOGIN
           				ALSO MMRS-LOGIN2
           					ALSO MMRS-LOGIN3 AND MMRS-LOGIN2
               WHEN 'A'
                   DISPLAY ' --- eval 1 --- '
           END-EVALUATE.

 		   EVALUATE TRUE
               WHEN OTHER
                   DISPLAY 'other'
           END-EVALUATE.

           GOBACK.
       END PROGRAM A.
