       IDENTIFICATION DIVISION.
       PROGRAM-ID. WMIN2777_GOTO4.
       PROCEDURE DIVISION.
       
       SECTION-A SECTION.
         PARA-A.
           DISPLAY 'IN PARA-A'
           IF FOO EQ 'BAR' THEN
             GO TO PARA-C IN SECTION-A DEPENDING ON CONDITION.
           END-IF.
         PARA-B.
           DISPLAY 'IN PARA-B'.
         PARA-C.
           DISPLAY 'IN PARA-C'
           GO TO PARA-D OF SECTION-B.
           
       SECTION-B SECTION.
         PARA-D.
           DISPLAY 'IN PARA-D'
           GOBACK.
       END PROGRAM WMIN2777_GOTO4.