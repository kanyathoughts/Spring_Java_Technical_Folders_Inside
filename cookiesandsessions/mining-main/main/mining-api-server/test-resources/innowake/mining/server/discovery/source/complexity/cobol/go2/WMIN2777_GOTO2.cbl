       IDENTIFICATION DIVISION.
       PROGRAM-ID. WMIN2777_GOTO2.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.
           PARA-A.
            DISPLAY 'IN PARA-A'
            GO TO PARA-B
           PARA-B.
            GO TO PARA-B
            DISPLAY 'IN PARA-B'.
           PARA-C.
            DISPLAY 'IN PARA-C'
            IF FOO EQ 'BAR' AND BAR EQ 'FOO'THEN
             DISPLAY 'TRUE'
            END-IF
       END PROGRAM WMIN2777_GOTO2.