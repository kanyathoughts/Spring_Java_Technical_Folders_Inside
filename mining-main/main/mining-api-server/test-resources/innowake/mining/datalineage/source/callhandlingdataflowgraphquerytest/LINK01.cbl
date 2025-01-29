           IDENTIFICATION DIVISION.
           PROGRAM-ID. LINK01.
           DATA DIVISION.
           WORKING-STORAGE SECTION.

            01 G1.
              05 G1-F1 PIC X.
              05 G1-F2 PIC X.
              05 G1-F3 PIC X.
              05 G1-F4 PIC X.
            01 G2.

           PROCEDURE DIVISION.

             MOVE "X" TO G1-F1.
             CALL "LINK02" USING G1, G1.

           END PROGRAM LINK01.
