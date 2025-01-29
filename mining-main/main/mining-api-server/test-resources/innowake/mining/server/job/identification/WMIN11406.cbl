       IDENTIFICATION DIVISION.
       PROGRAM-ID. AND3CLEE.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

        01 A PIC X.
        01 B PIC X.

        01 G1.
           05 G1-F1 PIC X.

        01 G2.
           05 G2-F1 PIC X.
           05 G2-F2 PIC X.
           
        01 G2-REDEFINED REDEFINES G1.
           05 G1-REDEFINED-F1 PIC X.
           05 G1-REDEFINED-F2 PIC X.

       PROCEDURE DIVISION USING G1 G2.

           MOVE "X" TO G1-F1.
           MOVE G1-F1 TO G2-F1.
           DISPLAY A.

       END PROGRAM AND3CLEE.
